module State
    exposing
        ( createDailySummaries
          -- , flags
        , getMissingTime
        , groupSumActivities
        , init
        , stringifyDuration
        , subscriptions
        , toIsoDate
        , update
        )

import Optimism
    exposing
        ( getCurrent
        , getOptimistic
        , Optimistic
            ( Failed
            , Succeeded
            , Unchanged
            , Waiting
            )
        )
import Timers
    exposing
        ( Against
        , groupSum
        , isRunning
        , Timer
        , TimerId
        , Timers
        , TimerStatus
            ( Deleted
            , Running
            , RunningRelative
            , Stopped
            )
        , upsert
        )
import Ports
    exposing
        ( checkNotificationsState
        , getNotificationClicks
        , getNotificationsState
        , requestNotificationPermission
        , sendNotification
        , sendActionableNotification
        )
import Types
    exposing
        ( AcceloResponse
        , AcceloResponseMeta
        , Activity
        , ActionableNotificationObject
        , DailyItineraries
        , DailySummaries
        , DaySummary
        , DomNotification
        , Duration
        , DurationComponentTemplate
        , EmptyAcceloResponse
        , Event
        , Identified
        , Itinerary
            ( Nonworkday
            , Workday
            )
        , ListOfActivities
        , Model
        , Msg
            ( CreateTimer
            , DeleteTimer
            , DeleteTimerForever
            , CmdMisbehaviour
            , FetchTimers
            , FetchTodaysActivities
            , FetchUserInfoThen
            , NotificationClicked
            , NotificationsStatusUpdate
            , ParseTimerCreationResponse
            , ParseTimerDeletionResponse
            , ParseTimersResponse
            , ParseTimerSubjectUpdateResponse
            , ParseTodaysActivitiesResponse
            , ParseUserInfoResponseThen
            , RequestNotificationPermission
            , Reset
            , ResumeTimer
            , SetTimerDate
            , SendTestNotification
            , StopTimer
            , Tick
            , ToggleTimerDateSelector
            , UndoDeleteTimer
            , UpdateLoggedTime
            , UpdateTimeInTimer
            )
        , Notification
            ( RegularNotification
            , ActionableNotification
            )
        , NotificationsState
            ( Denied
            , Granted
            , NotAsked
            , NotSupported
            , Unknown
            , Unrecognized
            )
        , NotificationTag
            ( DeficitGrowth
            , Test
            , UnrecognizedTag
            )
        , RegularNotificationObject
        , TimesOverDays
        , UserInfo
        , WorkdayItinerary
        )
import Time exposing (Time, second, every)
import Date exposing (Date)
import Date.Format
import Date.Extra
import Dict
import Http
import BasicUrl
import Json.Decode exposing (Decoder, andThen, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, custom)
import Process exposing (sleep)
import RemoteData
import Set
import Task


itemsPerPage : Int
itemsPerPage =
    10


activitiesRefetchDelay : Time
activitiesRefetchDelay =
    (60 * Time.second)


timersRefetchDelay : Time
timersRefetchDelay =
    (20 * Time.second)


notificationIconUrl : String
notificationIconUrl =
    "https://timers-helper-dev.au.ngrok.io/android-chrome-192x192.png"


timerFields : String
timerFields =
    "id,status,against_type,against_id,against_title,staff"


init : Types.Flags -> ( Model, Cmd Msg )
init flags =
    ( createEmptyModel flags.theTime
    , Cmd.batch
        [ Task.perform FetchTimers (Task.succeed flags.theTime)
        , fetchUserInfoThen <| FetchTodaysActivities
          -- don't have the time yet...
        , checkNotificationsState ()
        ]
    )



-- flags :


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CmdMisbehaviour messages ->
            ( { model | cmdMisbehaviour = model.cmdMisbehaviour ++ [ toString model.timeNow ] ++ messages }
            , Cmd.none
            )

        CreateTimer timers subject duration autoStart idToReplace theTime ->
            let
                ( newTimers, cmdMsg ) =
                    createTimer
                        timers
                        { now = theTime
                        , subject = subject
                        , duration = duration
                        , autoStart = autoStart
                        , idToReplace = idToReplace
                        }
            in
                ( { model | timers = newTimers }
                , cmdMsg
                )

        DeleteTimer id ->
            ( { model | timers = Timers.delete model.timeNow model.timers id }
            , deleteTimer id
            )

        DeleteTimerForever id ->
            ( { model | timers = Timers.deleteForever model.timers id }
            , Cmd.none
            )

        FetchTodaysActivities ->
            fetchTodaysActivities { model | activitiesRaw = RemoteData.Loading }

        FetchUserInfoThen nextMsg ->
            ( { model | userInfo = RemoteData.Loading }
            , fetchUserInfoThen nextMsg
            )

        FetchTimers now ->
            ( { model
                | timersRaw = RemoteData.Loading
                , timersLastRequested = Just now
              }
            , fetchTimers now
            )

        NotificationClicked event Nothing ->
            ( model
            , Task.perform (NotificationClicked event) <| Task.map Just Time.now
            )

        NotificationClicked event (Just now) ->
            let
                ( newTimers, action ) =
                    case parseNotificationTag event.target.tag of
                        DeficitGrowth ->
                            createTimer
                                model.timers
                                { now = now
                                , subject = ("quick timer " ++ Timers.timeDescription now)
                                , duration = (calcTodaysDeficit model.dailySummaries now |> Maybe.withDefault 0)
                                , autoStart = True
                                , idToReplace = Just <| "createdFromNotification_" ++ toString now
                                }

                        Test ->
                            createTimer
                                model.timers
                                { now = now
                                , subject = ("quick timer " ++ Timers.timeDescription now)
                                , duration = 0
                                , autoStart = True
                                , idToReplace = Just <| "createdFromTest_" ++ toString now
                                }

                        UnrecognizedTag s ->
                            {- should anything happen here? -}
                            ( model.timers, Cmd.none )
            in
                ( model
                , action
                )

        NotificationsStatusUpdate status ->
            ( { model | notificationsState = parseNotificationState status }
            , Cmd.none
            )

        ParseTimersResponse timersResponse Nothing ->
            ( model
            , Task.perform (ParseTimersResponse timersResponse) <| Task.map Just Time.now
            )

        ParseTimersResponse timersResponse (Just theTime) ->
            ( updateWithReceivedTimers model theTime timersResponse
            , Cmd.none
            )

        ParseTimerSubjectUpdateResponse id timerResponse Nothing ->
            ( model
            , Task.perform (ParseTimerSubjectUpdateResponse id timerResponse) <| Task.map Just Time.now
            )

        ParseTimerSubjectUpdateResponse id timerResponse (Just theTime) ->
            case timerResponse of
                RemoteData.Success timer ->
                    ( updateWithReceivedTimer model theTime timer Nothing
                    , Cmd.none
                    )

                RemoteData.Failure err ->
                    -- FIXME: handle failures
                    ( logMisbehaviour
                        { model | timers = Timers.updateById model.timers id <| Timers.setSubjectFailed }
                        [ "ParseTimerSubjectUpdateResponse :: RemoteDate.Failure"
                        , toString err
                        ]
                    , Cmd.none
                    )

                RemoteData.NotAsked ->
                    -- FIXME: handle failures
                    ( logMisbehaviour
                        model
                        [ "ParseTimerSubjectUpdateResponse :: RemoteDate.NotAsked"
                        ]
                    , Cmd.none
                    )

                RemoteData.Loading ->
                    -- FIXME: handle failures
                    ( logMisbehaviour
                        model
                        [ "ParseTimerSubjectUpdateResponse :: RemoteDate.Loading"
                        ]
                    , Cmd.none
                    )

        ParseTimerCreationResponse idToReplace timerResponse Nothing ->
            ( model
            , Task.perform (ParseTimerCreationResponse idToReplace timerResponse) <| Task.map Just Time.now
            )

        ParseTimerCreationResponse idToReplace timerResponse (Just theTime) ->
            case timerResponse of
                RemoteData.Success timer ->
                    ( updateWithReceivedTimer model theTime timer idToReplace
                    , Cmd.none
                    )

                RemoteData.Failure err ->
                    -- FIXME: handle failures
                    ( logMisbehaviour
                        model
                        [ "updateWithReceivedTimer :: RemoteDate.Failure"
                        , "idToReplace: " ++ (toString idToReplace)
                        , toString err
                        ]
                    , Cmd.none
                    )

                RemoteData.NotAsked ->
                    -- FIXME: handle failures
                    ( logMisbehaviour
                        model
                        [ "updateWithReceivedTimer :: RemoteDate.NotAsked"
                        , "idToReplace: " ++ (toString idToReplace)
                        ]
                    , Cmd.none
                    )

                RemoteData.Loading ->
                    -- FIXME: handle failures
                    ( logMisbehaviour
                        model
                        [ "updateWithReceivedTimer :: RemoteDate.Loading"
                        , "idToReplace: " ++ (toString idToReplace)
                        ]
                    , Cmd.none
                    )

        ParseTimerDeletionResponse deletedId response ->
            case response of
                RemoteData.Success _ ->
                    ( { model | timers = Timers.updateById model.timers deletedId Timers.setStatusSucceeded }
                    , Cmd.none
                    )

                RemoteData.Loading ->
                    ( model
                    , Cmd.none
                    )

                RemoteData.NotAsked ->
                    ( model
                    , Cmd.none
                    )

                RemoteData.Failure err ->
                    {- fixme: what do we do now? unmark the timer as deleted? -}
                    ( { model | timers = Timers.updateById model.timers deletedId Timers.setStatusFailed }
                    , Cmd.none
                    )

        ParseUserInfoResponseThen nextMsg userInfo ->
            ( { model | userInfo = userInfo }
            , Task.perform (always nextMsg) (Task.succeed Nothing)
            )

        ParseTodaysActivitiesResponse response ->
            ( updateWithReceivedActivities model response
            , delayThen activitiesRefetchDelay FetchTodaysActivities
            )

        SetTimerDate timerId someDate Nothing ->
            ( model
            , Task.perform (SetTimerDate timerId someDate) <| Task.map Just Time.now
            )

        SetTimerDate timerId someDate (Just now) ->
            let
                ( newTimers, cmdMsg ) =
                    setTimerDate model.timers timerId someDate now
            in
                ( { model | timers = Timers.toggleDateSelector newTimers timerId <| Just False }
                , cmdMsg
                )

        -- FIXME: strip the Maybe from this
        StopTimer id Nothing ->
            ( model
            , Task.perform (StopTimer id) <| Task.map Just Time.now
            )

        StopTimer id (Just now) ->
            ( { model | timers = Timers.stop model.timers id }
            , stopTimer now id
            )

        ResumeTimer id Nothing ->
            ( model
            , Task.perform (ResumeTimer id) <| Task.map Just Time.now
            )

        ResumeTimer id (Just now) ->
            ( { model | timers = Timers.resume now model.timers id }
            , resumeTimer now id
            )

        RequestNotificationPermission ->
            ( model
            , requestNotificationPermission ()
            )

        Reset ->
            ( createEmptyModel model.timeNow
            , Cmd.none
            )

        SendTestNotification message ->
            ( model
            , sendNotification
                { title = "Timers Notification Test"
                , body = Just <| message ++ " \nLucky number: " ++ toString model.timeNow
                , tag = toString Test
                , icon = notificationIconUrl
                , timestamp = model.timeNow
                , silent = False
                , renotify = True
                }
            )

        Tick time ->
            let
                dailySummaries =
                    getMissingTime model

                ( newLastAlertTime, notificationCmd ) =
                    alertDeficit time model.lastAlertTime model.notificationsState model.timers model.activitiesRaw dailySummaries

                recheckNotificationStatus =
                    case model.notificationsState of
                        Unknown ->
                            checkNotificationsState ()

                        Denied ->
                            Cmd.none

                        Granted ->
                            Cmd.none

                        NotAsked ->
                            Cmd.none

                        NotSupported ->
                            Cmd.none

                        Unrecognized string ->
                            Cmd.none

                ( timersLastRequested, refetchTimers ) =
                    case (model.timersLastRequested |> moreThan timersRefetchDelay time) of
                        False ->
                            ( model.timersLastRequested, Cmd.none )

                        True ->
                            case model.timersRaw of
                                RemoteData.Loading ->
                                    ( model.timersLastRequested, Cmd.none )

                                RemoteData.Success _ ->
                                    ( Just time, Task.perform (always <| FetchTimers time) (Task.succeed Nothing) )

                                RemoteData.NotAsked ->
                                    ( Just time, Task.perform (always <| FetchTimers time) (Task.succeed Nothing) )

                                RemoteData.Failure _ ->
                                    ( Just time, Task.perform (always <| FetchTimers time) (Task.succeed Nothing) )
            in
                ( { model
                    | timeNow = time
                    , timers = tickTimers time model.timers
                    , dailySummaries = dailySummaries
                    , lastAlertTime = newLastAlertTime
                    , timersLastRequested = timersLastRequested
                  }
                , Cmd.batch
                    [ notificationCmd
                    , recheckNotificationStatus
                    , refetchTimers
                    ]
                )

        ToggleTimerDateSelector timerId newState ->
            ( { model | timers = Timers.toggleDateSelector model.timers timerId newState }
            , Cmd.none
            )

        UndoDeleteTimer id Nothing ->
            ( model
            , Task.perform (UndoDeleteTimer id) <| Task.map Just Time.now
            )

        UndoDeleteTimer id (Just now) ->
            case Timers.getById model.timers id of
                {- Should we tell the user that there's nothing to restore? -}
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just aTimer ->
                    let
                        ( newTimers, action ) =
                            createTimer
                                model.timers
                                { now = now
                                , subject = getOptimistic aTimer.subject |> Timers.subjectToString
                                , duration = aTimer.duration
                                , autoStart = False
                                , idToReplace = Just id
                                }
                    in
                        ( { model | timers = Timers.stop newTimers id }
                        , action
                        )

        UpdateLoggedTime index string ->
            {- fixme -}
            ( model
            , Cmd.none
            )

        UpdateTimeInTimer index string ->
            {- fixme -}
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every second Tick
        , getNotificationsState NotificationsStatusUpdate
        , getNotificationClicks (\e -> NotificationClicked e Nothing)
        ]


createEmptyModel : Time -> Model
createEmptyModel time =
    { timeNow = time
    , userInfo = RemoteData.NotAsked
    , dailySummaries = Dict.empty
    , defaultItinerary = Workday emptyItinerary
    , customItineraries = Dict.empty
    , timers = Timers.fromList []
    , timersRaw = RemoteData.NotAsked
    , timersLastRequested = Nothing
    , activities = []
    , activitiesRaw = RemoteData.NotAsked
    , cmdMisbehaviour = []
    , lastAlertTime = Nothing
    , notificationsState = Unknown
    , targets = Dict.empty
    }


emptyItinerary : WorkdayItinerary
emptyItinerary =
    { morning = ( hoursMinutes 8 30, hoursMinutes 12 0 )
    , afternoon = ( hoursMinutes 13 0, hoursMinutes 17 0 )
    }


hoursMinutes : Float -> Float -> Time
hoursMinutes hours minutes =
    Time.hour * hours + Time.minute * minutes


delayThen : Time -> Msg -> Cmd Msg
delayThen delay msg =
    Task.perform (\_ -> msg) <| sleep delay


getTimeThen : (Maybe Time -> Msg) -> Cmd Msg
getTimeThen msgNeedingTime =
    Task.perform msgNeedingTime <| Task.map Just Time.now


fetchUserInfoThen : Msg -> Cmd Msg
fetchUserInfoThen nextMsg =
    Http.get "/api/v0/staff/whoami" decodeUserInfo
        |> RemoteData.sendRequest
        |> Cmd.map (ParseUserInfoResponseThen nextMsg)


fetchTimers : Time -> Cmd Msg
fetchTimers now =
    let
        urlObject =
            BasicUrl.create "/api/v0/timers"
                |> Result.map (BasicUrl.setFields timerFields)
                |> Result.map (BasicUrl.setParam ( "hey", Just "whereAreTheWebhooksForThis" ))
    in
        case urlObject of
            Ok url ->
                recursiveFetch (decodeTimers now) [] url
                    |> RemoteData.asCmd
                    |> Cmd.map (\a -> ParseTimersResponse a Nothing)

            Err err ->
                Debug.crash <| "Assembling the timers URL should never fail... " ++ toString err


{-|
todo: support `against_id`, `against_type`
-}
createTimer : Timers -> Timers.TimerBase -> ( Timers, Cmd Msg )
createTimer timers base =
    let
        autoStartValue =
            case base.autoStart of
                True ->
                    "1"

                False ->
                    "0"

        url =
            "/api/v0/timers?subject="
                ++ base.subject
                ++ "&seconds="
                ++ (toString <| round <| Time.inSeconds base.duration)
                ++ "&auto_start="
                ++ autoStartValue
                ++ "&_fields="
                ++ timerFields

        newTimers =
            Timers.upsert
                base.now
                (Timers.timerFromBase base)
                timers
    in
        ( newTimers
        , Http.post url Http.emptyBody (extractResponseValueThen <| decodeResponseTimer <| base.now)
            |> RemoteData.sendRequest
            |> Cmd.map (\a -> ParseTimerCreationResponse base.idToReplace a Nothing)
        )


setTimerDate : Timers -> TimerId -> Date -> Time -> ( Timers, Cmd Msg )
setTimerDate timers id date now =
    case Timers.getById timers id of
        Nothing ->
            ( timers, Cmd.none )

        Just anOldTimer ->
            let
                newTimer =
                    Timers.setDate anOldTimer id date

                url =
                    "/api/v0/timers/"
                        ++ id
                        ++ "?_fields="
                        ++ timerFields
                        ++ "&subject="
                        ++ (Http.encodeUri <| Timers.subjectToString <| Optimism.getOptimistic newTimer.subject)
            in
                ( Timers.upsert now newTimer timers
                , Http.request
                    { method = "PUT"
                    , headers = []
                    , url = url
                    , body = Http.emptyBody
                    , expect = Http.expectJson (extractResponseValueThen <| decodeResponseTimer now)
                    , timeout = Nothing
                    , withCredentials = False
                    }
                    |> RemoteData.sendRequest
                    |> Cmd.map (\a -> ParseTimerSubjectUpdateResponse id a Nothing)
                )


resumeTimer : Time -> String -> Cmd Msg
resumeTimer now timerId =
    let
        url =
            "/api/v0/timers/"
                ++ timerId
                ++ "/start"
                ++ "?_fields="
                ++ timerFields
    in
        Http.post url Http.emptyBody (extractResponseValueThen <| decodeResponseTimer now)
            |> RemoteData.sendRequest
            |> Cmd.map (\a -> ParseTimerCreationResponse (Just timerId) a Nothing)


stopTimer : Time -> String -> Cmd Msg
stopTimer now timerId =
    let
        url =
            "/api/v0/timers/"
                ++ timerId
                ++ "/pause"
                ++ "?_fields="
                ++ timerFields
    in
        Http.post url Http.emptyBody (extractResponseValueThen <| decodeResponseTimer now)
            |> RemoteData.sendRequest
            |> Cmd.map (\a -> ParseTimerCreationResponse (Just timerId) a Nothing)


deleteTimer : String -> Cmd Msg
deleteTimer timerId =
    let
        url =
            "/api/v0/timers/" ++ timerId
    in
        Http.request
            { method = "DELETE"
            , headers = []
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectJson (decodeEmptyAcceloResponse)
            , timeout = Nothing
            , withCredentials = False
            }
            |> RemoteData.sendRequest
            |> Cmd.map (ParseTimerDeletionResponse timerId)


{-| Ten items in list ∴ full page ∴ there may be further pages
-}
nextPageFetchable : List a -> Bool
nextPageFetchable =
    List.length
        >> (<=) itemsPerPage


incrementUrlPage : BasicUrl.BasicUrl -> BasicUrl.BasicUrl
incrementUrlPage urlObject =
    let
        currentPage =
            BasicUrl.getParam "_page" urlObject
                |> Maybe.map (String.toInt >> Result.withDefault 0)
                |> Maybe.withDefault 0
    in
        BasicUrl.setPage (currentPage + 1) urlObject


{-| Keep fetching pages one-by-one, as long as they indicate there's a next page.
-}
recursiveFetch : Decoder (List items) -> List items -> BasicUrl.BasicUrl -> Task.Task Http.Error (List items)
recursiveFetch decoder accumulator urlObject =
    Http.get (BasicUrl.toString urlObject) decoder
        |> Http.toTask
        |> Task.andThen
            (\items ->
                case nextPageFetchable items of
                    True ->
                        recursiveFetch decoder (accumulator ++ items) (incrementUrlPage urlObject)

                    False ->
                        Task.succeed (accumulator ++ items)
            )


{-| Somehow, we know how many items (and therefore pages) to expect. Get them all, as fast as you can.

Usage example:

```elm
batchFetch decodeActivities url 42
    |> RemoteData.asCmd
    |> Cmd.map ParseTodaysActivitiesResponse
```

NB: Not actually parallel; waiting for Elm's Process module to be fleshed out; see
<https://github.com/elm-lang/core/issues/223#issuecomment-228553615>
-}
batchFetch : Decoder (List items) -> BasicUrl.BasicUrl -> Int -> Task.Task Http.Error (List items)
batchFetch decoder urlObject itemCount =
    List.range 0 (itemCount // itemsPerPage)
        |> List.map toString
        |> List.map
            (\page ->
                Http.get (BasicUrl.toString <| BasicUrl.setParam ( "_page", Just page ) urlObject) decoder
            )
        |> List.map Http.toTask
        |> Task.sequence
        -- todo: use Task.parallel (or... ?)
        |>
            Task.map (List.foldr (++) [])


logMisbehaviour : Model -> List String -> Model
logMisbehaviour model list =
    { model | cmdMisbehaviour = model.cmdMisbehaviour ++ [ "⏲ " ++ toString model.timeNow ] ++ list }


fetchActivitiesInRange : String -> Date -> Date -> Date -> Model -> ( Model, Cmd Msg )
fetchActivitiesInRange staffID dateFrom dateTo modifiedSince model =
    let
        filterComponents =
            [ "date_logged_after(" ++ (toString <| toUnixTime dateFrom) ++ ")"
            , "date_logged_before(" ++ (toString <| toUnixTime dateTo) ++ ")"
            , "date_modified_after(" ++ (toString <| toUnixTime modifiedSince) ++ ")"
            , "staff(" ++ staffID ++ ")"
            ]

        setParams =
            (BasicUrl.setFilters <| String.join "," filterComponents)
                >> BasicUrl.setFields "id,date_created,date_started,date_ended,date_due,date_modified,staff,billable,nonbillable"

        urlObject =
            BasicUrl.create "/api/v0/activities"
                |> Result.map setParams
                |> Result.map (BasicUrl.setParam ( "hey", Just "whereAreTheWebhooksForThis" ))
    in
        case ( dateFrom == dateTo, urlObject ) of
            ( True, _ ) ->
                -- fixme: don't swallow errors
                ( logMisbehaviour model
                    [ "fetchActivitiesInRange"
                    , "dateFrom == dateTo"
                    , toString dateFrom
                    ]
                , Cmd.none
                )

            ( False, Ok url ) ->
                ( model
                , recursiveFetch decodeActivities [] url
                    |> RemoteData.asCmd
                    |> Cmd.map ParseTodaysActivitiesResponse
                )

            ( False, Err err ) ->
                -- fixme: don't swallow errors
                ( logMisbehaviour model
                    [ "fetchActivitiesInRange"
                    , "urlObject is Err"
                    , toString err
                    ]
                , Cmd.none
                )


fetchTodaysActivities : Model -> ( Model, Cmd Msg )
fetchTodaysActivities model =
    case model.userInfo of
        RemoteData.NotAsked ->
            -- need the staff ID; get it & try again
            ( model
            , Task.perform
                (always <| FetchUserInfoThen <| FetchTodaysActivities)
                (Task.succeed Nothing)
            )

        RemoteData.Loading ->
            -- fixme: don't swallow errors
            ( logMisbehaviour model
                [ "fetchTodaysActivities"
                , "userInfo is loading, but someone triggered this?"
                , "userInfo: " ++ toString model.userInfo
                , "timeNow: " ++ toString model.timeNow
                ]
            , Cmd.none
            )

        RemoteData.Failure err ->
            -- fixme: don't swallow errors
            ( logMisbehaviour model
                [ "fetchTodaysActivities"
                , "userInfo request failed (should we retry?)"
                , "userInfo: " ++ toString model.userInfo
                , "timeNow: " ++ toString model.timeNow
                ]
            , Cmd.none
            )

        RemoteData.Success userInfo ->
            let
                dateFrom =
                    Date.fromTime model.timeNow
                        |> Date.Extra.add Date.Extra.Day -6
                        |> Date.Extra.floor Date.Extra.Day

                dateTo =
                    Date.fromTime model.timeNow
                        |> Date.Extra.ceiling Date.Extra.Day
            in
                fetchActivitiesInRange userInfo.id dateFrom dateTo (lastActivitySeen model.activities) model


maxDate : Date -> Date -> Date
maxDate a b =
    case Date.Extra.compare a b of
        GT ->
            a

        _ ->
            b


lastActivitySeen : ListOfActivities -> Date
lastActivitySeen =
    List.map .date_modified
        >> List.foldr maxDate (fromUnixTime 0)


extractResponseValueThen : Decoder a -> Decoder a
extractResponseValueThen nextDecoder =
    Json.Decode.map .response (decodeAcceloResponse <| nextDecoder)


decodeTimers : Time -> Decoder (List Timer)
decodeTimers now =
    (extractResponseValueThen <| Json.Decode.list <| decodeResponseTimer now)


decodeUserInfo : Decoder UserInfo
decodeUserInfo =
    extractResponseValueThen <|
        (decode UserInfo
            |> required "id" (Json.Decode.string)
            |> required "firstname" (Json.Decode.string)
            |> required "surname" (Json.Decode.string)
        )


decodeActivities : Decoder ListOfActivities
decodeActivities =
    extractResponseValueThen <| Json.Decode.list decodeResponseActivity


decodeAcceloResponse : Decoder a -> Decoder (AcceloResponse a)
decodeAcceloResponse responseDecoder =
    decode AcceloResponse
        |> required "meta" (decodeResponseMeta)
        |> required "response" (responseDecoder)


decodeEmptyAcceloResponse : Decoder EmptyAcceloResponse
decodeEmptyAcceloResponse =
    decode EmptyAcceloResponse
        |> required "meta" (decodeResponseMeta)


decodeResponseMeta : Decoder AcceloResponseMeta
decodeResponseMeta =
    decode AcceloResponseMeta
        |> required "status" (Json.Decode.string)
        |> required "message" (Json.Decode.string)
        |> required "more_info" (Json.Decode.string)


getDictVal : Dict.Dict String Json.Decode.Value -> String -> Result String String
getDictVal dict key =
    Dict.get key dict
        |> Result.fromMaybe ("'against_' key missing: " ++ key)
        |> Result.andThen (Json.Decode.decodeValue Json.Decode.string)


dictToAgainst : Dict.Dict String Json.Decode.Value -> List Against
dictToAgainst d =
    case ( getDictVal d "against_id", getDictVal d "against_type", getDictVal d "against_title" ) of
        ( Ok id, Ok type_, Ok "" ) ->
            [ Against id type_ "<no title>" ]

        ( Ok id, Ok type_, Ok title ) ->
            [ Against id type_ title ]

        ( Ok id, Ok type_, Err _ ) ->
            [ Against id type_ "<title decode error>" ]

        ( _, _, _ ) ->
            []


{-| A Timers response includes three fields: `against_id`, `against_type`, and
`against_title`. But, our model represents those three as a single Type.
So we must decode three separate fields as one.

todo: How can (should we?) fail the decode if the fields aren't present?
-}
againstDecoder : Decoder (List Against)
againstDecoder =
    Json.Decode.dict Json.Decode.value
        |> Json.Decode.map dictToAgainst


decodeResponseTimer : Time -> Decoder Timer
decodeResponseTimer now =
    decode Timer
        |> custom againstDecoder
        |> required "id" Json.Decode.string
        |> required "seconds" decodeDuration
        |> {- targetDuration -} hardcoded Nothing
        |> required "staff" Json.Decode.string
        |> required "status" (Json.Decode.map (Unchanged << mapTimerStatus Nothing) Json.Decode.string)
        |> required "subject" (Json.Decode.map (Unchanged << Timers.subjectFromString (Just now)) Json.Decode.string)
        |> {- alerts -} hardcoded []
        |> {- dateSelectorOpen -} hardcoded Nothing


decodeResponseActivity : Decoder Activity
decodeResponseActivity =
    decode Activity
        |> required "id" Json.Decode.string
        |> required "date_created" decodeAcceloUnixString
        |> optional "date_started" decodeOptionalAcceloUnixString Nothing
        |> optional "date_ended" decodeOptionalAcceloUnixString Nothing
        |> optional "date_due" decodeOptionalAcceloUnixString Nothing
        |> required "date_modified" decodeAcceloUnixString
        |> required "staff" Json.Decode.string
        |> optional "billable" decodeDuration 0
        |> optional "nonbillable" decodeDuration 0


decodeEvent : Decoder (Event DomNotification)
decodeEvent =
    decode Event
        |> required "defaultPrevented" Json.Decode.bool
        |> required "type_" Json.Decode.string
        |> required "target" decodeDomNotification


decodeDomNotification : Decoder DomNotification
decodeDomNotification =
    decode DomNotification
        |> required "body" Json.Decode.string
        |> required "tag" Json.Decode.string
        |> required "timestamp" (Json.Decode.map ((*) Time.millisecond) Json.Decode.float)
        |> required "title" Json.Decode.string


tickTimer : Time -> Timer -> Timer
tickTimer now timer =
    case getOptimistic timer.status of
        Deleted _ ->
            timer

        Stopped ->
            timer

        RunningRelative ->
            -- not enough info to modify duration
            timer

        Running start ->
            let
                diff =
                    Date.Extra.diff Date.Extra.Millisecond start (Date.fromTime now)
            in
                { timer | duration = toFloat diff }


tickTimers : Time -> Timers -> Timers
tickTimers now timers =
    Timers.map (tickTimer now) timers


{-| Upsert this new timer into our local list of timers.

If `idToReplace` is specified, the timer with that ID will be removed before the upsert.
-}
updateWithReceivedTimer : Model -> Time -> Timer -> Maybe String -> Model
updateWithReceivedTimer model theTime newTimer idToReplace =
    let
        timersWithDeleted =
            case idToReplace of
                Nothing ->
                    model.timers

                Just anId ->
                    Timers.deleteForever model.timers anId
    in
        { model
            | timers = Timers.upsert theTime newTimer timersWithDeleted
        }


{-| The received timers represent the whole list. We drop any we had
that aren't in this new list.
-}
updateWithReceivedTimers : Model -> Time -> RemoteData.WebData (List Timer) -> Model
updateWithReceivedTimers model theTime response =
    case response of
        RemoteData.Success newTimers ->
            { model
                | timers = Timers.joinOuterRight theTime model.timers newTimers
                , timersRaw = response
            }

        -- fixme: don't swallow errors
        RemoteData.Failure _ ->
            { model
                | timersRaw = response
            }

        RemoteData.NotAsked ->
            { model
                | timersRaw = response
            }

        RemoteData.Loading ->
            { model
                | timersRaw = response
            }


updateWithReceivedActivities : Model -> RemoteData.WebData ListOfActivities -> Model
updateWithReceivedActivities model response =
    case response of
        RemoteData.Success activities ->
            { model
                | activities = applyDelta model.activities activities
                , activitiesRaw = response
            }

        _ ->
            { model
                | activitiesRaw = response
            }


{-| Given two lists of items, update the first with items from the second.

Updated items will be moved to the end of the list.

Deletions not considered.
-}
applyDelta : List (Identified a) -> List (Identified a) -> List (Identified a)
applyDelta base delta =
    let
        newIds =
            List.map .id delta

        onlyOlds =
            List.filter (\x -> not <| List.member x.id newIds) base
    in
        onlyOlds ++ delta


mapTimerStatus : Maybe Date -> String -> TimerStatus
mapTimerStatus start s =
    case ( s, start ) of
        ( "running", Nothing ) ->
            RunningRelative

        ( "running", Just date ) ->
            Running date

        ( "stopped", _ ) ->
            Stopped

        ( _, _ ) ->
            Stopped


parseSeconds : String -> Result String Duration
parseSeconds s =
    String.toFloat s
        |> Result.map (\t -> t * second)


decodeDuration : Decoder Duration
decodeDuration =
    Json.Decode.string
        |> andThen
            (\s ->
                case parseSeconds s of
                    Ok duration ->
                        succeed duration

                    Err err ->
                        fail err
            )


{-| Decode a unix timestamp in a string.

E.g., `"1486692359"`. Note that Accelo measures by the second, but
Elm measures by the millisecond.

If parsing the string fails, the whole decoder fails.
-}
decodeAcceloUnixString : Decoder Date
decodeAcceloUnixString =
    Json.Decode.string
        |> andThen
            (\s ->
                case String.toFloat s of
                    Ok timestamp ->
                        succeed <| fromUnixTime timestamp

                    Err err ->
                        fail err
            )


{-| Decode an optional unix timestamp in a string.

If parsing fails, return Nothing.
-}
decodeOptionalAcceloUnixString : Decoder (Maybe Date)
decodeOptionalAcceloUnixString =
    Json.Decode.string
        |> andThen
            (\s ->
                case String.toFloat s of
                    Ok timestamp ->
                        succeed <| Just <| fromUnixTime timestamp

                    Err err ->
                        succeed Nothing
            )


toIsoDate : Date -> String
toIsoDate =
    Date.Format.format "%Y-%m-%d"


parseNotificationState : String -> NotificationsState
parseNotificationState state =
    case String.toLower state of
        "denied" ->
            Denied

        "granted" ->
            Granted

        "default" ->
            NotAsked

        "notsupported" ->
            NotSupported

        _ ->
            Unrecognized <| String.toLower state


parseNotificationTag : String -> NotificationTag
parseNotificationTag s =
    case s of
        "DeficitGrowth" ->
            DeficitGrowth

        "Test" ->
            Test

        _ ->
            UnrecognizedTag s


alertDeficit : Time -> Maybe Time -> NotificationsState -> Timers -> RemoteData.WebData a -> DailySummaries -> ( Maybe Time, Cmd Msg )
alertDeficit now lastAlertTime notificationsState timers activitiesReq summaries =
    let
        activitiesLoaded =
            case activitiesReq of
                RemoteData.Success _ ->
                    True

                RemoteData.Loading ->
                    False

                RemoteData.NotAsked ->
                    False

                RemoteData.Failure _ ->
                    False

        notificationsAllowed =
            case notificationsState of
                Granted ->
                    True

                Denied ->
                    False

                NotAsked ->
                    False

                NotSupported ->
                    False

                Unknown ->
                    False

                Unrecognized _ ->
                    False

        timersRunning =
            Timers.anyRunning timers

        timeSinceLast =
            now - (lastAlertTime |> Maybe.withDefault 0)

        notTooSoon =
            timeSinceLast > (2 * Time.minute)

        todaysDeficit =
            calcTodaysDeficit summaries now
                |> Maybe.map stringifyDuration

        negativeResult =
            ( lastAlertTime
            , Cmd.none
            )

        -- should we send alerts outside of the active hours of today's Itinerary?
    in
        case ( notificationsAllowed && activitiesLoaded && not timersRunning && notTooSoon, todaysDeficit ) of
            ( True, Just deficit ) ->
                ( Just now
                , sendNotification
                    { title = "The deficit is growing... "
                    , body = Just <| "Quick! Start a timer for " ++ deficit
                    , tag = toString DeficitGrowth
                    , icon = notificationIconUrl
                    , timestamp = now
                    , silent = False
                    , renotify = True
                    }
                )

            ( True, Nothing ) ->
                negativeResult

            ( False, Just _ ) ->
                -- todo: show in-page toast
                negativeResult

            ( False, Nothing ) ->
                negativeResult


sumActivities : ListOfActivities -> Duration
sumActivities activities =
    let
        billable =
            List.foldr (+) 0 <| List.map .billable activities

        nonbillable =
            List.foldr (+) 0 <| List.map .nonbillable activities
    in
        billable + nonbillable



-- todo: add corresponding encoders, for updating timers


foldActivityTime : Activity -> TimesOverDays -> TimesOverDays
foldActivityTime activity times =
    let
        date =
            -- todo: what is the correct field to use, for the "logged date" of the activity?
            activity.date_started
                |> Maybe.withDefault activity.date_created
                |> toIsoDate

        duration =
            activity.billable + activity.nonbillable

        prior =
            Dict.get date times |> Maybe.withDefault 0
    in
        Dict.insert date (prior + duration) times


groupSumActivities : ListOfActivities -> TimesOverDays
groupSumActivities activities =
    List.foldr foldActivityTime Dict.empty activities


{-| Given a range [a, b] and a variable x, how much of the range is
less than x?
E.g:
    exposedRange 3 7 1 == 0
    exposedRange 3 7 4 == 1
    exposedRange 3 7 5 == 2
    exposedRange 7 3 5 == 2
    exposedRange 3 7 9 == 4
    exposedRange 3 7 102 == 4
-}
exposedRange : ( Float, Float ) -> Float -> Float
exposedRange ( a, b ) x =
    let
        start =
            min a b

        end =
            max a b
    in
        clamp 0 (end - start) (x - start)


getItineraryProgress : Itinerary -> Date -> Duration
getItineraryProgress itinerary now =
    case itinerary of
        Nonworkday ->
            0

        Workday itinerary ->
            let
                startOfDay =
                    Date.Extra.floor Date.Extra.Day now

                timeOfDay =
                    Date.Extra.diff Date.Extra.Second startOfDay now
                        |> toFloat
                        |> (*) Time.second

                morning =
                    exposedRange itinerary.morning timeOfDay

                afternoon =
                    exposedRange itinerary.afternoon timeOfDay
            in
                (morning + afternoon)


getItineraryTarget : Itinerary -> Duration
getItineraryTarget itinerary =
    case itinerary of
        Nonworkday ->
            0

        Workday itinerary ->
            let
                morning =
                    exposedRange itinerary.morning (24.0 * Time.hour)

                afternoon =
                    exposedRange itinerary.afternoon (24.0 * Time.hour)
            in
                morning + afternoon


isWeekday : Date -> Bool
isWeekday date =
    let
        dayOfWeek =
            Date.Extra.toFormattedString "e" date
    in
        dayOfWeek /= "6" && dayOfWeek /= "7"


getItineraryForDate : Date -> Itinerary -> DailyItineraries -> Itinerary
getItineraryForDate date defaultItin customItineraries =
    let
        customItinerary =
            Dict.get (toIsoDate date) customItineraries

        fallbackItinerary =
            case isWeekday date of
                True ->
                    defaultItin

                False ->
                    Nonworkday
    in
        customItinerary |> Maybe.withDefault fallbackItinerary


{-|
TODO: Performance optimization
-}
getMissingTime : Model -> DailySummaries
getMissingTime model =
    let
        excludeDeletedTimers =
            Timers.filter (.status >> getOptimistic >> Timers.isDeleted >> not) model.timers
    in
        createDailySummaries
            (Date.fromTime model.timeNow)
            (groupSum excludeDeletedTimers)
            (groupSumActivities model.activities)
            model.customItineraries
            model.defaultItinerary


{-| Convert a `Date` to a time in seconds.

A time is the number of seconds since
[the Unix epoch](http://en.wikipedia.org/wiki/Unix_time).
-}
toUnixTime : Date -> Time
toUnixTime =
    Date.toTime >> (\t -> t / Time.second)


{-| Convert a time in seconds into a `Date`.

A time is the number of seconds since
[the Unix epoch](http://en.wikipedia.org/wiki/Unix_time).
-}
fromUnixTime : Time -> Date
fromUnixTime =
    (\t -> t * Time.second) >> Date.fromTime


createDailySummary : Date -> TimesOverDays -> TimesOverDays -> DailyItineraries -> Itinerary -> String -> ( String, DaySummary )
createDailySummary now timers activities customItineraries defaultItinerary date =
    let
        inTimers =
            Dict.get date timers |> Maybe.withDefault 0

        inActivities =
            Dict.get date activities |> Maybe.withDefault 0

        itinerary =
            Dict.get date customItineraries |> Maybe.withDefault defaultItinerary

        target =
            case date == toIsoDate now of
                True ->
                    getItineraryProgress itinerary now

                False ->
                    getItineraryTarget itinerary
    in
        ( date
        , { inTimers = inTimers
          , inActivities = inActivities
          , target = target
          , missing = target - (inTimers + inActivities)
          }
        )


createDailySummaries : Date -> TimesOverDays -> TimesOverDays -> DailyItineraries -> Itinerary -> DailySummaries
createDailySummaries now timers activities customItineraries defaultItinerary =
    let
        keys =
            [ toIsoDate now ]
                ++ (Dict.keys timers)
                ++ (Dict.keys activities)
                |> Set.fromList
                |> Set.toList

        withDurations =
            List.map (createDailySummary now timers activities customItineraries defaultItinerary) keys
    in
        Dict.fromList withDurations


{-| Turn a Time / Duration / Float into a string like `9h 30m`.
-}
stringifyDuration : Duration -> String
stringifyDuration duration =
    let
        absDuration =
            abs duration

        -- Not sure this is the right way to go...
        -- todo: look up modulo behaviour on negative numbers
        sign =
            case duration < 0 of
                True ->
                    "-"

                False ->
                    ""

        hours =
            floor <| Time.inHours absDuration

        minutes =
            mod 60 <| floor <| Time.inMinutes absDuration

        seconds =
            mod 60 <| floor <| Time.inSeconds absDuration

        components =
            [ hours, minutes, seconds ]

        templates =
            [ { prefix = "", suffix = "h" }
            , { prefix = "", suffix = "m" }
            , { prefix = "", suffix = "s" }
            ]
    in
        List.map2 durationComponentToString templates components
            |> List.filter ((/=) "")
            |> String.join " "
            |> String.trim
            |> (++) sign


durationComponentToString : DurationComponentTemplate -> Int -> String
durationComponentToString template amount =
    case amount of
        0 ->
            ""

        _ ->
            template.prefix ++ toString amount ++ template.suffix


{-| `mod 2 5` === `flip (%) 2 5`, but easier to read?
-}
mod : Int -> Int -> Int
mod divisor dividend =
    dividend % divisor


calcTodaysDeficit : DailySummaries -> Time -> Maybe Duration
calcTodaysDeficit summaries now =
    Dict.get (now |> Date.fromTime >> toIsoDate) summaries
        |> Maybe.map .missing
        |> Maybe.andThen
            (\deficit ->
                if deficit <= 0 then
                    Nothing
                else
                    Just deficit
            )


moreThan : Duration -> Time -> Maybe Time -> Bool
moreThan gap timeA timeB =
    (timeA - (timeB |> Maybe.withDefault 0)) >= gap
