fun factorial n = let
    fun lp (0, acc) = acc
      | lp (m, acc) = lp (m-1, m*acc)
    in
        lp (n, 1)
    end

type loc = real * real

fun dist ((x0, y0), (x1, y1)) = let
    val dx = x1 - x0
    val dy = y1 - y0
    in
        Math.sqrt (dx * dx + dy * dy)
    end

fun heron (a, b, c) = let
    val ab = dist (a, b)
    val bc = dist (b, c)
    val ac = dist (a, c)
    val perim = ab + bc + ac
    val s = perim / 2.0
    in
        Math.sqrt (s * (s - ab) * (s - bc) * (s - ac))
    end
