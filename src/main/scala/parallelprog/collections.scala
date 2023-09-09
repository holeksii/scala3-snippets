package parallelprog

def mapArrSeg[A, B](
    in: Array[A],
    left: Int,
    right: Int,
    f: A => B,
    out: Array[B]
): Unit =
  var i = left
  while i < right do
    out(i) = f(in(i))
    i += 1

def mapArrayPar[A, B](
    in: Array[A],
    f: A => B,
    numOfTasks: Int,
    out: Array[B]
): Unit =
  val treshold = in.length / numOfTasks

  def mapFromLeftToRight(left: Int, right: Int): Unit =
    if right - left < treshold then mapArrSeg(in, left, right, f, out)
    else
      val mid = left + (right - left) / 2
      parallel(
        mapFromLeftToRight(left, mid),
        mapFromLeftToRight(mid, right)
      )

  mapFromLeftToRight(0, in.length)
