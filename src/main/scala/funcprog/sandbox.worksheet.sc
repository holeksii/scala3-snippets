def whileLoop(cond: => Boolean)(body: => Unit): Unit =
  if cond then
    body
    whileLoop(cond)(body)

def whileTrue(body: => Unit): Unit = whileLoop(true)(body)

var i = 0
whileLoop(i < 10) {
  print(i)
  i += 1
}
