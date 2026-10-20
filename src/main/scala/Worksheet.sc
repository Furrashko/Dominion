println("hello")
val x = 1

def add(a: Int, b: Int): Int = a + b
add(5, 3)

def greet(name: String): String = s"Hallo, $name!"
greet("David")

def isEven(n: Int): Boolean = n % 2 == 0
isEven(10)
isEven(7)

val tuple = (42, "Luke", true)
tuple._1
tuple(0)
tuple._2
tuple(1)
tuple._3
tuple(2)

def div(x: Int, y: Int): (Int, Int) = (x / y, x % y)
val divResult = div(10, 3)
divResult(0)
divResult(1)

val (quotient, remainder) = div(10, 3)

