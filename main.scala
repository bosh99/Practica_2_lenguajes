import scala.io.StdIn.readLine

sealed trait Tree[+A] 
case object Nil extends Tree[Nothing]
case class Leaf[+A](value: A) extends Tree[A] //caso hoja
case class Branch[+A](left:Tree[A],operator:A,right:Tree[A]) extends Tree[A] //caso rama --> nodo con hijos 


object Tree {

    def apply[A](expression:Array[A]): Tree[A] = {

        def go[A](i: Int, operators:List[String], expression:Array[A]): (Tree[A], Int) = {
            if (expression.isEmpty) (Nil,0)
            else {
                val bool = operators.contains(expression(i))
                if (bool) {
                    val left = go(i+1, operators, expression)
                    val j = left._2
                    val right = go(j+1, operators, expression)
                    (Branch(left._1, expression(i), right._1),j+1)
                } 
                else (Leaf(expression(i)), i)
            }
        }
        go(0, List("*","+","-","/"), expression)._1
    }

    def translate[A](tree: Tree[A]): String = tree match {
        case Nil => ""
        case Leaf(value) => value.toString()
        case Branch(left, operator, right) => {
            val x = translate(left)
            val y = translate(right)
            val result = x + operator.toString() + y
            result
        }
    }

    def calculate[A](tree: Tree[A]): Int = tree match {
        case Nil => 0
        case Leaf(value) => value.toString().toInt
        case Branch(left, operator, right) => {
            val x = calculate(left)
            val y = calculate(right)
            val result = operate(x, y, operator.toString())
            result
        }
    }

    def operate(num1: Int, num2: Int, operation: String):Int = operation match {
        case "+" => num1+num2
        case "*" => num1*num2
        case "-" => num1-num2
        case "/" => {
            try {
                num1/num2
            } catch {
                case cero: ArithmeticException => sys.error("Sorry there was a problem...")
            }
        }
    }

}

object Main{

    def main(args: Array[String]): Unit = {
        print("Please enter the operation (separated by spaces): ")
        val expression = readLine()
        val expOnTree = Tree(expression.split(" "))
        println(expOnTree)

        val algebraicExpression = Tree.translate(expOnTree)
        println(s"Algebraic expression: $algebraicExpression")
        val result = Tree.calculate(expOnTree)
        println(s"Result: $result")


    }

    // * + 5 9 - 7 8
    // * - 5 7 / 4 0
    // / * - 4 2 + 7 1 - * 2 2 + 9 1
}