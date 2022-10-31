import scala.io.StdIn.readLine

sealed trait Tree
case object Nil extends Tree
case class Leaf(value: Char) extends Tree
case class Branch(left:Tree,operator:Char,right:Tree) extends Tree 


object Tree {

    def apply(expression: List[Char]): Tree = {
        buildTree(expression, List('*','+','-','/'))._1
    }

    def buildTree(expression: List[Char], operators:List[Char]): (Tree, List[Char]) = expression match {
        case e if (e.isEmpty) => (Nil, List())
        case head :: tail if (!operators.contains(head)) => (Leaf(head), tail)
        case head :: tail if (operators.contains(head)) => {
            val (left, restAfterLeft) = buildTree(tail, operators)
            val (right, restAfterRight) = buildTree(restAfterLeft, operators)
            (Branch(left, head, right), restAfterRight)
        }
        case _ => sys.error("There was a problem...")
    }

    def translate(tree: Tree): String = tree match {
        case Nil => ""
        case Leaf(value) => value.toString()
        case Branch(left, operator, right) => {
            val x = translate(left)
            val y = translate(right)
            val result = x + operator + y
            result
        }
    }

    def calculate(tree: Tree): Int = tree match {
        case Nil => 0
        case Leaf(value) => value.toString().toInt
        case Branch(left, operator, right) => {
            val x = calculate(left)
            val y = calculate(right)
            val result = operate(x, y, operator)
            result.getOrElse(0)
        }
    }

    def operate(num1: Int, num2: Int, operation: Char):Option[Int] = operation match {
        case '+' => Some(num1+num2)
        case '*' => Some(num1*num2)
        case '-' => Some(num1-num2)
        case '/' => try {
            Some(num1/num2)
        } catch {
            case e: ArithmeticException => println("I can`t divide by 0"); sys.exit(0)
        }
    }

}

object Main{

    def main(args: Array[String]): Unit = {
        print("Please enter the operation (separated by spaces): ")
        val expression = readLine()

        val expOnTree = Tree(expression.toList)
        println(expOnTree)

        val algebraicExpression = Tree.translate(expOnTree)
        println(s"Algebraic expression: $algebraicExpression")

        val result = Tree.calculate(expOnTree)
        println(s"Result: $result")


    }

    // *+59-78
    // *-57/40
    // /*-42+71-*22+91
}