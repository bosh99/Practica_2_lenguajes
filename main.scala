sealed trait Tree[+A] 
case object Nil extends Tree[Nothing]
case class Leaf[+A](value: A) extends Tree[A] //caso hoja
case class Branch[+A](left:Tree[A],operator:A,right:Tree[A]) extends Tree[A] //caso rama --> nodo con hijos 


object Tree{
    def apply[A](i:Int, operators:List[A],expression:List[A]): (Tree[A],Int) = {
        if (expression.isEmpty) (Nil,0)
        else{
            val bool = operators.contains(expression(i))
            if (bool){
                val left = apply(i+1,operators,expression)
                val j = left._2
                val right = apply(j+1,operators,expression)
                (Branch(left._1,expression(i),right._1),j+1)
            } 
            else (Leaf(expression(i)),i)
        }
    } 
}

object Main{
    def main(args: Array[String]): Unit = {
        val operators = List("*","+","-","/")
        val lstExpression = List("*","+","5","9","-","7","8")
        val expOnTree = Tree(0,operators,lstExpression)
        println(expOnTree._1)
    }
    // def create_expression(expression:String): List = {
        
    // }
}