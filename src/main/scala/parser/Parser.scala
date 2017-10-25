package parser

object Parser {

  type Operator = (Int, Int) => Int

  object Operator {
    val operators: Map[String, Operator] =
      Map("+" -> { _+_ },
          "-" -> {_ - _},
          "*" -> {_ * _},
          "/" -> {_ / _})
    val tokens: Map[Operator, String] = operators map {_.swap}

    def unapply(token: String): Option[Operator] = operators.get(token)
  }

  object Number {
    def unapply(token: String): Option[Int] = try {
      Some(token.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }

  sealed trait Expression
  case class NumberExpression(value: Int) extends Expression
  case class OperationExpression(lhs: Expression, rhs: Expression, op: Operator) extends Expression

  def step(stack: List[Expression], token: String): List[Expression] = {
    token match {
      case Number(n) => NumberExpression(n) :: stack
      case Operator(op) => stack match {
        case rhs :: lhs :: rest => OperationExpression(lhs, rhs, op) :: rest
        case _ => throw new IllegalArgumentException("Bad input")
      }
    }
  }

  def pars(input: String): Expression = {
    val tokens = input.split("\\s+")
    val expression = tokens.foldLeft(List.empty[Expression])(step)
    expression.head
  }

  def calculate(expression: Expression): Int = expression match {
    case NumberExpression(value) => value
    case OperationExpression(lhs, rhs, op) => op(calculate(lhs), calculate(rhs))
  }

  def toInfix(expression: Expression): String = expression match {
    case NumberExpression(value) => value.toString
    case OperationExpression(lhs, rhs, op) => s"(${toInfix(lhs)} ${Operator.tokens(op)} ${toInfix(rhs)})"
  }

  def main(args: Array[String]): Unit = {
    val exp = pars(args(0))
    println(s"${toInfix(exp)} = ${calculate(exp)}")
  }
}
