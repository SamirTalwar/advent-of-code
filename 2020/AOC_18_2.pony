use "debug"
use collections = "collections"
use "itertools"

type Num is ISize

primitive Add is Stringable
  fun string(): String iso^ =>
    "+".clone()

primitive Multiply is Stringable
  fun string(): String iso^ =>
    "*".clone()

type Operator is (Add | Multiply)

class val Operation is Stringable
  let left: Expression
  let operator: Operator
  let right: Expression

  new val create(left': Expression, operator': Operator, right': Expression) =>
    left = left'
    operator = operator'
    right = right'

  fun string(): String iso^ =>
    "".join(["("; left.string(); " "; operator.string(); " "; right.string(); ")"].values())

type Expression is (Num | Operation)

type Token is (Num | Operator | Operation)

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = LineCollector[Expression](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[Expression] val](collector, solution)

class Parser is SingleItemParser[Expression]
  fun parse(line: String): Expression ? =>
    (let expression, let remainder) = _parse_sub_expression(line)?
    if remainder.size() > 0 then
      error
    end
    expression

  fun _parse_sub_expression(string: String): (Expression, String) ? =>
    let tokens = Array[Token]
    var remainder = string
    while true do
      if (remainder.size() == 0) then
        break
      else
        match remainder(0)?
        | ' ' =>
          remainder = remainder.substring(1)
        | '(' =>
          (let expr, remainder) = _parse_sub_expression(remainder.substring(1))?
          tokens.push(expr)
        | ')' =>
          remainder = remainder.substring(1)
          break
        | let c: U8 if (c >= '0') and (c <= '9') =>
          (let number, remainder) = _parse_number(remainder)?
          tokens.push(number)
        else
          (let operator, remainder) = _parse_operator(remainder)?
          tokens.push(operator)
        end
      end
    end

    if tokens.size() == 0 then
      error
    end

    let expression = _operation(tokens)?
    (expression, remainder)

  fun _parse_number(string: String): (Num, String) ? =>
    let offset = try
      Iter[U8](string.values())
        .zip[USize](collections.Range(0, string.size()))
        .find({ (pair) => (pair._1 < '0') or (pair._1 > '9') })?
        ._2
    else
      string.size()
    end
    if offset == 0 then
      error
    end
    let number = string.substring(0, offset.isize()).isize()?
    let remainder = string.substring(offset.isize())
    remainder.strip()
    (number, remainder)

  fun _parse_operator(string: String): (Operator, String) ? =>
    let operator = match string(0)?
    | '+' => Add
    | '*' => Multiply
    else error
    end
    let remainder = string.substring(1)
    remainder.strip()
    (operator, remainder)

  fun _operation(tokens: Array[Token]): Expression ? =>
    var multiplications = Array[Array[Token]]
    var last: USize = 0
    for (i, token) in tokens.pairs() do
      if tokens(i)? is Multiply then
        multiplications.push(tokens.slice(last, i))
        last = i + 1
      end
    end
    multiplications.push(tokens.slice(last))

    var expression = _sub_operation(multiplications.shift()?)?
    for multiplication in multiplications.values() do
      expression = Operation(expression, Multiply, _sub_operation(multiplication)?)
    end
    expression

  fun _sub_operation(tokens: Array[Token]): Expression ? =>
    var expression = match tokens.shift()?
    | let expr: Expression => expr
    else error
    end
    for i in collections.Range(0, tokens.size() where inc = 2) do
      let operator = match tokens(i)?
      | let operator: Operator => operator
      else error
      end
      let right = match tokens(i + 1)?
      | let expr: Expression => expr
      else error
      end
      expression = Operation(expression, operator, right)
    end
    expression

actor Solution is Solve[Array[Expression] val]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(expressions: Array[Expression] val) =>
    var result: Num = 0
    for expression in expressions.values() do
      result = result + _evaluate(expression)
    end
    _answer.answer(result)

  fun _evaluate(expression: Expression): Num =>
    match expression
    | let num: Num =>
      num
    | let operation: Operation =>
      let left = _evaluate(operation.left)
      let right = _evaluate(operation.right)
      match operation.operator
      | Add => left + right
      | Multiply => left * right
      end
    end
