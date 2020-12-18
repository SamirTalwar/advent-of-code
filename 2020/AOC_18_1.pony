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

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = LineCollector[Expression](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[Expression] val](collector, solution)

class Parser is SingleItemParser[Expression]
  fun parse(line: String): Expression ? =>
    // We reverse the line so we can cheat and use a left-biased parser.
    (let expression, let remainder) = _parse_sub_expression(line.reverse())?
    if remainder.size() > 0 then
      error
    end
    expression

  fun _parse_sub_expression(string: String): (Expression, String) ? =>
    // The string is backwards, so it's right before left.
    (let right: Expression, let remainder: String) =
      // And ')' before '('.
      if string(0)? == ')' then
        _parse_sub_expression(string.substring(1))?
      else
        _parse_number(string)?
      end
    if (remainder.size() == 0) then
      (right, remainder)
    elseif (remainder(0)? == '(') then
      let remainder' = remainder.substring(1)
      remainder'.strip()
      (right, remainder')
    else
      (let operator, let remainder') = _parse_operator(remainder)?
      (let left, let remainder'') = _parse_sub_expression(remainder')?
      (Operation(left, operator, right), remainder'')
    end

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
    // We reverse because the string is backwards.
    let number = string.substring(0, offset.isize()).reverse().isize()?
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
