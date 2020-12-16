use "collections"
use "itertools"

type AnsweredYes is U8

class val CustomsDeclarationForm
  let people: Array[Array[AnsweredYes] val] val

  new val create(people': Array[Array[AnsweredYes] val] val) =>
    people = people'

  fun count(): USize =>
    var all = Set[AnsweredYes]
    for person in people.values() do
      for answer in person.values() do
        all = all.add(answer)
      end
    end
    all.size()

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = MultipleLineCollector[CustomsDeclarationForm](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[CustomsDeclarationForm] val](collector, solution)

class Parser is MultipleItemParser[CustomsDeclarationForm]
  fun parse(lines: Array[String] val): CustomsDeclarationForm =>
    let people = recover
      Iter[String](lines.values())
        .map[Array[AnsweredYes] val]({ (line) => recover line.array() end })
        .collect(Array[Array[AnsweredYes] val])
    end
    CustomsDeclarationForm(consume people)

actor Solution is Solve[Array[CustomsDeclarationForm val] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be apply(forms: Array[CustomsDeclarationForm] val) =>
    let result = Iter[CustomsDeclarationForm](forms.values())
      .map[USize]({ (form) => form.count() })
      .fold[USize](0, { (sum, count) => sum + count })
    _answer.answer(result)
