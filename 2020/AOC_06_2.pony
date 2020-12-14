use "collections/persistent"
use "itertools"

type AnsweredYes is U8

class val CustomsDeclarationForm
  let people: Array[Set[AnsweredYes]] val

  new val create(people': Array[Set[AnsweredYes]] val) =>
    people = people'

  fun count(): USize =>
    Fold[Set[AnsweredYes]](people.values())
      .through1(Set[AnsweredYes], { (intersection, person) =>
        intersection and person
      })
      .size()

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = MultipleLineCollector[CustomsDeclarationForm](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[CustomsDeclarationForm] val](collector, solution)

class Parser is MultipleLineParser[CustomsDeclarationForm]
  fun parse(lines: Array[String] val): CustomsDeclarationForm =>
    let people = recover
      Iter[String](lines.values())
        .map[Set[AnsweredYes]]({ (line) =>
          ToSet[AnsweredYes].from_iterator(line.values())
        })
        .collect(Array[Set[AnsweredYes]])
    end
    CustomsDeclarationForm(consume people)

actor Solution is Solve[Array[CustomsDeclarationForm] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be apply(forms: Array[CustomsDeclarationForm] val) =>
    let result = Iter[CustomsDeclarationForm](forms.values())
      .map[USize]({ (form) => form.count() })
      .fold[USize](0, { (sum, count) => sum + count })
    _answer.answer(result)
