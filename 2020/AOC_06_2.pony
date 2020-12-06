use "collections/persistent"
use "itertools"

type AnsweredYes is U8

class val CustomsDeclarationForm
  let people: Array[Set[AnsweredYes]] val

  new val create(people': Array[Set[AnsweredYes]] val) =>
    people = people'

  fun count(): USize =>
    var intersection: (Set[AnsweredYes] | None) = None
    for person in people.values() do
      intersection = match intersection
      | None => person
      | let i: Set[AnsweredYes] => i and person
      end
    end
    match intersection
    | None => 0
    | let i: Set[AnsweredYes] => i.size()
    end

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    orchestrator.start(
      MultipleLineCollector[CustomsDeclarationForm](
        Solution(orchestrator),
        orchestrator,
        Parser
      )
    )

class Parser is MultipleLineParser[CustomsDeclarationForm]
  fun parse(lines: Array[String] val): CustomsDeclarationForm =>
    let people = recover
      Iter[String](lines.values())
        .map[Set[AnsweredYes]]({ (line) =>
          Iter[AnsweredYes](line.values()).fold[Set[AnsweredYes]](Set[AnsweredYes], { (answers, answer) =>
            answers.add(answer)
          })
        })
        .collect(Array[Set[AnsweredYes]])
    end
    CustomsDeclarationForm(consume people)

actor Solution is ASolution[Array[CustomsDeclarationForm] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be solve(forms: Array[CustomsDeclarationForm] val) =>
    let result = Iter[CustomsDeclarationForm](forms.values())
      .map[USize]({ (form) => form.count() })
      .fold[USize](0, { (sum, count) => sum + count })
    _answer.answer(result)
