use "collections"
use "itertools"

class val Passport is Stringable
  let _expected_fields: Set[String] val = _set_of(recover val [
    "byr"
    "ecl"
    "eyr"
    "hcl"
    "hgt"
    "iyr"
    "pid"
  ] end)
  let _field_names: Set[String] val

  new val create(field_names: Array[String] val) =>
    _field_names = _set_of(field_names)

  fun val is_valid(): Bool =>
    Iter[String](_expected_fields.values())
      .all({ (field) => _field_names.contains(field) })

  fun tag _set_of(array: Array[String] val): Set[String] val =>
    recover
      var set = Set[String](array.size())
      for item in array.values() do
        set = (consume set).add(item)
      end
      set
    end

  fun box string(): String iso^ =>
    " ".join(_field_names.values())

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    orchestrator.start(
      MultipleLineCollector[Passport](
        Solution(orchestrator),
        orchestrator,
        Parser
      )
    )

class Parser is MultipleLineParser[Passport]
  fun parse(lines: Array[String] val): Passport =>
    let field_names = recover
      Iter[String](lines.values())
        .flat_map[String]({ (line) => line.split_by(" ").values() })
        .map[String]({ (field) ? => field.split_by(":")(0)? })
        .collect(Array[String])
    end
    Passport(consume field_names)

actor Solution is ASolution[Array[Passport] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be solve(passports: Array[Passport] val) =>
    let result = Iter[Passport](passports.values())
      .filter({ (passport) => passport.is_valid() })
      .count()
    _answer.answer(result)
