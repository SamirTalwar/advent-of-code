use "collections"
use "itertools"
use "regex"

class val Passport is Stringable
  let _fields: Map[String, String] val
  let _hgt_cm_parser: Regex = Regex("^(\\d+)cm$")?
  let _hgt_in_parser: Regex = Regex("^(\\d+)in$")?
  let _hcl_parser: Regex = Regex("^#[0-9a-f]{6}$")?
  let _ecl_parser: Regex = Regex("^(amb|blu|brn|gry|grn|hzl|oth)$")?
  let _pid_parser: Regex = Regex("^\\d{9}$")?

  new val create(fields: Array[(String, String)] val) ? =>
    _fields = _map_of(fields)

  fun val is_valid(): Bool =>
    let byr: Bool = _year_between(1920, 2002, "byr")
    let iyr: Bool = _year_between(2010, 2020, "iyr")
    let eyr: Bool = _year_between(2020, 2030, "eyr")
    let hgt: Bool = match _fields.get_or_else("hgt", "")
    | "" => false
    | let str: String => try
        let matched = _hgt_cm_parser(str)?
        let height = matched(1)?.usize()?
        (height >= 150) and (height <= 193)
      else
        try
          let matched = _hgt_in_parser(str)?
          let height = matched(1)?.usize()?
          (height >= 59) and (height <= 76)
        else
          false
        end
      end
    end
    let hcl: Bool = _matches(_hcl_parser, "hcl")
    let ecl: Bool = _matches(_ecl_parser, "ecl")
    let pid: Bool = _matches(_pid_parser, "pid")
    byr and iyr and eyr and hgt and hcl and ecl and pid

  fun box _matches(parser: Regex val, field_name: String): Bool =>
    match _fields.get_or_else(field_name, "")
    | "" => false
    | let value: String => parser.eq(value)
    end

  fun box _year_between(first: USize, last: USize, field_name: String): Bool =>
    match _fields.get_or_else(field_name, "")
    | "" => false
    | let year_string: String =>
      try
        let year = year_string.usize()?
        (year >= first) and (year <= last)
      else
        false
      end
    end

  fun tag _map_of(array: Array[(String, String)] val): Map[String, String] val =>
    recover
      var map = Map[String, String](array.size())
      for (name, value) in array.values() do
        map.insert(name, value)
      end
      map
    end

  fun box string(): String iso^ =>
    " ".join(Iter[(String, String)](_fields.pairs()).map[String]({ (pair) => pair._1 + ":" + pair._2 }))

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = MultipleLineCollector[Passport](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[Passport] val](collector, solution)

class Parser is MultipleLineParser[Passport]
  fun parse(lines: Array[String] val): Passport ? =>
    let fields = recover
      Iter[String](lines.values())
        .flat_map[String]({ (line) => line.split_by(" ").values() })
        .map[(String, String)]({ (field) ? =>
          let split = field.split_by(":")
          (split(0)?, split(1)?)
        })
        .collect(Array[(String, String)])
    end
    Passport(consume fields)?

actor Solution is Solve[Array[Passport] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be apply(passports: Array[Passport] val) =>
    let result = Iter[Passport](passports.values())
      .filter({ (passport) => passport.is_valid() })
      .count()
    _answer.answer(result)
