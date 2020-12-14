use collections = "collections"
use "itertools"

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = LineCollector[USize](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[USize] val](collector, solution)

class Parser is LineParser[USize]
  fun parse(line: String): USize ? =>
    line.usize()?

actor Solution is Solve[Array[USize] val]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(numbers: Array[USize] val) =>
    let numbers_ref = Array[USize](numbers.size() + 1)
    numbers_ref.push(0)
    numbers.copy_to(numbers_ref, 0, 1, numbers.size())
    let sorted = collections.Sort[Array[USize], USize](numbers_ref)
    let cache = collections.Map[USize, USize]
    try
      _answer.answer(count_paths(sorted, sorted.size() - 1, cache)?)
    else
      _answer.fail("Failed.")
    end

  fun count_paths(numbers: Array[USize], i: USize, cache: collections.Map[USize, USize]): USize ? =>
    if cache.contains(i) then
      cache(i)?
    elseif i == 0 then
      1
    else
      let element = numbers(i)?
      var j = i - 1
      var count: USize = 0
      while (element - numbers(j)?) <= 3 do
        count = count + count_paths(numbers, j, cache)?
        if j == 0 then
          break  // Underflow means we can't check if it goes below zero.
        end
        j = j - 1
      end
      cache(i) = count
      count
    end
