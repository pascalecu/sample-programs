import Foundation

let usage = """
    Usage: please provide a list of integers (e.g. "8, 3, 1, 2")
    """

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

func parseList(_ input: String?) -> [Int]? {
    guard let input, !input.isEmpty else { return nil }

    let values =
        input
        .split(separator: ",")
        .map { $0.trimmed }
        .compactMap(Int.init)

    return values.isEmpty ? nil : values
}

func maximumArrayRotation(_ nums: [Int]) -> Int {
    let n = nums.count
    let totalSum = nums.reduce(0, +)

    var current = nums.enumerated().reduce(0) { $0 + $1.element * $1.offset }
    var best = current

    var suffix = nums

    for _ in 1..<n {
        let last = suffix.removeLast()
        suffix.insert(last, at: 0)

        current += totalSum - n * last
        best = max(best, current)
    }

    return best
}

let args = CommandLine.arguments

guard
    args.count == 2,
    let input = args.dropFirst().first,
    let numbers = parseList(input)
else {
    print(usage)
    exit(1)
}

print(maximumArrayRotation(numbers))
