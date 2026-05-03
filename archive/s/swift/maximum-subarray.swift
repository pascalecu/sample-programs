import Foundation

let usage = """
    Usage: Please provide a list of integers in the format: "1, 2, 3, 4, 5"
    """

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

extension Collection {
    subscript(safe index: Index) -> Element? {
        indices.contains(index) ? self[index] : nil
    }
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

func maxSubarraySum(_ numbers: [Int]) -> Int {
    var best = numbers[0]
    var current = numbers[0]

    for value in numbers.dropFirst() {
        current = max(value, current + value)
        best = max(best, current)
    }

    return best
}

let args = CommandLine.arguments

guard let list = parseList(args[safe: 1]) else {
    print(usage)
    exit(1)
}

let result = maxSubarraySum(list)
print(result)
