import Foundation

let usage = """
    Usage: please provide a list of sorted integers ("1, 4, 5, 11, 12") and the integer to find ("11")
    """

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

extension Collection where Element: Comparable {
    var isSorted: Bool { zip(self, dropFirst()).allSatisfy(<=) }
}

extension Collection {
    subscript(safe index: Index) -> Element? {
        indices.contains(index) ? self[index] : nil
    }
}

extension RandomAccessCollection where Element: Comparable {
    func binarySearch(for target: Element) -> Index? {
        var low = startIndex
        var high = index(before: endIndex)

        while low <= high {
            let distance = self.distance(from: low, to: high)
            let mid = index(low, offsetBy: distance / 2)

            let value = self[mid]

            if value == target { return mid }
            if value < target {
                low = index(after: mid)
            } else {
                high = index(before: mid)
            }
        }

        return nil
    }
}

func parseInput(_ args: [String]) -> ([Int], Int)? {
    guard
        let rawList = args[safe: 1],
        let rawTarget = args[safe: 2],
        let target = Int(rawTarget)
    else { return nil }

    let values =
        rawList
        .split(separator: ",")
        .compactMap { Int($0.trimmed) }

    guard !values.isEmpty, values.isSorted else {
        return nil
    }

    return (values, target)
}

guard let (values, target) = parseInput(CommandLine.arguments) else {
    print(usage)
    exit(1)
}

print(values.binarySearch(for: target) != nil)
