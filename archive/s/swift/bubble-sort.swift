import Foundation

let usage = """
    Usage: please provide a list of at least two integers to sort in the format "1, 2, 3, 4, 5"
    """

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

func parseIntegers(from args: [String]) -> [Int]? {
    guard args.count == 2 else { return nil }

    let parts = args[1].split(separator: ",", omittingEmptySubsequences: false)
    let values = parts.compactMap { Int($0.trimmed) }

    guard values.count == parts.count,
        values.count >= 2
    else {
        return nil
    }

    return values
}

extension MutableCollection where Self: BidirectionalCollection {
    mutating func bubbleSort(by areInIncreasingOrder: (Element, Element) -> Bool) {
        guard count > 1 else { return }

        var end = index(before: endIndex)

        while end > startIndex {
            var lastSwap = startIndex
            var current = startIndex

            while current < end {
                let next = index(after: current)

                if areInIncreasingOrder(self[next], self[current]) {
                    swapAt(current, next)
                    lastSwap = next
                }

                current = next
            }

            end = lastSwap
        }
    }
}

guard var numbers = parseIntegers(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

numbers.bubbleSort(by: <)

print(numbers.map(String.init).joined(separator: ", "))
