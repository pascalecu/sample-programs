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
    mutating func selectionSort(by areInIncreasingOrder: (Element, Element) -> Bool) {
        guard count > 1 else { return }

        var i = startIndex

        while i != index(before: endIndex) {
            var minIndex = i
            var j = index(after: i)

            while j != endIndex {
                if areInIncreasingOrder(self[j], self[minIndex]) {
                    minIndex = j
                }
                j = index(after: j)
            }

            if minIndex != i {
                swapAt(i, minIndex)
            }

            i = index(after: i)
        }
    }
}

guard var numbers = parseIntegers(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

numbers.selectionSort(by: <)

print(numbers.map(String.init).joined(separator: ", "))
