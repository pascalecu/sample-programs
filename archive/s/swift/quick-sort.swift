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

extension RangeReplaceableCollection
where Self: MutableCollection, Self: RandomAccessCollection {

    mutating func quickSort(by areInIncreasingOrder: (Element, Element) -> Bool) {
        guard count > 1 else { return }

        var buffer = Array(self)
        quickSort(&buffer, low: 0, high: buffer.count - 1, by: areInIncreasingOrder)
        self = Self(buffer)
    }

    private func quickSort(
        _ a: inout [Element],
        low: Int,
        high: Int,
        by areInIncreasingOrder: (Element, Element) -> Bool
    ) {
        guard low < high else { return }

        let pivotIndex = partition(&a, low: low, high: high, by: areInIncreasingOrder)

        quickSort(&a, low: low, high: pivotIndex - 1, by: areInIncreasingOrder)
        quickSort(&a, low: pivotIndex + 1, high: high, by: areInIncreasingOrder)
    }

    private func partition(
        _ a: inout [Element],
        low: Int,
        high: Int,
        by areInIncreasingOrder: (Element, Element) -> Bool
    ) -> Int {
        let pivot = a[high]
        var i = low

        for j in low..<high {
            if areInIncreasingOrder(a[j], pivot) {
                a.swapAt(i, j)
                i += 1
            }
        }

        a.swapAt(i, high)
        return i
    }
}

guard var numbers = parseIntegers(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

numbers.quickSort(by: <)

print(numbers.map(String.init).joined(separator: ", "))
