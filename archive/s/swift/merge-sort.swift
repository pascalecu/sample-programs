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
where Self: MutableCollection, Self: RandomAccessCollection, Element: Comparable {
    mutating func mergeSort(by areInIncreasingOrder: (Element, Element) -> Bool) {
        var buffer = Array(self)
        var temp = buffer

        mergeSort(&buffer, &temp, start: 0, end: buffer.count, by: areInIncreasingOrder)

        self = Self(buffer)
    }

    private func mergeSort(
        _ buffer: inout [Element],
        _ temp: inout [Element],
        start: Int,
        end: Int,
        by areInIncreasingOrder: (Element, Element) -> Bool
    ) {
        guard end - start > 1 else { return }

        let mid = (start + end) / 2

        mergeSort(&buffer, &temp, start: start, end: mid, by: areInIncreasingOrder)
        mergeSort(&buffer, &temp, start: mid, end: end, by: areInIncreasingOrder)

        merge(&buffer, &temp, start: start, mid: mid, end: end, by: areInIncreasingOrder)
    }

    private func merge(
        _ buffer: inout [Element],
        _ temp: inout [Element],
        start: Int,
        mid: Int,
        end: Int,
        by areInIncreasingOrder: (Element, Element) -> Bool
    ) {
        var i = start
        var j = mid
        var k = start

        while i < mid && j < end {
            if areInIncreasingOrder(buffer[i], buffer[j]) {
                temp[k] = buffer[i]
                i += 1
            } else {
                temp[k] = buffer[j]
                j += 1
            }
            k += 1
        }

        while i < mid {
            temp[k] = buffer[i]
            i += 1
            k += 1
        }

        while j < end {
            temp[k] = buffer[j]
            j += 1
            k += 1
        }

        for index in start..<end {
            buffer[index] = temp[index]
        }
    }
}

guard var numbers = parseIntegers(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

numbers.mergeSort(by: <)

print(numbers.map(String.init).joined(separator: ", "))
