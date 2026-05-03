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

extension Array where Element == Int {
    func sleepSorted() async -> [Element] {
        let minValue = self.min() ?? 0
        let offset = minValue < 0 ? abs(minValue) : 0

        return await withTaskGroup(of: Int.self) { group in
            for element in self {
                group.addTask {
                    let nanoseconds = UInt64(element + offset) * 1_000_000
                    try? await Task.sleep(nanoseconds: nanoseconds)
                    return element
                }
            }

            var result: [Element] = []
            for await sortedElement in group {
                result.append(sortedElement)
            }
            return result
        }
    }
}

guard let numbers = parseIntegers(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

let sorted = await numbers.sleepSorted()
print(sorted.map(String.init).joined(separator: ", "))
