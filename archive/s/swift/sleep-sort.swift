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

func sleepSorted(_ numbers: [Int]) -> [Int] {
    let group = DispatchGroup()
    let queue = DispatchQueue(label: "sleep-sort", attributes: .concurrent)
    let lock = NSLock()

    var result: [Int] = []

    let minValue = numbers.min() ?? 0
    let offset = minValue < 0 ? abs(minValue) : 0

    for number in numbers {
        group.enter()
        queue.async {
            let delay = UInt32((number + offset) * 500_000)
            usleep(delay)

            lock.lock()
            result.append(number)
            lock.unlock()

            group.leave()
        }
    }

    group.wait()
    return result
}

guard let numbers = parseIntegers(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

let sorted = sleepSorted(numbers)
print(sorted.map(String.init).joined(separator: ", "))
