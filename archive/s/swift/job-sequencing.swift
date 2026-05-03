import Foundation

let usage = """
    Usage: please provide a list of profits and a list of deadlines
    """

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

func sequenceJobs(profits: [Int], deadlines: [Int]) -> Int {
    let jobs = zip(profits, deadlines)
        .sorted { $0.0 > $1.0 }

    guard let maxDeadline = deadlines.max() else { return 0 }

    var slots = Array(repeating: false, count: maxDeadline + 1)
    var totalProfit = 0

    for (profit, deadline) in jobs {
        let limit = min(deadline, maxDeadline)
        for slot in stride(from: limit, through: 1, by: -1) where !slots[slot] {
            slots[slot] = true
            totalProfit += profit
            break
        }
    }

    return totalProfit
}

func parseList(_ input: String) -> [Int]? {
    let parts =
        input
        .split(separator: ",")
        .map { $0.trimmed }

    let values = parts.compactMap(Int.init)

    guard values.count == parts.count else { return nil }
    return values
}

let args = CommandLine.arguments

guard args.count == 3,
    let profits = parseList(args[1]),
    let deadlines = parseList(args[2]),
    profits.count == deadlines.count
else {
    print(usage)
    exit(1)
}

print(sequenceJobs(profits: profits, deadlines: deadlines))
