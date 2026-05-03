import Foundation

let usage = "Usage: please input the total number of people and number of people to skip."

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

extension Array {
    func argument(at index: Int) -> Element? {
        indices.contains(index) ? self[index] : nil
    }
}

func solveJosephus(_ n: Int, _ k: Int) -> Int {
    guard n > 0 else { return 0 }

    if k == 2 {
        let mostSignificantBit = 1 << (Int.bitWidth - 1 - n.leadingZeroBitCount)
        return 2 * (n - mostSignificantBit) + 1
    }

    return (2...n).reduce(0) { (survivor, i) in
        (survivor + k) % i
    } + 1
}

func parseInput(from args: [String]) -> (n: Int, k: Int)? {
    guard let rawN = args.argument(at: 1),
        let rawK = args.argument(at: 2),
        let n = Int(rawN.trimmed),
        let k = Int(rawK.trimmed),
        n > 0, k > 0
    else { return nil }

    return (n, k)
}

guard let (n, k) = parseInput(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

print(solveJosephus(n, k))
