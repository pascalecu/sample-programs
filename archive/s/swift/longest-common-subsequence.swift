import Foundation

let usage = """
    Usage: please provide two lists in the format "1, 2, 3, 4, 5"
    """

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
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

func parseInput(_ args: [String]) -> ([Int], [Int])? {
    guard
        let a = args.dropFirst().first,
        let b = args.dropFirst().dropFirst().first,
        let listA = parseList(a),
        let listB = parseList(b)
    else {
        return nil
    }

    return (listA, listB)
}

func longestCommonSubsequence(_ a: [Int], _ b: [Int]) -> [Int] {
    let n = a.count
    let m = b.count

    var dp = Array(
        repeating: Array(repeating: 0, count: m + 1),
        count: n + 1
    )

    for i in 1...n {
        for j in 1...m {
            if a[i - 1] == b[j - 1] {
                dp[i][j] = dp[i - 1][j - 1] + 1
            } else {
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
            }
        }
    }

    var result: [Int] = []
    var i = n
    var j = m

    while i > 0 && j > 0 {
        if a[i - 1] == b[j - 1] {
            result.append(a[i - 1])
            i -= 1
            j -= 1
        } else if dp[i - 1][j] > dp[i][j - 1] {
            i -= 1
        } else {
            j -= 1
        }
    }

    return result.reversed()
}

guard let (a, b) = parseInput(CommandLine.arguments) else {
    print(usage)
    exit(1)
}

let lcs = longestCommonSubsequence(a, b)
print(lcs.map(String.init).joined(separator: ", "))
