import Foundation

let usage = "Usage: please provide a comma-separated list of integers"

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

func parseMatrix(_ input: String?) -> [[Int]]? {
    guard
        let input = input,
        !input.isEmpty
    else { return nil }

    let values =
        input
        .split(separator: ",")
        .map { Int($0.trimmed) }

    guard values.allSatisfy({ $0 != nil }) else { return nil }
    let flat = values.compactMap { $0 }

    let n = Int(Double(flat.count).squareRoot())
    guard n * n == flat.count else { return nil }

    return stride(from: 0, to: flat.count, by: n)
        .map { Array(flat[$0..<$0 + n]) }
}

func minimumSpanningTreeCost(_ matrix: [[Int]]) -> Int {
    let n = matrix.count
    guard n > 0 else { return 0 }

    var visited = Array(repeating: false, count: n)
    var minEdge = Array(repeating: Int.max, count: n)
    minEdge[0] = 0

    var total = 0

    for _ in 0..<n {
        guard
            let u = (0..<n)
                .filter({ !visited[$0] })
                .min(by: { minEdge[$0] < minEdge[$1] }),
            minEdge[u] != Int.max
        else { break }

        visited[u] = true
        total += minEdge[u]

        for (v, weight) in matrix[u].enumerated()
        where weight > 0 && !visited[v] && weight < minEdge[v] {
            minEdge[v] = weight
        }
    }

    return total
}

guard
    let matrix = parseMatrix(CommandLine.arguments.dropFirst().first)
else {
    print(usage)
    exit(1)
}

print(minimumSpanningTreeCost(matrix))
