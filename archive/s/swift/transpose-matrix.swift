import Foundation

let usage = "Usage: please enter the dimension of the matrix and the serialized matrix"

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

func parseInt(_ s: String?) -> Int? {
    guard let s = s else { return nil }
    return Int(s.trimmed)
}

func parseMatrix(_ input: String, rows: Int, cols: Int) -> [Int]? {
    let values = input
        .split(separator: ",")
        .compactMap { Int($0.trimmed) }

    guard values.count == rows * cols else { return nil }
    return values
}

func transpose(_ matrix: [Int], rows: Int, cols: Int) -> [Int] {
    var result = [Int]()
    result.reserveCapacity(matrix.count)

    for c in 0..<cols {
        for r in 0..<rows {
            result.append(matrix[r * cols + c])
        }
    }

    return result
}

let args = CommandLine.arguments

guard
    args.count >= 4,
    let cols = parseInt(args[1]),
    let rows = parseInt(args[2]),
    let matrix = parseMatrix(args[3], rows: rows, cols: cols)
else {
    print(usage)
    exit(1)
}

let result = transpose(matrix, rows: rows, cols: cols)
print(result.map(String.init).joined(separator: ", "))