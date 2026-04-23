import Foundation

func fibs(upTo limit: Int) -> [Int] {
    guard limit >= 1 else { return [] }

    var result = [1, 2]
    while true {
        let next = result[result.count - 1] + result[result.count - 2]
        if next > limit { break }
        result.append(next)
    }
    return result
}

func zeckendorf(_ n: Int) -> String {
    guard n > 0 else { return "" }

    let fibsList = fibs(upTo: n).reversed()

    let result = fibsList.reduce(into: (remaining: n, out: [Int]())) { acc, f in
        if f <= acc.remaining {
            acc.out.append(f)
            acc.remaining -= f
        }
    }.out

    return result.map(String.init).joined(separator: ", ")
}

guard CommandLine.argc == 2,
      let n = Int(CommandLine.arguments[1]),
      n >= 0 else {
    print("Usage: please input a non-negative integer")
    exit(0)
}

print(zeckendorf(16383))