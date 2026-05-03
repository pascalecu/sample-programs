import Foundation

let usage = """
    Usage: please provide a list of integers ("1, 4, 5, 11, 12") and the integer to find ("11")
    """

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

extension Collection {
    subscript(safe index: Index) -> Element? {
        indices.contains(index) ? self[index] : nil
    }
}

extension Collection where Element: Equatable {
    func linearSearch(for target: Element) -> Index? {
        firstIndex(of: target)
    }
}

func parseInput(_ args: [String]) -> ([Int], Int)? {
    guard
        let rawList = args[safe: 1],
        let rawTarget = args[safe: 2],
        let target = Int(rawTarget)
    else { return nil }

    let values =
        rawList
        .split(separator: ",")
        .compactMap { Int($0.trimmed) }

    return values.isEmpty ? nil : (values, target)
}

guard let (values, target) = parseInput(CommandLine.arguments) else {
    print(usage)
    exit(1)
}

print(values.linearSearch(for: target) != nil)
