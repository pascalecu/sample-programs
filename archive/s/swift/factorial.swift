import Foundation

let usage = """
    Usage: please input a non-negative integer
    """

extension FixedWidthInteger {
    var factorial: Self? {
        guard self >= 0 else { return nil }
        guard self > 1 else { return 1 }

        var result: Self = 1

        for i in 2...self {
            let (value, overflow) = result.multipliedReportingOverflow(by: i)
            if overflow { return nil }
            result = value
        }

        return result
    }
}

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

guard
    let raw = CommandLine.arguments.dropFirst().first?.trimmed,
    let n = Int(raw),
    let result = n.factorial
else {
    print(usage)
    exit(1)
}

print(result)
