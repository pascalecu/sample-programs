import Foundation

let usage = """
    Usage: please provide a string that contains at least one palindrome
    """

extension Collection {
    subscript(safe index: Index) -> Element? {
        indices.contains(index) ? self[index] : nil
    }
}

func findLongestPalindrome(_ s: String) -> String {
    let originalChars = Array(s)
    guard !originalChars.isEmpty else { return "" }

    let processed: [Character] = ["#"] + originalChars.flatMap { [$0, "#"] }

    let n = processed.count
    var radii = Array(repeating: 0, count: n)
    var center = 0
    var rightBoundary = 0

    var maxRadius = 0
    var centerOfMax = 0

    for i in 0..<n {
        if i < rightBoundary {
            let mirror = 2 * center - i
            radii[i] = min(rightBoundary - i, radii[mirror])
        }

        var left = i - (1 + radii[i])
        var right = i + (1 + radii[i])

        while left >= 0 && right < n, processed[left] == processed[right] {
            radii[i] += 1
            left -= 1
            right += 1
        }

        if i + radii[i] > rightBoundary {
            center = i
            rightBoundary = i + radii[i]
        }

        if radii[i] > maxRadius {
            maxRadius = radii[i]
            centerOfMax = i
        }
    }

    guard maxRadius > 1 else { return "" }

    let start = (centerOfMax - maxRadius) / 2
    let resultRange = start..<(start + maxRadius)

    return String(originalChars[resultRange])
}

guard let input = CommandLine.arguments[safe: 1], !input.isEmpty else {
    print(usage)
    exit(1)
}

let result = findLongestPalindrome(input)

if result.isEmpty {
    print(usage)
    exit(1)
} else {
    print(result)
}
