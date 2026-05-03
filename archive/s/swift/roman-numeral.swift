import Foundation

let romanValues: [Character: Int] = [
    "I": 1,
    "V": 5,
    "X": 10,
    "L": 50,
    "C": 100,
    "D": 500,
    "M": 1000,
]

let usage = "Usage: please provide a string of roman numerals"
let error = "Error: invalid string of roman numerals"

func romanToInt(_ s: String) -> Int? {
    var total = 0
    var previous = 0

    for char in s.uppercased().reversed() {
        guard let value = romanValues[char] else { return nil }

        if value < previous {
            total -= value
        } else {
            total += value
            previous = value
        }
    }

    return total
}

guard let input = CommandLine.arguments.dropFirst().first else {
    print(usage)
    exit(1)
}

guard let result = romanToInt(input) else {
    print(error)
    exit(1)
}

print(result)
