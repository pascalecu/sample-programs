import Foundation

let args = CommandLine.arguments.dropFirst()

let sentence = args
    .joined(separator: " ")
    .trimmingCharacters(in: .whitespacesAndNewlines)

guard !sentence.isEmpty else {
    print("Usage: please provide a string")
    exit(1)
}

let result = sentence.prefix(1).uppercased() + sentence.dropFirst()

print(result)