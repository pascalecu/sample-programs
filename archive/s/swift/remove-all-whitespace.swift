import Foundation

let usage = "Usage: please provide a string"

guard
    let input = CommandLine.arguments.dropFirst().first,
    !input.isEmpty
else {
    print(usage)
    exit(1)
}

print(input.filter { !$0.isWhitespace })
