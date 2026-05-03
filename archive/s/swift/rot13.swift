import Foundation

let usage = "Usage: please provide a string to encrypt"

extension StringProtocol {
    var rot13: String {
        String(
            unicodeScalars.map { scalar in
                switch scalar {
                case "A"..."M", "a"..."m":
                    return Character(UnicodeScalar(scalar.value + 13)!)
                case "N"..."Z", "n"..."z":
                    return Character(UnicodeScalar(scalar.value - 13)!)
                default:
                    return Character(scalar)
                }
            }
        )
    }
}

guard
    let input = CommandLine.arguments.dropFirst().first,
    !input.isEmpty
else {
    print(usage)
    exit(1)
}

print(input.rot13)
