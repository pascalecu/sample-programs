import Foundation

private enum Base64Processor {
    private static let alphabet = CharacterSet(charactersIn: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

    static func isValid(_ string: String) -> Bool {
        guard string.count % 4 == 0 else { return false }
        
        guard string.unicodeScalars.allSatisfy(alphabet.contains) else { return false }
        
        let paddingCount = string.reversed().prefix(while: { $0 == "=" }).count
        guard paddingCount <= 2 else { return false }
        
        let withoutPadding = string.dropLast(paddingCount)
        return !withoutPadding.contains("=")
    }
}

func usage() -> Never {
    print("Usage: please provide a mode and a string to encode/decode")
    exit(1)
}

let args = CommandLine.arguments

guard args.count == 3 else { usage() }

let mode = args[1]
let input = args[2]

guard !input.isEmpty else { usage() }

switch mode {
case "encode":
    guard let data = input.data(using: .ascii) else { usage() }
    print(data.base64EncodedString())
    
case "decode":
    guard Base64Processor.isValid(input),
          let data = Data(base64Encoded: input),
          let decoded = String(data: data, encoding: .ascii) else {
        usage()
    }
    print(decoded)
    
default:
    usage()
}