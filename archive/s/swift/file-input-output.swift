import Foundation

let url = URL(fileURLWithPath: "output.txt")
let text = "Line 1\nLine 2\nLine 3"

// Write: Create file and write content
try? text.write(to: url, atomically: true, encoding: .utf8)

// Read: Print file content directly
if let content = try? String(contentsOf: url, encoding: .utf8) {
    print(content)
}
