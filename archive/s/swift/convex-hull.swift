import Foundation

let usage = """
    Usage: please provide at least 3 x and y coordinates as separate lists (e.g. "100, 440, 210")
    """

struct Point: Comparable {
    let x: Int
    let y: Int

    static func < (lhs: Point, rhs: Point) -> Bool {
        return lhs.x < rhs.x || (lhs.x == rhs.x && lhs.y < rhs.y)
    }
}

enum Orientation {
    case clockwise, counterClockwise, collinear
}

func orientation(_ a: Point, _ b: Point, _ c: Point) -> Orientation {
    let value = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)

    if value == 0 { return .collinear }
    return value > 0 ? .counterClockwise : .clockwise
}

func convexHull(from points: [Point]) -> [Point] {
    guard points.count > 2 else { return points }

    let sorted = points.sorted()

    func buildHalfHull(_ points: some Sequence<Point>) -> [Point] {
        var hull: [Point] = []
        hull.reserveCapacity(points.underestimatedCount)

        for p in points {
            while hull.count >= 2
                && orientation(hull[hull.count - 2], hull[hull.count - 1], p)
                    != .counterClockwise
            {
                hull.removeLast()
            }
            hull.append(p)
        }

        return hull
    }

    let lower = buildHalfHull(sorted)
    let upper = buildHalfHull(sorted.reversed())

    return lower.dropLast() + upper.dropLast()
}

extension StringProtocol {
    var trimmed: String { trimmingCharacters(in: .whitespacesAndNewlines) }
}

func parseList(_ input: String) -> [Int]? {
    let parts = input.split(separator: ",")
    let values = parts.map { Int($0.trimmed) }

    guard values.count >= 3,
        values.allSatisfy({ $0 != nil })
    else {
        return nil
    }

    return values.compactMap { $0 }
}

func parsePoints(from args: [String]) -> [Point]? {
    guard args.count == 3,
        let xs = parseList(args[1]),
        let ys = parseList(args[2]),
        xs.count == ys.count
    else { return nil }

    return zip(xs, ys).map(Point.init)
}

guard let points = parsePoints(from: CommandLine.arguments) else {
    print(usage)
    exit(1)
}

print(convexHull(from: points).map { "(\($0.x), \($0.y))" }.joined(separator: "\n"))
