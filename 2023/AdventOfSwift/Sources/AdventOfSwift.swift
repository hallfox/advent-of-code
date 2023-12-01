import ArgumentParser
import Foundation

@main
struct AdventOfSwift: ParsableCommand {
  @Option(help: "Specify the day")
  public var day: String

  public func run() throws {
    switch day {
        case "1": day01()
        default: print("no day specified")
    }
  }
}

func readStdin () -> String? {
    var input: String?

    while let line = readLine() {
        if input == nil {
            input = line
        } else {
            input! += "\n" + line
        }
    }

    return input
}

func solve(_ x: String) -> Int {
    Int("\(x.first!)\(x.last!)")!
}

func num(_ x: String) -> Int {
    switch x {
        case "one", "1": return 1
        case "two", "2": return 2
        case "three", "3": return 3
        case "four", "4": return 4
        case "five", "5": return 5
        case "six", "6": return 6
        case "seven", "7": return 7
        case "eight", "8": return 8
        case "nine", "9": return 9
        default: assertionFailure("impossible")
    }
    return -1
}

func solve2<S: StringProtocol>(_ x: S) -> Int {
    let nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    var i: String.Index?
    var j: String.Index?
    var fst: Int!
    var snd: Int!
    for n in nums {
        if let a = x.range(of: n), i == nil || a.lowerBound < i!  {
            i = a.lowerBound
            fst = num(n)
        }

        if let b = x.range(of: n, options: String.CompareOptions.backwards),
            j == nil || b.lowerBound > j! {
            j = b.lowerBound
            snd = num(n)
        }
    }
    return Int("\(fst!)\(snd!)")!
}

func day01() {
    let input = readStdin()!
    let elves = input.split(separator: "\n")
    let res: [String] = elves
        .map({ $0.filter({ $0.isNumber }) })
    let res2 = res
        .map(solve)
        .reduce(0, { $0 + $1 })
    print(res2)

    let res3 = elves
        .map({solve2($0)})
        .reduce(0, { $0 + $1 })
    print(res3)
}
