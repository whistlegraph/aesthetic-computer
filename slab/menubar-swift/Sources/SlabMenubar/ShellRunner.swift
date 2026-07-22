import Foundation

enum ShellRunner {
    @discardableResult
    static func run(_ path: String, args: [String], timeout: TimeInterval? = nil,
                    input: String? = nil) -> (status: Int32, output: String) {
        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: path)
        proc.arguments = args

        let outPipe = Pipe()
        let errPipe = Pipe()
        proc.standardOutput = outPipe
        proc.standardError = errPipe
        let inPipe: Pipe? = input == nil ? nil : Pipe()
        if let inPipe { proc.standardInput = inPipe }

        do {
            try proc.run()
        } catch {
            return (-1, "")
        }

        // Feeding prompts over stdin keeps arbitrary session text out of a
        // shell command line. Close immediately so consumers waiting for EOF
        // (notably `claude -p`) can begin inference.
        if let input, let inPipe {
            inPipe.fileHandleForWriting.write(Data(input.utf8))
            try? inPipe.fileHandleForWriting.close()
        }

        var timedOut = false
        if let timeout = timeout {
            DispatchQueue.global().asyncAfter(deadline: .now() + timeout) {
                if proc.isRunning {
                    timedOut = true
                    proc.terminate()
                }
            }
        }

        proc.waitUntilExit()
        let data = outPipe.fileHandleForReading.readDataToEndOfFile()
        _ = errPipe.fileHandleForReading.readDataToEndOfFile()
        let status = timedOut ? Int32(-2) : proc.terminationStatus
        return (status, String(data: data, encoding: .utf8) ?? "")
    }

    static func output(_ path: String, args: [String], timeout: TimeInterval? = nil) -> String? {
        let result = run(path, args: args, timeout: timeout)
        return result.status == 0 ? result.output : nil
    }

    static func runAsync(_ path: String, args: [String], completion: (() -> Void)? = nil) {
        DispatchQueue.global(qos: .utility).async {
            _ = run(path, args: args)
            completion?()
        }
    }

    static func runShellAsync(_ command: String, completion: (() -> Void)? = nil) {
        runAsync("/bin/sh", args: ["-c", command], completion: completion)
    }
}
