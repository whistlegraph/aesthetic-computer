import Foundation

enum ShellRunner {
    @discardableResult
    static func run(_ path: String, args: [String], timeout: TimeInterval? = nil) -> (status: Int32, output: String) {
        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: path)
        proc.arguments = args

        let outPipe = Pipe()
        let errPipe = Pipe()
        proc.standardOutput = outPipe
        proc.standardError = errPipe

        do {
            try proc.run()
        } catch {
            return (-1, "")
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

    /// Run a command whose output is intentionally not consumed. Sending both
    /// streams to /dev/null avoids the classic pipe-buffer deadlock where a
    /// verbose long-lived child blocks before `waitUntilExit()` can return.
    @discardableResult
    static func runQuiet(_ path: String, args: [String], cwd: String? = nil) -> Int32 {
        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: path)
        proc.arguments = args
        if let cwd, !cwd.isEmpty {
            proc.currentDirectoryURL = URL(fileURLWithPath: cwd, isDirectory: true)
        }
        proc.standardOutput = FileHandle.nullDevice
        proc.standardError = FileHandle.nullDevice
        do { try proc.run() } catch { return -1 }
        proc.waitUntilExit()
        return proc.terminationStatus
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
