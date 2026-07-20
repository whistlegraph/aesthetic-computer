import ACMacAudio
import Darwin
import Foundation

/// Machine-aware policy over the shared audio transport. The wire protocol,
/// clock synchronization, process tap, and renderers remain in ACMacAudio.
final class JukeRoomAudio {
    enum Source: String {
        case spotify = "Spotify"
        case aesthetic = "Aesthetic"
    }

    enum Layout: Int, CaseIterable {
        case neoStereo, blueberryStereo, mirrorStereo, splitLR, splitRL, panMono

        var title: String {
            switch self {
            case .neoStereo: return "Neo · stereo"
            case .blueberryStereo: return "Blueberry · stereo"
            case .mirrorStereo: return "Both Macs · mirrored stereo"
            case .splitLR: return "Neo L · Blueberry R"
            case .splitRL: return "Neo R · Blueberry L"
            case .panMono: return "Pan · mono across Macs"
            }
        }
    }

    struct Snapshot: Equatable {
        let source: Source
        let layout: Layout
        let pan: Float
        let neo: String
        let blueberry: String
    }

    enum State: Equatable {
        case idle
        case live(Snapshot)
        case failed(String)
    }

    var onState: ((State) -> Void)?
    private(set) var state: State = .idle { didSet { onState?(state) } }
    private(set) var layout: Layout = .splitLR
    private(set) var pan: Float = 0
    private(set) var source: Source = .spotify

    private var sender: ACAudioRoomSender?
    private var localReceiver: ACAudioRoomReceiver?
    private var spotifyTap: AnyObject?
    private var remoteReceiver: Process?

    func useSource(_ nextSource: Source) {
        guard source != nextSource else { return }
        source = nextSource
        if case .live = state { apply(layout, pan: pan) }
    }

    func apply(_ nextLayout: Layout, pan nextPan: Float? = nil, remote: String = "blueberry") {
        layout = nextLayout
        if let nextPan { pan = max(-1, min(1, nextPan)) }
        stop(notify: false)

        if nextLayout == .neoStereo {
            state = .live(snapshot(for: nextLayout))
            return
        }

        do { try startDistributed(remote: remote) }
        catch { state = .failed(error.localizedDescription) }
    }

    private func startDistributed(remote: String) throws {
        guard #available(macOS 14.2, *) else {
            throw ACAudioRoomError.unavailable("Room audio requires macOS 14.2 or newer")
        }
        let pid: pid_t
        switch source {
        case .spotify:
            guard let daemonPID = Self.jukedPID() else {
                throw ACAudioRoomError.unavailable("juked is not running")
            }
            pid = daemonPID
        case .aesthetic:
            pid = getpid()
        }

        let mix = channels(for: layout)
        let sender = ACAudioRoomSender()
        sender.onLog = { NSLog("JukeWizard room sender: \($0)") }
        try sender.start()

        var local: ACAudioRoomReceiver?
        var remoteProcess: Process?
        do {
            if let localMix = mix.local {
                let receiver = ACAudioRoomReceiver(configuration: .init(
                    host: "127.0.0.1", name: "Neo", channel: localMix.channel, gain: localMix.gain))
                receiver.onLog = { NSLog("JukeWizard room Neo: \($0)") }
                try receiver.start()
                local = receiver
            }

            if let remoteMix = mix.remote {
                let process = Process()
                process.executableURL = URL(fileURLWithPath: "/usr/bin/ssh")
                let gain = String(format: "%.3f", remoteMix.gain)
                process.arguments = [
                    "-o", "BatchMode=yes", remote,
                    "/bin/bash -lc 'receiver=\"$HOME/.local/bin/ac-audio-room\"; " +
                    "/usr/bin/pkill -f \"^$receiver receive --host neo.local\" 2>/dev/null || true; " +
                    "exec \"$receiver\" receive --host neo.local --channel \(remoteMix.channel.name) --gain \(gain) --name Blueberry'",
                ]
                process.standardOutput = FileHandle.nullDevice
                process.standardError = FileHandle.nullDevice
                process.terminationHandler = { [weak self] process in
                    DispatchQueue.main.async {
                        guard let self, case .live = self.state else { return }
                        self.stop(notify: false)
                        self.state = .failed("Blueberry exited (status \(process.terminationStatus))")
                    }
                }
                try process.run()
                remoteProcess = process
            }

            let tap = ACProcessAudioTap(processID: pid, name: "JukeWizard \(source.rawValue)", muteOriginal: true)
            tap.onLog = { NSLog("JukeWizard room tap: \($0)") }
            try tap.start { sender.send($0) }

            self.sender = sender
            self.localReceiver = local
            self.remoteReceiver = remoteProcess
            self.spotifyTap = tap
            state = .live(snapshot(for: layout))
        } catch {
            if remoteProcess?.isRunning == true { remoteProcess?.terminate() }
            local?.stop(); sender.stop()
            throw error
        }
    }

    func stop() { stop(notify: true) }
    private func stop(notify: Bool) {
        if #available(macOS 14.2, *), let tap = spotifyTap as? ACProcessAudioTap { tap.stop() }
        spotifyTap = nil
        localReceiver?.stop(); localReceiver = nil
        sender?.stop(); sender = nil
        remoteReceiver?.terminationHandler = nil
        if remoteReceiver?.isRunning == true { remoteReceiver?.terminate() }
        remoteReceiver = nil
        if notify { state = .idle }
    }

    private struct Mix { let channel: ACRoomWire.Channel; let gain: Float }
    private func channels(for layout: Layout) -> (local: Mix?, remote: Mix?) {
        switch layout {
        case .neoStereo: return (nil, nil)
        case .blueberryStereo: return (nil, Mix(channel: .stereo, gain: 1))
        case .mirrorStereo: return (Mix(channel: .stereo, gain: 1), Mix(channel: .stereo, gain: 1))
        case .splitLR: return (Mix(channel: .left, gain: 1), Mix(channel: .right, gain: 1))
        case .splitRL: return (Mix(channel: .right, gain: 1), Mix(channel: .left, gain: 1))
        case .panMono:
            let theta = Double((pan + 1) * .pi / 4)
            return (Mix(channel: .mono, gain: Float(cos(theta))),
                    Mix(channel: .mono, gain: Float(sin(theta))))
        }
    }

    private func snapshot(for layout: Layout) -> Snapshot {
        let mix = channels(for: layout)
        func label(_ value: Mix?) -> String {
            guard let value else { return "off" }
            let level = Int((value.gain * 100).rounded())
            return "\(value.channel.name) · \(level)%"
        }
        if layout == .neoStereo {
            return Snapshot(source: source, layout: layout, pan: pan,
                            neo: "stereo · 100%", blueberry: "off")
        }
        return Snapshot(source: source, layout: layout, pan: pan,
                        neo: label(mix.local), blueberry: label(mix.remote))
    }

    deinit { stop(notify: false) }

    private static func jukedPID() -> pid_t? {
        let installed = URL(fileURLWithPath: NSHomeDirectory()).appendingPathComponent(".local/bin/juked")
        guard FileManager.default.isExecutableFile(atPath: installed.path) else { return nil }
        let process = Process(), pipe = Pipe()
        process.executableURL = installed
        process.arguments = ["pid"]
        process.standardOutput = pipe
        process.standardError = FileHandle.nullDevice
        guard (try? process.run()) != nil else { return nil }
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        guard process.terminationStatus == 0,
              let value = String(data: data, encoding: .utf8)?.trimmingCharacters(in: .whitespacesAndNewlines),
              let pid = Int32(value) else { return nil }
        return pid
    }
}
