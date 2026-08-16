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
    /// Out-of-process local leg, used when the tap mutes our own process
    /// (aesthetic source) — see startDistributed.
    private var localHelper: Process?
    private var spotifyTap: AnyObject?
    private var remoteReceiver: Process?
    private var localOutputGeneration = 0
    private var routeObserver: NSObjectProtocol?
    // Remote-leg healing: a receiver that dies (far Mac slept, network
    // blink, ssh dropped) re-engages instead of killing the whole room; a
    // leg that keeps dying within a minute gives up after three tries so a
    // genuinely broken room reads as failed rather than looping forever.
    private var applyGeneration = 0
    private var remoteRetries = 0
    private var remoteSpawnedAt: Date?

    init() {
        // Follow the Mac's default output when it changes under us (AirPods
        // hop, unplugged interface) — the same reopen the manual picker does.
        DefaultOutputWatcher.shared.activate()
        routeObserver = NotificationCenter.default.addObserver(
            forName: .macDefaultOutputDeviceDidChange, object: nil, queue: .main
        ) { [weak self] _ in self?.refreshLocalOutputDevice() }
    }


    var isDistributing: Bool {
        guard case .live = state else { return false }
        return layout != .neoStereo
    }

    func useSource(_ nextSource: Source) {
        guard source != nextSource else { return }
        source = nextSource
        if case .live = state { apply(layout, pan: pan) }
    }

    func apply(_ nextLayout: Layout, pan nextPan: Float? = nil, remote: String = "blueberry") {
        remoteRetries = 0   // user intent = fresh grace for the far Mac
        engage(nextLayout, pan: nextPan, remote: remote)
    }

    private func engage(_ nextLayout: Layout, pan nextPan: Float? = nil, remote: String = "blueberry") {
        applyGeneration += 1
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
        sender.onLog = { NSLog("Menu Band Juke room sender: \($0)") }
        try sender.start()

        var local: ACAudioRoomReceiver?
        var helperProcess: Process?
        var remoteProcess: Process?
        do {
            if let localMix = mix.local {
                if source == .aesthetic {
                    // The aesthetic tap mutes THIS process — and
                    // .mutedWhenTapped is process-wide, so an in-process
                    // receiver renders into the mute and Neo goes silent
                    // while every log line reads healthy. Neo's leg must
                    // render from OUTSIDE the muted process: the same CLI
                    // blueberry runs, minus the ssh.
                    let helper = Process()
                    helper.executableURL = URL(fileURLWithPath: "/bin/bash")
                    let gain = String(format: "%.3f", localMix.gain)
                    helper.arguments = ["-lc",
                        "receiver=\"$HOME/.local/bin/ac-audio-room\"; " +
                        "/usr/bin/pkill -f \"^[^ ]*ac-audio-room receive --host 127.0.0.1\" 2>/dev/null || true; " +
                        "exec \"$receiver\" receive --host 127.0.0.1 --channel \(localMix.channel.name) --gain \(gain) --name Neo >> /tmp/ac-audio-room.log 2>&1"]
                    helper.standardOutput = FileHandle.nullDevice
                    helper.standardError = FileHandle.nullDevice
                    let generation = applyGeneration
                    helper.terminationHandler = { [weak self] process in
                        DispatchQueue.main.async {
                            guard let self, case .live = self.state,
                                  generation == self.applyGeneration else { return }
                            if self.remoteRetries < 3 {
                                self.remoteRetries += 1
                                NSLog("Menu Band Juke room: Neo helper exited (status %d) — re-engaging %d/3",
                                      process.terminationStatus, self.remoteRetries)
                                DispatchQueue.main.asyncAfter(deadline: .now() + 2.0) { [weak self] in
                                    guard let self, case .live = self.state,
                                          generation == self.applyGeneration else { return }
                                    self.engage(self.layout, pan: self.pan, remote: remote)
                                }
                            } else {
                                self.stop(notify: false)
                                self.state = .failed("Neo helper exited (status \(process.terminationStatus))")
                            }
                        }
                    }
                    try helper.run()
                    helperProcess = helper
                } else {
                    let receiver = ACAudioRoomReceiver(configuration: .init(
                        host: "127.0.0.1", name: "Neo", channel: localMix.channel, gain: localMix.gain))
                    receiver.onLog = { NSLog("Menu Band Juke room Neo: \($0)") }
                    try receiver.start()
                    local = receiver
                }
            }

            if let remoteMix = mix.remote {
                let process = Process()
                process.executableURL = URL(fileURLWithPath: "/usr/bin/ssh")
                let gain = String(format: "%.3f", remoteMix.gain)
                // pkill note: the pattern must match BOTH the ~/.local/bin
                // symlink and the resolved .build/release binary (the script
                // execs through readlink now), but must NOT match this bash
                // wrapper's own command line — hence the anchored first-token
                // form. Receiver output goes to a log on the far Mac instead
                // of the void; it self-exits when the sender disappears.
                process.arguments = [
                    "-o", "BatchMode=yes", remote,
                    "/bin/bash -lc 'receiver=\"$HOME/.local/bin/ac-audio-room\"; " +
                    "/usr/bin/pkill -f \"^[^ ]*ac-audio-room receive --host neo.local\" 2>/dev/null || true; " +
                    "exec \"$receiver\" receive --host neo.local --channel \(remoteMix.channel.name) --gain \(gain) --name Blueberry >> /tmp/ac-audio-room.log 2>&1'",
                ]
                process.standardOutput = FileHandle.nullDevice
                process.standardError = FileHandle.nullDevice
                let generation = applyGeneration
                process.terminationHandler = { [weak self] process in
                    DispatchQueue.main.async {
                        guard let self, case .live = self.state,
                              generation == self.applyGeneration else { return }
                        if let started = self.remoteSpawnedAt,
                           Date().timeIntervalSince(started) > 60 {
                            self.remoteRetries = 0   // it ran fine for a while — fresh outage
                        }
                        if self.remoteRetries < 3 {
                            self.remoteRetries += 1
                            NSLog("Menu Band Juke room: %@ leg exited (status %d) — re-engaging %d/3",
                                  remote, process.terminationStatus, self.remoteRetries)
                            DispatchQueue.main.asyncAfter(deadline: .now() + 2.0) { [weak self] in
                                guard let self, case .live = self.state,
                                      generation == self.applyGeneration else { return }
                                self.engage(self.layout, pan: self.pan, remote: remote)
                            }
                        } else {
                            self.stop(notify: false)
                            self.state = .failed("Blueberry exited (status \(process.terminationStatus))")
                        }
                    }
                }
                try process.run()
                remoteSpawnedAt = Date()
                remoteProcess = process
            }

            let tap = ACProcessAudioTap(processID: pid, name: "Menu Band Juke \(source.rawValue)", muteOriginal: true)
            tap.onLog = { NSLog("Menu Band Juke room tap: \($0)") }
            try tap.start { sender.send($0) }

            self.sender = sender
            self.localReceiver = local
            self.localHelper = helperProcess
            self.remoteReceiver = remoteProcess
            self.spotifyTap = tap
            state = .live(snapshot(for: layout))
        } catch {
            if remoteProcess?.isRunning == true { remoteProcess?.terminate() }
            if let helperProcess {
                helperProcess.terminationHandler = nil
                if helperProcess.isRunning { helperProcess.terminate() }
            }
            local?.stop(); sender.stop()
            throw error
        }
    }

    func stop() { stop(notify: true) }

    /// Reopen only Neo's renderer on the newly selected Core Audio output.
    /// The sender, clock, and Blueberry receiver continue uninterrupted.
    func refreshLocalOutputDevice() {
        if let helper = localHelper {
            // Helper-process leg: its engine won't follow a device change on
            // its own — terminate it and let the healing path re-engage the
            // room on the new output.
            helper.terminate()
            return
        }
        guard isDistributing, let mix = channels(for: layout).local else { return }
        localOutputGeneration += 1
        let generation = localOutputGeneration
        // Retain the old engine for the worker closure before clearing the
        // property. Both AVAudioEngine.stop() and deinit may wait on the HAL
        // while a route changes, so neither belongs on AppKit's main thread.
        let previousReceiver = localReceiver
        localReceiver = nil
        // Core Audio can take a moment to settle a Bluetooth/virtual-device
        // route change. AVAudioEngine may block while opening during that
        // interval, so never make AppKit's menu action wait for the HAL.
        DispatchQueue.global(qos: .userInitiated).asyncAfter(deadline: .now() + 0.35) { [weak self] in
            guard let self else { return }
            previousReceiver?.stop()
            let receiver = ACAudioRoomReceiver(configuration: .init(
                host: "127.0.0.1", name: "Neo", channel: mix.channel, gain: mix.gain))
            receiver.onLog = { NSLog("JukeWizard room Neo: \($0)") }
            do {
                try receiver.start()
                DispatchQueue.main.async { [weak self] in
                    guard let self else { Self.retireOffMain(receiver); return }
                    guard self.localOutputGeneration == generation else {
                        Self.retireOffMain(receiver)
                        return
                    }
                    self.localReceiver = receiver
                }
            } catch {
                DispatchQueue.main.async { [weak self] in
                    guard let self, self.localOutputGeneration == generation else { return }
                    self.state = .failed("Neo output: \(error.localizedDescription)")
                }
            }
        }
    }

    /// Keep AVAudioEngine teardown away from AppKit even when a completed
    /// reopen has already been superseded by another device selection.
    private static func retireOffMain(_ receiver: ACAudioRoomReceiver) {
        DispatchQueue.global(qos: .utility).async { receiver.stop() }
    }

    private func stop(notify: Bool) {
        localOutputGeneration += 1
        if #available(macOS 14.2, *), let tap = spotifyTap as? ACProcessAudioTap { tap.stop() }
        spotifyTap = nil
        localReceiver?.stop(); localReceiver = nil
        localHelper?.terminationHandler = nil
        if localHelper?.isRunning == true { localHelper?.terminate() }
        localHelper = nil
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

    deinit {
        if let routeObserver { NotificationCenter.default.removeObserver(routeObserver) }
        stop(notify: false)
    }

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
