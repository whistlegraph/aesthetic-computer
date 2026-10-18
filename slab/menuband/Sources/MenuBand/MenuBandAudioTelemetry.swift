import AVFoundation
import AudioToolbox
import CoreAudio
import CryptoKit

/// Explicitly enabled local troubleshooting. Existing taps and render notifies
/// feed scalar meters; a utility queue writes one snapshot per second.
final class MenuBandAudioTelemetry {
    static let preferenceKey = "MBAudioDiagnosticsEnabled"
    let enabled: Bool
    let tones = AudioSignalMeter(), percussion = AudioSignalMeter()
    let mix = AudioSignalMeter(), output = AudioSignalMeter(), hardware = AudioSignalMeter()
    private let queue = DispatchQueue(label: "menuband.audio-diagnostics", qos: .utility)
    private let activityLock = NSLock()
    private var melodyTriggers = 0, percussionTriggers = 0
    private var overloads = 0, abnormalStops = 0
    private var lastDeviceFault: TimeInterval = 0
    private var timer: DispatchSourceTimer?
    private var hardwareNode: AVAudioOutputNode?
    private var observedDevice: AudioDeviceID = 0
    private var deviceListeners: [(AudioObjectPropertyAddress, AudioObjectPropertyListenerBlock)] = []
    private var uidFingerprint = ""
    private var lastHardwareSampleTime: Double?
    private var lastClientPIDs: [Int32] = []
    private var loggedWriteFailure = false
    private let log = AudioDiagnosticLog(directory: FileManager.default.homeDirectoryForCurrentUser
        .appendingPathComponent("Library/Logs/MenuBand", isDirectory: true))

    init(enabled: Bool = UserDefaults.standard.bool(forKey: preferenceKey)) { self.enabled = enabled }

    func trigger(percussion: Bool) {
        guard enabled else { return }
        activityLock.lock()
        if percussion { percussionTriggers += 1 } else { melodyTriggers += 1 }
        activityLock.unlock()
    }

    func event(_ name: String, _ fields: [String: Any] = [:]) {
        guard enabled else { return }
        let observedAt = ProcessInfo.processInfo.systemUptime
        queue.async { [weak self] in
            var row = fields; row["event"] = name; row["observedAt"] = observedAt
            self?.write(row)
        }
    }

    // Overload notifications can originate on the device IO thread even with
    // a listener queue supplied. Accumulate scalars there; never dispatch or
    // allocate a JSON event from that callback.
    private func deviceFault(overload: Bool) {
        guard activityLock.try() else { return }
        if overload { overloads += 1 } else { abnormalStops += 1 }
        lastDeviceFault = ProcessInfo.processInfo.systemUptime
        activityLock.unlock()
    }

    func attachHardwareProbe(to engine: AVAudioEngine) {
        guard enabled, hardwareNode == nil else { return }
        let node = engine.outputNode
        guard node.inputFormat(forBus: 0).commonFormat == .pcmFormatFloat32,
              let au = node.audioUnit else { return }
        let status = AudioUnitAddRenderNotify(au, menuBandHardwareTelemetryNotify,
            Unmanaged.passUnretained(self).toOpaque())
        if status == noErr { hardwareNode = node }
        event("hardware_probe", ["status": status])
    }

    /// The provider supplies only app state and its bound output ID. Device
    /// polling and all JSON/file work happen after its engine lock is released.
    func start(snapshot: @escaping () -> [String: Any]) {
        guard enabled, timer == nil else { return }
        event("session_started", ["schema": 1, "sampleAudioStored": false])
        NSLog("MenuBand audio diagnostics: %@", log.url.path)
        let timer = DispatchSource.makeTimerSource(queue: queue)
        timer.schedule(deadline: .now(), repeating: 1, leeway: .milliseconds(100))
        timer.setEventHandler { [weak self] in
            guard let self else { return }
            var row = snapshot()
            let device = row["outputDevice"] as? UInt32 ?? self.observedDevice
            self.observeDevice(device)
            row["device"] = self.deviceSnapshot(device)
            let meters = ["tones": self.tones.takeSnapshot(), "percussion": self.percussion.takeSnapshot(),
                          "mix": self.mix.takeSnapshot(), "output": self.output.takeSnapshot(),
                          "hardware": self.hardware.takeSnapshot()]
            if let data = try? JSONEncoder().encode(meters),
               let value = try? JSONSerialization.jsonObject(with: data) { row["meters"] = value }
            self.activityLock.lock()
            let activity = (self.melodyTriggers, self.percussionTriggers,
                            self.overloads, self.abnormalStops, self.lastDeviceFault)
            self.melodyTriggers = 0; self.percussionTriggers = 0
            self.overloads = 0; self.abnormalStops = 0
            self.activityLock.unlock()
            row["melodyTriggers"] = activity.0
            row["percussionTriggers"] = activity.1
            row["processorOverloads"] = activity.2
            row["abnormalStops"] = activity.3
            if activity.4 > 0 { row["lastDeviceFaultUptime"] = activity.4 }
            row["event"] = "snapshot"
            self.write(row)
            let clients = self.outputClientPIDs()
            if clients != self.lastClientPIDs {
                self.lastClientPIDs = clients
                self.write(["event": "output_clients_changed", "pids": clients])
            }
        }
        self.timer = timer
        timer.resume()
    }

    func stop() {
        timer?.cancel(); timer = nil
        if let au = hardwareNode?.audioUnit {
            AudioUnitRemoveRenderNotify(au, menuBandHardwareTelemetryNotify,
                Unmanaged.passUnretained(self).toOpaque())
        }
        hardwareNode = nil
        queue.async { [weak self] in self?.removeDeviceListeners() }
        event("session_stopped")
    }

    private func write(_ row: [String: Any]) {
        do { try log.append(row) }
        catch {
            if !loggedWriteFailure { NSLog("MenuBand diagnostics write failed: %@", String(describing: error)) }
            loggedWriteFailure = true
        }
    }

    private func observeDevice(_ device: AudioDeviceID) {
        guard observedDevice != device else { return }
        removeDeviceListeners()
        observedDevice = device
        lastHardwareSampleTime = nil
        uidFingerprint = MenuBandAudioDevices.uid(for: device).map {
            SHA256.hash(data: Data($0.utf8)).prefix(8).map { String(format: "%02x", $0) }.joined()
        } ?? ""
        write(["event": "bound_device_changed", "device": device, "uidFingerprint": uidFingerprint])
        guard device != 0 else { return }
        let properties: [(AudioObjectPropertySelector, String)] = [
            (kAudioDeviceProcessorOverload, "processor_overload"),
            (kAudioDevicePropertyIOStoppedAbnormally, "io_stopped_abnormally"),
            (kAudioDevicePropertyDeviceIsAlive, "alive_changed"),
            (kAudioDevicePropertyDeviceIsRunningSomewhere, "io_running_changed"),
            (kAudioDevicePropertyBufferFrameSize, "buffer_changed"),
            (kAudioDevicePropertyNominalSampleRate, "rate_changed")]
        for (selector, name) in properties {
            var address = AudioObjectPropertyAddress(mSelector: selector,
                mScope: kAudioObjectPropertyScopeGlobal, mElement: kAudioObjectPropertyElementMain)
            let block: AudioObjectPropertyListenerBlock = { [weak self] _, _ in
                if selector == kAudioDeviceProcessorOverload || selector == kAudioDevicePropertyIOStoppedAbnormally {
                    self?.deviceFault(overload: selector == kAudioDeviceProcessorOverload)
                } else {
                    self?.event(name, ["device": device])
                }
            }
            let status = AudioObjectAddPropertyListenerBlock(device, &address, queue, block)
            if status == noErr { deviceListeners.append((address, block)) }
            else { write(["event": "device_listener_failed", "property": name, "status": status]) }
        }
    }

    private func removeDeviceListeners() {
        for (var address, block) in deviceListeners {
            AudioObjectRemovePropertyListenerBlock(observedDevice, &address, queue, block)
        }
        deviceListeners.removeAll()
    }

    // A dictionary's Any-valued subscript otherwise lets Swift infer T = Any.
    // CoreAudio would then overwrite an existential container, not a scalar.
    private func read<T: Numeric>(_ object: AudioObjectID, _ selector: AudioObjectPropertySelector,
                         scope: AudioObjectPropertyScope = kAudioObjectPropertyScopeGlobal,
                         initial: T) -> T? {
        var value = initial
        var address = AudioObjectPropertyAddress(mSelector: selector, mScope: scope,
            mElement: kAudioObjectPropertyElementMain)
        var size = UInt32(MemoryLayout<T>.size)
        let status = withUnsafeMutableBytes(of: &value) {
            AudioObjectGetPropertyData(object, &address, 0, nil, &size, $0.baseAddress!)
        }
        return status == noErr ? value : nil
    }

    private func objects(_ object: AudioObjectID, _ selector: AudioObjectPropertySelector,
                         scope: AudioObjectPropertyScope = kAudioObjectPropertyScopeGlobal) -> [AudioObjectID] {
        var address = AudioObjectPropertyAddress(mSelector: selector, mScope: scope,
            mElement: kAudioObjectPropertyElementMain)
        var size: UInt32 = 0
        guard AudioObjectGetPropertyDataSize(object, &address, 0, nil, &size) == noErr,
              size > 0 else { return [] }
        var values = [AudioObjectID](repeating: 0, count: Int(size) / 4)
        let status = values.withUnsafeMutableBytes {
            AudioObjectGetPropertyData(object, &address, 0, nil, &size, $0.baseAddress!)
        }
        return status == noErr ? values : []
    }

    func deviceSnapshot(_ device: AudioDeviceID) -> [String: Any] {
        var row: [String: Any] = ["id": device, "uidFingerprint": uidFingerprint]
        row["alive"] = read(device, kAudioDevicePropertyDeviceIsAlive, initial: UInt32(0))
        row["running"] = read(device, kAudioDevicePropertyDeviceIsRunningSomewhere, initial: UInt32(0))
        row["bufferFrames"] = read(device, kAudioDevicePropertyBufferFrameSize, initial: UInt32(0))
        row["rate"] = read(device, kAudioDevicePropertyNominalSampleRate, initial: Float64(0))
        row["hogPID"] = read(device, kAudioDevicePropertyHogMode, initial: Int32(-1))
        row["defaultOutput"] = MenuBandAudioDevices.systemDefaultOutputID()
        var clock = AudioTimeStamp()
        clock.mFlags = [.sampleTimeValid, .hostTimeValid]
        let status = AudioDeviceGetCurrentTime(device, &clock)
        row["clockStatus"] = status
        if status == noErr, clock.mFlags.contains(.sampleTimeValid), clock.mSampleTime.isFinite {
            row["sampleTime"] = clock.mSampleTime
            if let lastHardwareSampleTime { row["sampleTimeDelta"] = clock.mSampleTime - lastHardwareSampleTime }
            lastHardwareSampleTime = clock.mSampleTime
        }
        row["outputStreams"] = objects(device, kAudioDevicePropertyStreams,
            scope: kAudioDevicePropertyScopeOutput).map { stream -> [String: Any] in
                var value: [String: Any] = ["id": stream]
                value["active"] = read(stream, kAudioStreamPropertyIsActive, initial: UInt32(0))
                value["startingChannel"] = read(stream, kAudioStreamPropertyStartingChannel, initial: UInt32(0))
                return value
            }
        return row
    }

    private func outputClientPIDs() -> [Int32] {
        guard #available(macOS 14.2, *) else { return [] }
        return objects(AudioObjectID(kAudioObjectSystemObject), kAudioHardwarePropertyProcessObjectList).compactMap { object in
            guard read(object, kAudioProcessPropertyIsRunningOutput, initial: UInt32(0)) == 1 else { return nil }
            return read(object, kAudioProcessPropertyPID, initial: Int32(0))
        }.sorted()
    }
}

private let menuBandHardwareTelemetryNotify: AURenderCallback = { context, flags, _, _, frames, data in
    guard flags.pointee.contains(.unitRenderAction_PostRender) else { return noErr }
    let telemetry = Unmanaged<MenuBandAudioTelemetry>.fromOpaque(context).takeUnretainedValue()
    telemetry.hardware.observe(data.map { UnsafePointer($0) }, frames: Int(frames),
        silent: flags.pointee.contains(.unitRenderAction_OutputIsSilence),
        error: flags.pointee.contains(.unitRenderAction_PostRenderError))
    return noErr
}
