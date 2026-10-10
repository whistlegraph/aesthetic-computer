import AppKit
import CoreAudio
import AudioToolbox

/// A separate process keeps the playback callback out of its own tap while
/// still capturing every Menu Band engine. No audio is recorded or networked.
#if !MAC_APP_STORE
@available(macOS 14.2, *)
final class MenuBandSystemVolumeHelper {
    private var device: AudioDeviceID = 0
    private var aggregate: AudioDeviceID = 0
    private var tap: AudioObjectID = 0
    private var primer: AudioDeviceIOProcID?
    private var render: AudioDeviceIOProcID?
    private let gainLock = NSLock()
    private var requestedGain: Float = 0.25
    private var renderGain: Float = 0.25
    private var targetGain: Float = 0.25
    private var requestedAir: Float = 0
    private var targetAir: Float = 0
    private var renderAir: Float = 0
    private var requestedColor: MenuBandAirColor = .cabin
    private var color: MenuBandAirColor = .cabin
    private var requestedCutoff = MenuBandAirNoise.defaultCutoff
    private var cutoff = MenuBandAirNoise.defaultCutoff
    private var noise = MenuBandAirNoise()
    private var physicalInputChannels = 0
    private var sampleRate: Double = 44100

    init(inputChannels: Int = 0, gain: Float = 0.25, sampleRate: Double = 44100) {
        physicalInputChannels = inputChannels
        self.sampleRate = sampleRate
        let initial: Float = gain.isFinite ? max(0, min(1, gain)) : 0.25
        requestedGain = initial; renderGain = initial; targetGain = initial
    }

    enum Failure: LocalizedError {
        case message(String)
        var errorDescription: String? { if case .message(let text) = self { return text }; return nil }
    }
    private func check(_ status: OSStatus, _ operation: String) throws {
        guard status == noErr else { throw Failure.message("\(operation) (\(status))") }
    }
    private func read<T>(_ id: AudioObjectID, _ selector: AudioObjectPropertySelector,
                         _ initial: T, scope: AudioObjectPropertyScope = kAudioObjectPropertyScopeGlobal) throws -> T {
        var address = AudioObjectPropertyAddress(mSelector: selector, mScope: scope, mElement: 0)
        var size = UInt32(MemoryLayout<T>.size), result = initial
        try withUnsafeMutableBytes(of: &result) {
            try check(AudioObjectGetPropertyData(id, &address, 0, nil, &size, $0.baseAddress!), "Read audio property")
        }
        return result
    }
    private func channels(_ id: AudioDeviceID, _ scope: AudioObjectPropertyScope) throws -> Int {
        var address = AudioObjectPropertyAddress(mSelector: kAudioDevicePropertyStreamConfiguration, mScope: scope, mElement: 0)
        var bytes: UInt32 = 0
        try check(AudioObjectGetPropertyDataSize(id, &address, 0, nil, &bytes), "Read channel count")
        let storage = UnsafeMutableRawPointer.allocate(byteCount: Int(bytes), alignment: MemoryLayout<AudioBufferList>.alignment)
        defer { storage.deallocate() }
        try check(AudioObjectGetPropertyData(id, &address, 0, nil, &bytes, storage), "Read channels")
        return UnsafeMutableAudioBufferListPointer(storage.assumingMemoryBound(to: AudioBufferList.self)).reduce(0) { $0 + Int($1.mNumberChannels) }
    }
    private func validateOutputFormat(_ id: AudioDeviceID) throws {
        var address = AudioObjectPropertyAddress(mSelector: kAudioDevicePropertyStreams,
                                                 mScope: kAudioDevicePropertyScopeOutput, mElement: 0)
        var bytes: UInt32 = 0
        try check(AudioObjectGetPropertyDataSize(id, &address, 0, nil, &bytes), "Read output streams")
        var streams = [AudioStreamID](repeating: 0, count: Int(bytes) / MemoryLayout<AudioStreamID>.size)
        try check(AudioObjectGetPropertyData(id, &address, 0, nil, &bytes, &streams), "Read output streams")
        for stream in streams {
            let format = try read(stream, kAudioStreamPropertyVirtualFormat, AudioStreamBasicDescription())
            guard format.mFormatID == kAudioFormatLinearPCM,
                  format.mFormatFlags & kAudioFormatFlagIsFloat != 0,
                  format.mFormatFlags & kAudioFormatFlagIsBigEndian == 0,
                  format.mBitsPerChannel == 32 else {
                throw Failure.message("The output does not provide native Float32 audio")
            }
        }
    }
    func setGain(_ value: Float) {
        guard value.isFinite else { return }
        gainLock.lock(); requestedGain = max(0, min(1, value)); gainLock.unlock()
    }
    func setAir(_ value: Float, color: MenuBandAirColor, cutoff: Double = MenuBandAirNoise.defaultCutoff) {
        guard value.isFinite else { return }
        gainLock.lock()
        requestedAir = max(0, min(1, value)); requestedColor = color
        if cutoff.isFinite { requestedCutoff = max(60, min(6000, cutoff)) }
        gainLock.unlock()
    }
    func start(uid: String, gain: Float) throws {
        setGain(gain); renderGain = requestedGain; targetGain = requestedGain
        device = try read(AudioObjectID(kAudioObjectSystemObject), kAudioHardwarePropertyDefaultOutputDevice, AudioDeviceID(0))
        let actualUID: CFString? = try read(device, kAudioDevicePropertyDeviceUID, Optional<CFString>.none)
        guard actualUID as String? == uid else { throw Failure.message("System output changed; enable volume again") }
        guard try channels(device, kAudioDevicePropertyScopeOutput) == 2 else {
            throw Failure.message("System volume currently supports stereo outputs")
        }
        physicalInputChannels = try channels(device, kAudioDevicePropertyScopeInput)
        sampleRate = try read(device, kAudioDevicePropertyNominalSampleRate, Double(44100))
        // Register this helper as an audio process before resolving its ID.
        try check(AudioDeviceCreateIOProcIDWithBlock(&primer, device, nil) { _, _, _, output, _ in
            for buffer in UnsafeMutableAudioBufferListPointer(output) {
                if let data = buffer.mData { memset(data, 0, Int(buffer.mDataByteSize)) }
            }
        }, "Prepare output")
        try check(AudioDeviceStart(device, primer), "Start output")
        var pid = getpid(), process = AudioObjectID(0), bytes = UInt32(MemoryLayout<AudioObjectID>.size)
        var address = AudioObjectPropertyAddress(mSelector: kAudioHardwarePropertyTranslatePIDToProcessObject,
                                                 mScope: kAudioObjectPropertyScopeGlobal, mElement: 0)
        try check(AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address,
                                             UInt32(MemoryLayout<pid_t>.size), &pid, &bytes, &process), "Identify volume helper")
        guard process != 0 else { throw Failure.message("Could not exclude output helper from capture") }
        let description = CATapDescription(excludingProcesses: [process], deviceUID: uid, stream: 0)
        description.name = "Menu Band System Volume"
        description.uuid = UUID(); description.isPrivate = true
        description.muteBehavior = .mutedWhenTapped
        try check(AudioHardwareCreateProcessTap(description, &tap), "Allow Menu Band system audio access")
        let format = try read(tap, kAudioTapPropertyFormat, AudioStreamBasicDescription())
        guard format.mFormatID == kAudioFormatLinearPCM,
              format.mFormatFlags & kAudioFormatFlagIsFloat != 0,
              format.mBitsPerChannel == 32, format.mChannelsPerFrame == 2 else {
            throw Failure.message("The output tap did not provide stereo Float32 audio")
        }
        let spec: [String: Any] = [
            kAudioAggregateDeviceNameKey: "Menu Band System Volume",
            kAudioAggregateDeviceUIDKey: "computer.aesthetic.menuband.volume.\(UUID().uuidString)",
            kAudioAggregateDeviceIsPrivateKey: true,
            kAudioAggregateDeviceIsStackedKey: false,
            kAudioAggregateDeviceMainSubDeviceKey: uid,
            kAudioAggregateDeviceSubDeviceListKey: [[kAudioSubDeviceUIDKey: uid]],
            kAudioAggregateDeviceTapAutoStartKey: true,
            kAudioAggregateDeviceTapListKey: [[kAudioSubTapUIDKey: description.uuid.uuidString,
                                             kAudioSubTapDriftCompensationKey: true]],
        ]
        try check(AudioHardwareCreateAggregateDevice(spec as CFDictionary, &aggregate), "Prepare system volume")
        // Physical inputs precede the tap. Never copy the microphone channels.
        let inputs = try channels(aggregate, kAudioDevicePropertyScopeInput)
        guard inputs == physicalInputChannels + 2,
              try channels(aggregate, kAudioDevicePropertyScopeOutput) == 2 else {
            throw Failure.message("Unexpected system-volume channel layout")
        }
        try validateOutputFormat(aggregate)
        // Run directly on the HAL audio thread; a dispatch hop adds scheduling
        // jitter to every device cycle, especially at very small IO buffers.
        try check(AudioDeviceCreateIOProcIDWithBlock(&render, aggregate, nil) { [weak self] _, input, _, output, _ in
            self?.process(input: input, output: output)
        }, "Prepare volume callback")
        try check(AudioDeviceStart(aggregate, render), "Start system volume")
    }
    func process(input: UnsafePointer<AudioBufferList>, output: UnsafeMutablePointer<AudioBufferList>) {
        // No allocation or blocking lock on the audio callback. A slider write
        // that overlaps this buffer is simply picked up by the next buffer.
        if gainLock.try() {
            targetGain = requestedGain; targetAir = requestedAir; color = requestedColor; cutoff = requestedCutoff
            gainLock.unlock()
        }
        let ins = UnsafeMutableAudioBufferListPointer(UnsafeMutablePointer(mutating: input))
        let outs = UnsafeMutableAudioBufferListPointer(output)
        for buffer in outs { if let data = buffer.mData { memset(data, 0, Int(buffer.mDataByteSize)) } }
        var channelOffset = 0
        for source in ins {
            let width = Int(source.mNumberChannels)
            defer { channelOffset += width }
            guard width > 0, let data = source.mData else { continue }
            let frames = Int(source.mDataByteSize) / (4 * width)
            for channel in 0..<width {
                let stereoChannel = channelOffset + channel - physicalInputChannels
                guard (0..<2).contains(stereoChannel) else { continue }
                var offset = 0
                for destination in outs {
                    let outWidth = Int(destination.mNumberChannels)
                    defer { offset += outWidth }
                    guard stereoChannel >= offset, stereoChannel < offset + outWidth,
                          let outData = destination.mData else { continue }
                    let count = min(frames, Int(destination.mDataByteSize) / (4 * outWidth))
                    let src = data.assumingMemoryBound(to: Float.self)
                    let dst = outData.assumingMemoryBound(to: Float.self)
                    for frame in 0..<count {
                        let sample = src[frame * width + channel]
                        dst[frame * outWidth + stereoChannel - offset] = sample.isFinite ? sample : 0
                    }
                }
            }
        }
        // Add one coherent, quiet stereo noise bed, then apply System gain
        // to both music and Air. A zero System slider always means silence.
        var frames = Int.max
        for destination in outs where destination.mNumberChannels > 0 && destination.mData != nil {
            frames = min(frames, Int(destination.mDataByteSize) / (4 * Int(destination.mNumberChannels)))
        }
        if frames == Int.max { frames = 0 }
        let gainStep: Float = 1 / Float(max(1, sampleRate * 0.015))
        let airStep: Float = 1 / Float(max(1, sampleRate * 0.25))
        for frame in 0..<frames {
            renderGain += max(-gainStep, min(gainStep, targetGain - renderGain))
            renderAir += max(-airStep, min(airStep, targetAir - renderAir))
            let air: Float = renderAir > 0
                ? noise.next(color: color, sampleRate: sampleRate, cutoff: cutoff) * renderAir * renderAir * 0.12 : 0
            for destination in outs {
                guard let data = destination.mData else { continue }
                let width = Int(destination.mNumberChannels)
                let samples = data.assumingMemoryBound(to: Float.self)
                for channel in 0..<width {
                    samples[frame * width + channel] = max(-1, min(1, (samples[frame * width + channel] + air) * renderGain))
                }
            }
        }
    }
    func stop() {
        if aggregate != 0 {
            if let render { AudioDeviceStop(aggregate, render); AudioDeviceDestroyIOProcID(aggregate, render) }
            AudioHardwareDestroyAggregateDevice(aggregate)
        }
        if tap != 0 { AudioHardwareDestroyProcessTap(tap) }
        if let primer { AudioDeviceStop(device, primer); AudioDeviceDestroyIOProcID(device, primer) }
        render = nil; primer = nil; aggregate = 0; tap = 0
    }
    deinit { stop() }

    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.count > 1, args[1] == "--system-volume-helper" else { return false }
        guard args.count == 4, let gain = Float(args[3]), gain.isFinite else { return true }
        let helper = MenuBandSystemVolumeHelper()
        do {
            try helper.start(uid: args[2], gain: gain)
            print("READY"); fflush(stdout)
            // EOF ties lifetime to the UI parent. A crash/quit cannot strand
            // the mute tap. The helper never persists a system routing change.
            while let line = readLine() {
                if line == "STOP" { break }
                let parts = line.split(separator: " ")
                if parts.count == 4, parts[0] == "AIR",
                   let color = MenuBandAirColor(rawValue: String(parts[1])), let value = Float(parts[2]),
                   let cutoff = Double(parts[3]) {
                    helper.setAir(value, color: color, cutoff: cutoff); continue
                }
                if let value = Float(line) { helper.setGain(value) }
            }
        } catch {
            print("ERROR \(error.localizedDescription)"); fflush(stdout)
        }
        helper.stop()
        return true
    }
}

final class MenuBandSystemVolume: NSObject {
    static let shared = MenuBandSystemVolume()
    private var helper: Process?
    private var commands: FileHandle?
    private var response = ""
    private var output: FileHandle?
    private var deviceUID: String?
    private var deviceRate: Double = 0
    private var watchdog: Timer?
    private(set) var active = false
    private(set) var starting = false
    private(set) var error: String?
    private(set) var gain: Float = 0.25
    private(set) var airLevel: Float = 0
    private(set) var airColor = MenuBandAirColor(rawValue: UserDefaults.standard.string(forKey: "systemAirColor") ?? "") ?? .cabin
    private(set) var airCutoff: Double = {
        let saved = UserDefaults.standard.double(forKey: "systemAirCutoff")
        return saved.isFinite && saved >= 60 ? min(6000, saved) : MenuBandAirNoise.defaultCutoff
    }()
    var onChange: (() -> Void)?

    func setGain(_ value: Float) {
        guard value.isFinite else { return }
        gain = max(0, min(1, value))
        if let deviceUID { UserDefaults.standard.set(gain, forKey: "systemVolume.\(deviceUID)") }
        do { try commands?.write(contentsOf: Data("\(gain)\n".utf8)) }
        catch { stop(); self.error = "System volume disconnected" }
        onChange?()
    }
    func setAir(_ value: Float, color: MenuBandAirColor? = nil) {
        guard value.isFinite else { return }
        airLevel = max(0, min(1, value))
        if let color { airColor = color; UserDefaults.standard.set(color.rawValue, forKey: "systemAirColor") }
        do { try commands?.write(contentsOf: Data("AIR \(airColor.rawValue) \(airLevel) \(airCutoff)\n".utf8)) }
        catch { stop(); self.error = "System volume disconnected" }
        onChange?()
    }
    func setAirCutoff(_ value: Double) {
        guard value.isFinite else { return }
        airCutoff = max(60, min(6000, value))
        UserDefaults.standard.set(airCutoff, forKey: "systemAirCutoff")
        setAir(airLevel)
    }
    func start() {
        guard helper == nil else { return }
        guard #available(macOS 14.2, *) else { error = "Requires macOS 14.2 or newer"; onChange?(); return }
        guard let id = MenuBandAudioDevices.systemDefaultOutputID(),
              let device = MenuBandAudioDevices.all().first(where: { $0.id == id }),
              let executable = Bundle.main.executableURL else { return }
        deviceUID = device.uid
        deviceRate = MenuBandAudioDevices.nominalRate(of: id)
        signal(SIGPIPE, SIG_IGN) // A stopped helper turns writes into errors, not an app crash.
        let key = "systemVolume.\(device.uid)"
        gain = UserDefaults.standard.object(forKey: key) == nil ? 0.25 : Float(UserDefaults.standard.double(forKey: key))
        gain = gain.isFinite ? max(0, min(1, gain)) : 0.25
        let process = Process(), input = Pipe(), output = Pipe()
        process.executableURL = executable
        process.arguments = ["--system-volume-helper", device.uid, String(gain)]
        process.standardInput = input; process.standardOutput = output
        self.output = output.fileHandleForReading
        self.commands = input.fileHandleForWriting
        response = ""; error = nil; starting = true; helper = process
        output.fileHandleForReading.readabilityHandler = { [weak self, weak process] handle in
            let data = handle.availableData
            guard !data.isEmpty else { handle.readabilityHandler = nil; return }
            DispatchQueue.main.async {
                guard let self, let process, self.helper === process else { return }
                self.receive(String(decoding: data, as: UTF8.self))
            }
        }
        process.terminationHandler = { [weak self] child in
            DispatchQueue.main.async {
                guard let self, self.helper === child else { return }
                self.stop(); self.error = "System volume stopped; hardware level restored"; self.onChange?()
            }
        }
        do {
            try process.run()
            watchdog = Timer.scheduledTimer(withTimeInterval: 0.5, repeats: true) { [weak self] _ in
                guard let self else { return }
                let id = MenuBandAudioDevices.systemDefaultOutputID()
                if id.flatMap({ MenuBandAudioDevices.uid(for: $0) }) != self.deviceUID {
                    self.stop(); self.error = "Output changed; enable volume again"; self.onChange?()
                } else if let id, abs(MenuBandAudioDevices.nominalRate(of: id) - self.deviceRate) > 1 {
                    self.stop(); self.error = "Output rate changed; enable volume again"; self.onChange?()
                }
            }
        } catch { stop(); self.error = error.localizedDescription }
        onChange?()
    }
    private func receive(_ text: String) {
        response += text
        while let end = response.firstIndex(of: "\n") {
            let line = String(response[..<end]); response.removeSubrange(...end)
            if line == "READY" { starting = false; active = true; setAir(airLevel) }
            else if line.hasPrefix("ERROR ") { stop(); error = String(line.dropFirst(6)) }
            onChange?()
        }
    }
    func stop() {
        watchdog?.invalidate(); watchdog = nil
        let process = helper; helper = nil
        try? commands?.write(contentsOf: Data("STOP\n".utf8)); try? commands?.close(); commands = nil
        output?.readabilityHandler = nil; output = nil
        if let process {
            DispatchQueue.main.asyncAfter(deadline: .now() + 2) { if process.isRunning { process.terminate() } }
        }
        active = false; starting = false; airLevel = 0; onChange?()
    }
}

final class MenuBandSystemVolumeView: NSStackView {
    private let toggle = NSButton(checkboxWithTitle: "System", target: nil, action: nil)
    private let slider = NSSlider(value: 0.25, minValue: 0, maxValue: 1, target: nil, action: nil)
    private let readout = NSTextField(labelWithString: "")
    private let airSlider = NSSlider(value: 0, minValue: 0, maxValue: 1, target: nil, action: nil)
    private let airColor = NSPopUpButton(frame: .zero, pullsDown: false)
    private let airReadout = NSTextField(labelWithString: "0%")
    private let filterSlider = NSSlider(value: 0, minValue: 0, maxValue: 1, target: nil, action: nil)
    private let filterReadout = NSTextField(labelWithString: "160 Hz")
    private let control = MenuBandSystemVolume.shared
    init() {
        super.init(frame: .zero)
        orientation = .vertical; alignment = .leading; spacing = 5
        toggle.controlSize = .small; toggle.target = self; toggle.action = #selector(toggleVolume)
        toggle.toolTip = "Control all apps on the system output. Turning this off restores the hardware volume."
        slider.controlSize = .small; slider.isContinuous = true
        slider.target = self; slider.action = #selector(changeVolume)
        slider.setAccessibilityLabel("System output volume")
        slider.widthAnchor.constraint(equalToConstant: 115).isActive = true
        readout.font = .monospacedDigitSystemFont(ofSize: 10, weight: .regular)
        readout.widthAnchor.constraint(equalToConstant: 32).isActive = true
        let systemRow = NSStackView(views: [toggle, slider, readout])
        systemRow.orientation = .horizontal; systemRow.alignment = .centerY; systemRow.spacing = 7
        addArrangedSubview(systemRow)
        let airLabel = NSTextField(labelWithString: "Air")
        airLabel.font = .systemFont(ofSize: 11)
        airColor.addItems(withTitles: MenuBandAirColor.allCases.map(\.title))
        airColor.controlSize = .mini; airColor.target = self; airColor.action = #selector(changeAirColor)
        airColor.setAccessibilityLabel("Air noise color")
        airColor.widthAnchor.constraint(equalToConstant: 62).isActive = true
        airSlider.controlSize = .small; airSlider.isContinuous = true
        airSlider.target = self; airSlider.action = #selector(changeAir)
        airSlider.setAccessibilityLabel("Air level")
        airSlider.widthAnchor.constraint(equalToConstant: 92).isActive = true
        airSlider.toolTip = "Background noise to mask distractions; zero is silent. Requires System volume."
        airReadout.font = .monospacedDigitSystemFont(ofSize: 10, weight: .regular)
        airReadout.widthAnchor.constraint(equalToConstant: 32).isActive = true
        let airRow = NSStackView(views: [airLabel, airColor, airSlider, airReadout])
        airRow.orientation = .horizontal; airRow.alignment = .centerY; airRow.spacing = 5
        addArrangedSubview(airRow)
        let filterLabel = NSTextField(labelWithString: "Filter")
        filterLabel.font = .systemFont(ofSize: 11)
        filterLabel.widthAnchor.constraint(equalToConstant: 38).isActive = true
        filterSlider.controlSize = .small; filterSlider.isContinuous = true
        filterSlider.target = self; filterSlider.action = #selector(changeFilter)
        filterSlider.setAccessibilityLabel("Air filter cutoff")
        filterSlider.widthAnchor.constraint(equalToConstant: 119).isActive = true
        filterSlider.toolTip = "Left: deeper rumble. Right: brighter air. Filters Air only."
        filterReadout.font = .monospacedDigitSystemFont(ofSize: 10, weight: .regular)
        filterReadout.widthAnchor.constraint(equalToConstant: 51).isActive = true
        let filterRow = NSStackView(views: [filterLabel, filterSlider, filterReadout])
        filterRow.orientation = .horizontal; filterRow.alignment = .centerY; filterRow.spacing = 5
        addArrangedSubview(filterRow)
        control.onChange = { [weak self] in self?.refresh() }; refresh()
    }
    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }
    @objc private func toggleVolume() { if control.active || control.starting { control.stop() } else { control.start() }; refresh() }
    @objc private func changeVolume() { control.setGain(Float(slider.doubleValue)) }
    @objc private func changeAir() { control.setAir(Float(airSlider.doubleValue)) }
    @objc private func changeAirColor() {
        control.setAir(control.airLevel, color: MenuBandAirColor.allCases[airColor.indexOfSelectedItem])
    }
    @objc private func changeFilter() {
        control.setAirCutoff(MenuBandAirNoise.cutoff(at: filterSlider.doubleValue))
    }
    private func refresh() {
        toggle.state = control.active || control.starting ? .on : .off
        slider.isEnabled = control.active
        slider.doubleValue = Double(control.gain)
        filterSlider.doubleValue = MenuBandAirNoise.position(for: control.airCutoff)
        filterReadout.stringValue = "\(Int(control.airCutoff.rounded())) Hz"
        airSlider.isEnabled = control.active
        airSlider.doubleValue = Double(control.airLevel)
        airColor.selectItem(at: MenuBandAirColor.allCases.firstIndex(of: control.airColor) ?? 0)
        airReadout.stringValue = "\(Int(control.airLevel * 100))%"
        readout.stringValue = control.starting ? "…" : control.active ? "\(Int(control.gain * 100))%" : "off"
        let device = MenuBandAudioDevices.systemDefaultOutputID().flatMap { id in MenuBandAudioDevices.all().first { $0.id == id }?.name } ?? "System output"
        toolTip = control.error ?? "\(device) — all apps. Audio stays on this Mac."
        toggle.toolTip = control.error ?? "Enable system output volume. Turning it off restores the hardware level."
        slider.toolTip = "\(device) — all apps"
        if control.error != nil { readout.stringValue = "!" }
    }
}
#endif
