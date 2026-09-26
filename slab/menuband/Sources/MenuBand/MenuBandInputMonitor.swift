import Foundation
import AVFoundation
import AudioToolbox
import CoreAudio
import os

/// Direct-device monitor: the microphone comes in through a plain AUHAL
/// input unit bound straight to the interface (the path ffmpeg and DAWs
/// use), lands in a lock-guarded ring, and an `AVAudioSourceNode` pulls it
/// into the host engine's graph. The host engine stays a pure OUTPUT engine.
///
/// Why not `engine.inputNode`: on macOS the engine then hosts input+output
/// on a PRIVATE AGGREGATE device, and on the Scarlett Solo that aggregate
/// handed us 21 ms of zeros every 53 ms (three IO cycles of audio, two of
/// silence — a 19 Hz "fan" on the voice) regardless of IO buffer size, and
/// left the interface wedged for every client once the process exited
/// (frisbee, macOS 26.7, 2026-09-25). An independent avfoundation capture of
/// the same input was gapless, so the device is fine; the aggregate is not.
final class MenuBandInputMonitor {
    private let dryBus = AVAudioMixerNode()
    private let monoMixer = AVAudioMixerNode()
    /// Tone on the monitored voice (dry path only; the tape's raw stem is
    /// tapped upstream): a rumble cut and an "air" shelf that takes the
    /// hiss above 6 kHz down. `notepat.voiceAirCutDb` (default -4) sets it.
    private let voiceEQ = AVAudioUnitEQ(numberOfBands: 3)
    // Downward expander (gate) on the dry voice: hiss between phrases eases
    // down `gateDepth` when the input sits under `gateThreshold`; opens in a
    // few ms on the next syllable. `notepat.voiceGateDb` / `voiceGateDepthDb`.
    private var gateEnv: Float = 0
    private var gateGain: Float = 1
    private let gateThreshold: Float = {
        let v = UserDefaults.standard.double(forKey: "notepat.voiceGateDb"); return powf(10, Float(v != 0 ? v : -48) / 20)
    }()
    private let gateFloor: Float = {
        // Off by default (0 dB): the EQ is the noise control; set e.g. -12 to gate.
        let v = UserDefaults.standard.double(forKey: "notepat.voiceGateDepthDb"); return powf(10, Float(v) / 20)
    }()
    /// Wet-only pitch stage: the copy of the voice that feeds the echo and
    /// space sends rides the trackpad's pitch bend here. Time-pitch costs
    /// tens of ms, which a wet tail can afford and the dry voice cannot.
    private var wetConnected = false
    private var sourceNode: AVAudioSourceNode?
    private var dryTapInstalled = false
    private var attached = false
    private var enabled = false
    /// Monitor gain (unity = 1). Applied only while enabled; the mute
    /// state is `outputVolume = 0`, never a lost gain value.
    private var gain: Float = 1

    // MARK: AUHAL input side

    private var inputUnit: AudioUnit?
    private var inputDevice: AudioDeviceID = 0
    private var deviceChannels = 0
    private var sampleRate: Double = 0
    private var renderList: UnsafeMutableAudioBufferListPointer?
    private let renderCapacity = 4096   // frames per pull the unit may ask for

    // MARK: Ring (one Float plane per device channel)

    private let ringFrames = 32_768     // power of two; ~0.68 s at 48 kHz
    private var ring: [UnsafeMutablePointer<Float>] = []
    private var writeIndex = 0          // absolute frame counters
    private var readIndex = 0           // dry reader (the fast monitor path)
    private var readCursor = MonitorReadCursor()
    private var wetUnderflows = 0
    private var wetSource: AVAudioSourceNode?

    // Real-time pitch shifter on the voice itself (dry reader). Two taps
    // sliding through a short window, crossfaded — the classic delay-line
    // shifter. At ratio 1 the voice is direct (no delay); while bending
    // it costs about half the window. Robotic on big bends, and instant.
    private let shiftWindow = 2048                 // frames ≈ 46 ms at 44.1 kHz; fewer crossfades = less warble
    private var shiftRatio: Float = 1              // 2^(cents/1200)
    private var shiftPhase: Float = 0              // 0…1 position of tap 1
    private var shiftMix: Float = 0                // 0 = direct, 1 = shifted (ramped)
    private var shiftTargetMix: Float = 0

    // What the dry reader produced (post-shift, post channel pick) — the
    // wet path reads THIS, so echo/space carry the shifted voice and no
    // clean copy can leak through under the effects.
    private let postFrames = 16_384
    private var postRing: UnsafeMutablePointer<Float>?
    private var postWriteIndex = 0
    private var wetReadIndex = 0
    private let ringLock: UnsafeMutablePointer<os_unfair_lock> = {
        let p = UnsafeMutablePointer<os_unfair_lock>.allocate(capacity: 1)
        p.initialize(to: os_unfair_lock())
        return p
    }()
    private var nanScrubbed = 0
    /// Watchdog: capture callbacks per stats interval. Zero while attached
    /// means the AUHAL went quiet (its device churned away under it) —
    /// reopen on whatever the live input device is now.
    private var captures = 0
    private var stalledIntervals = 0
    private let watchdogQueue = DispatchQueue(label: "menuband.monitor.watchdog")
    /// IO cycle the host wants on the device while monitoring. Written on the
    /// input unit before start and re-asserted whenever the device flips
    /// back (AVAudioEngine restarts and a fresh AUHAL both like 512).
    var preferredIOBufferFrames: UInt32 = 64
    private var bufferListenerQueue = DispatchQueue(label: "menuband.monitor.buffer")
    private var bufferListenerInstalled = false
    private var bufferListenerBlock: AudioObjectPropertyListenerBlock?
    /// Re-assert budget: at most three corrections per ten seconds. A
    /// buffer-size tug of war with another client is how the Scarlett's
    /// USB stack wedges, so after the budget we log and back off.
    private var reassertTimes: [TimeInterval] = []
    /// Reader lead in frames: the ring is primed with this much silence so
    /// the render never runs into an empty ring on cycle-phase jitter. An
    /// underflow re-primes (latency floats up to the jitter ceiling only).
    private var leadFrames = 128
    private var lastCallbackFrames = 0
    private var monitorChannel = 0      // 0 = mix all, k = device channel k
    private var underflows = 0
    private var overflows = 0
    private var drops = 0
    private var leadAccum = 0
    private var pulls = 0
    private var lastStatsLog: TimeInterval = 0

    private var healthTimer: DispatchSourceTimer?
    private func startHealthTimer() {
        healthTimer?.cancel()
        let t = DispatchSource.makeTimerSource(queue: watchdogQueue)
        t.schedule(deadline: .now() + 5, repeating: 5)
        t.setEventHandler { [weak self] in self?.logStatsIfDue(force: true) }
        t.resume()
        healthTimer = t
    }
    private func stopHealthTimer() { healthTimer?.cancel(); healthTimer = nil }

    var isAttached: Bool { attached }
    var boundDeviceID: AudioDeviceID { inputDevice }
    var boundDeviceDescription: String {
        attached ? "\(inputDevice) ch=\(deviceChannels) sr=\(sampleRate)" : "(not attached)"
    }

    // MARK: - Attach / detach

    /// Open the current input device through its own AUHAL and join it to
    /// `output` (mono, at the device rate). Safe to call with the engine
    /// stopped or running — nothing here touches `engine.inputNode`.
    func attach(to engine: AVAudioEngine, output: AVAudioNode, wet: [AVAudioMixerNode] = []) {
        guard !attached else { return }
        guard let device = Self.resolveInputDevice() else {
            NSLog("MenuBand monitor: no input device to attach")
            return
        }
        guard openInputUnit(on: device) else { return }
        guard deviceChannels > 0, sampleRate > 0,
              let monoFormat = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                             sampleRate: sampleRate,
                                             channels: 1, interleaved: false) else {
            closeInputUnit()
            return
        }
        allocateRing()
        assertDeviceBuffer()
        installBufferListener(on: device)
        // Scarlett Solo: input 1 is the mic, input 2 the instrument jack —
        // an empty jack's noise floor summed 50/50 into the voice reads as
        // "fuzz". Default to channel 1 unless the user ever picked a channel.
        if !MenuBandAudioDevices.monitorChannelWasEverSet, deviceChannels == 2,
           MenuBandAudioDevices.all().first(where: { $0.id == device })?.name.localizedCaseInsensitiveContains("scarlett") == true {
            monitorChannel = 1
            NSLog("MenuBand monitor: Scarlett default → channel 1 (mic only)")
        }

        let source = AVAudioSourceNode(format: monoFormat) { [weak self] _, _, frameCount, abl -> OSStatus in
            self?.pull(frameCount: Int(frameCount), into: abl) ?? noErr
        }
        sourceNode = source
        engine.attach(source)
        engine.attach(dryBus)
        engine.attach(monoMixer)
        engine.connect(source, to: dryBus, format: monoFormat)
        engine.connect(dryBus, to: monoMixer, format: monoFormat)
        // Dry → the fast output. Never claim bus 0 of a shared mixer by
        // name — on the main mixer that is the limiter's bus, and taking it
        // unplugs every instrument.
        let outBus = (output as? AVAudioMixerNode)?.nextAvailableInputBus ?? 0
        let hp = voiceEQ.bands[0]
        hp.filterType = .highPass; hp.frequency = 80; hp.bandwidth = 0.7; hp.bypass = false
        let air = voiceEQ.bands[1]
        let cut = UserDefaults.standard.object(forKey: "notepat.voiceAirCutDb") == nil
            ? -6.0 : UserDefaults.standard.double(forKey: "notepat.voiceAirCutDb")
        air.filterType = .highShelf; air.frequency = 5000; air.gain = Float(cut); air.bypass = false
        let lp = voiceEQ.bands[2]
        lp.filterType = .lowPass; lp.frequency = 12_000; lp.bandwidth = 0.7; lp.bypass = false
        engine.attach(voiceEQ)
        engine.connect(monoMixer, to: voiceEQ, format: monoFormat)
        engine.connect(voiceEQ, to: [AVAudioConnectionPoint(node: output, bus: outBus)],
                       fromBus: 0, format: monoFormat)
        NSLog("MenuBand monitor: voice tone — high-pass 80 Hz, air shelf 6 kHz \(cut) dB")
        // Wet → its OWN reader of the ring → time-pitch → echo/space sends.
        // Time-pitch pulls its input on its own cadence; branching the dry
        // source node would drain the ring twice and underflow every other
        // cycle, so the wet path reads the ring independently.
        if !wet.isEmpty {
            let wetNode = AVAudioSourceNode(format: monoFormat) { [weak self] _, _, frameCount, abl -> OSStatus in
                self?.pullWet(frameCount: Int(frameCount), into: abl) ?? noErr
            }
            wetSource = wetNode
            engine.attach(wetNode)
            let wetPoints = wet.map { AVAudioConnectionPoint(node: $0, bus: $0.nextAvailableInputBus) }
            engine.connect(wetNode, to: wetPoints, fromBus: 0, format: monoFormat)
            wetConnected = true
        }
        monoMixer.outputVolume = 0
        attached = true

        let status = AudioOutputUnitStart(inputUnit!)
        NSLog("MenuBand monitor: direct device input on \(device) ch=\(deviceChannels) sr=\(sampleRate) (start \(status))")
        startHealthTimer()
    }

    /// Unwire the monitor graph and close the input unit. Callers may stop
    /// the engine around this; the source node detaches cleanly either way.
    func detach(from engine: AVAudioEngine) {
        guard attached else { return }
        stopHealthTimer()
        removeDryTap()
        removeBufferListener()
        closeInputUnit()
        if let source = sourceNode {
            engine.disconnectNodeOutput(source)
            engine.detach(source)
            sourceNode = nil
        }
        engine.disconnectNodeOutput(dryBus)
        engine.disconnectNodeOutput(monoMixer)
        engine.disconnectNodeOutput(voiceEQ)
        engine.detach(voiceEQ)
        if wetConnected {
            if let w = wetSource {
                engine.disconnectNodeOutput(w)
                engine.detach(w)
                wetSource = nil
            }
            wetConnected = false
        }
        engine.detach(dryBus)
        engine.detach(monoMixer)
        freeRing()
        attached = false
        NSLog("MenuBand monitor: direct device input closed")
    }

    /// After the host engine recovers from a device change, the AUHAL's own
    /// stream can be stale (it stopped delivering and briefly rendered NaN on
    /// the Scarlett). Close and reopen it on the current input device; the
    /// engine-side graph (source node, buses, tap) stays as it is.
    func restartInput() {
        guard attached else { return }
        removeBufferListener()
        closeInputUnit()
        guard let device = Self.resolveInputDevice(), openInputUnit(on: device) else {
            NSLog("MenuBand monitor: input restart failed — no device")
            return
        }
        allocateRing()
        assertDeviceBuffer()
        installBufferListener(on: device)
        let status = AudioOutputUnitStart(inputUnit!)
        NSLog("MenuBand monitor: input restarted on \(device) ch=\(deviceChannels) sr=\(sampleRate) (start \(status))")
    }

    /// Write `preferredIOBufferFrames` on the input unit and the device.
    func assertDeviceBuffer() {
        guard let au = inputUnit, inputDevice != 0 else { return }
        var frames = preferredIOBufferFrames
        _ = AudioUnitSetProperty(au, AudioUnitPropertyID(kAudioDevicePropertyBufferFrameSize),
                                 kAudioUnitScope_Global, 0, &frames, 4)
        var addr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyBufferFrameSize,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        let status = AudioObjectSetPropertyData(inputDevice, &addr, 0, nil, 4, &frames)
        var now: UInt32 = 0
        var size: UInt32 = 4
        AudioObjectGetPropertyData(inputDevice, &addr, 0, nil, &size, &now)
        // Cushion: two buffers below 128 frames (a 1.45 ms cycle can't
        // absorb the jitter between the mic unit's thread and the engine's
        // render with one), one buffer from 128 up.
        let lead = Int(max(32, min(1024, now < 128 ? now * 2 : now)))
        os_unfair_lock_lock(ringLock)
        leadFrames = lead
        os_unfair_lock_unlock(ringLock)
        NSLog("MenuBand monitor: device IO buffer asserted \(frames) → now \(now) (status \(status)); ring lead \(lead)")
    }

    /// Re-assert when anything else moves the device's buffer while we
    /// monitor. Runs on our own queue (never on the HAL IO thread).
    private func installBufferListener(on device: AudioDeviceID) {
        guard !bufferListenerInstalled else { return }
        var addr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyBufferFrameSize,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        let block: AudioObjectPropertyListenerBlock = { [weak self] _, _ in
            guard let self, self.attached, self.inputDevice != 0 else { return }
            var now: UInt32 = 0
            var size: UInt32 = 4
            var a = AudioObjectPropertyAddress(
                mSelector: kAudioDevicePropertyBufferFrameSize,
                mScope: kAudioObjectPropertyScopeGlobal,
                mElement: kAudioObjectPropertyElementMain)
            guard AudioObjectGetPropertyData(self.inputDevice, &a, 0, nil, &size, &now) == noErr,
                  now != self.preferredIOBufferFrames else { return }
            let t = ProcessInfo.processInfo.systemUptime
            self.reassertTimes = self.reassertTimes.filter { t - $0 < 10 }
            guard self.reassertTimes.count < 3 else {
                NSLog("MenuBand monitor: device buffer moved to \(now) — leaving it (re-assert budget spent)")
                return
            }
            self.reassertTimes.append(t)
            NSLog("MenuBand monitor: device buffer moved to \(now) — re-asserting \(self.preferredIOBufferFrames)")
            self.assertDeviceBuffer()
        }
        bufferListenerBlock = block
        let status = AudioObjectAddPropertyListenerBlock(device, &addr, bufferListenerQueue, block)
        bufferListenerInstalled = status == noErr
    }

    private func removeBufferListener() {
        guard bufferListenerInstalled, let block = bufferListenerBlock, inputDevice != 0 else { return }
        var addr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyBufferFrameSize,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        AudioObjectRemovePropertyListenerBlock(inputDevice, &addr, bufferListenerQueue, block)
        bufferListenerInstalled = false
        bufferListenerBlock = nil
    }

    /// How far the effects wheel is up (0…1). The DRY voice ducks by this
    /// much so the monitor is "just the effected part" at full wheel; the
    /// wet copy is unaffected (its level is the sends').
    private var duck: Float = 0

    private func applyDryLevel() {
        monoMixer.outputVolume = enabled ? gain * (1 - duck) : 0
    }

    func setEffectsAmount(_ amount: Float) {
        duck = max(0, min(1, amount))
        applyDryLevel()
    }

    func setEnabled(_ value: Bool) {
        enabled = value
        applyDryLevel()
    }

    /// Signed -1…+1 wheel bend → ±1 octave on the wet copy (cents).
    func setPitchBend(amount: Float) {
        let cents = max(-1200, min(1200, amount * 1200))
        os_unfair_lock_lock(ringLock)
        shiftRatio = powf(2, cents / 1200)
        shiftTargetMix = abs(cents) > 2 ? 1 : 0
        os_unfair_lock_unlock(ringLock)
    }

    func setGain(_ value: Float) {
        gain = value
        applyDryLevel()
    }

    /// Monitor/record channel pick: 0 = mix all device channels, k = only
    /// device channel k. Takes effect on the next pull — no engine bounce.
    func setMonitorChannel(_ channel: Int) {
        os_unfair_lock_lock(ringLock)
        monitorChannel = max(0, channel)
        os_unfair_lock_unlock(ringLock)
    }

    /// Dry input capture for the tape rides `installDryTap`; this hook stays
    /// for the SampleVoice fallback path, which no longer feeds it.
    func ingest(_ input: AVAudioPCMBuffer) {}

    // MARK: - Dry tap (the tape's voice stem)

    /// Pre-fader mono voice (after the channel pick, before the gain) for
    /// the tape. Returns false when the graph isn't wired.
    @discardableResult
    func installDryTap(_ handler: @escaping (AVAudioPCMBuffer) -> Void) -> Bool {
        guard attached else { return false }
        guard !dryTapInstalled else { return true }
        let format = dryBus.outputFormat(forBus: 0)
        guard format.channelCount > 0, format.sampleRate > 0 else { return false }
        dryBus.installTap(onBus: 0, bufferSize: 256, format: format) { buffer, _ in handler(buffer) }
        dryTapInstalled = true
        NSLog("MenuBand monitor: dry voice tap on (ch=\(format.channelCount) sr=\(format.sampleRate))")
        return true
    }

    func removeDryTap() {
        guard dryTapInstalled else { return }
        dryBus.removeTap(onBus: 0)
        dryTapInstalled = false
    }

    // MARK: - Device resolution

    private static func resolveInputDevice() -> AudioDeviceID? {
        if let uid = MenuBandAudioDevices.pinnedInputUID,
           let pinned = MenuBandAudioDevices.device(uid: uid),
           pinned.inputChannels > 0 {
            return pinned.id
        }
        return MenuBandAudioDevices.systemDefaultInputID()
    }

    // MARK: - AUHAL plumbing

    private func openInputUnit(on device: AudioDeviceID) -> Bool {
        var desc = AudioComponentDescription(
            componentType: kAudioUnitType_Output,
            componentSubType: kAudioUnitSubType_HALOutput,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0, componentFlagsMask: 0)
        guard let component = AudioComponentFindNext(nil, &desc) else {
            NSLog("MenuBand monitor: no HAL output component"); return false
        }
        var unit: AudioUnit?
        var status = AudioComponentInstanceNew(component, &unit)
        guard status == noErr, let au = unit else {
            NSLog("MenuBand monitor: AUHAL instance failed \(status)"); return false
        }
        func fail(_ what: String, _ s: OSStatus) -> Bool {
            NSLog("MenuBand monitor: \(what) failed \(s)")
            AudioComponentInstanceDispose(au)
            return false
        }
        var one: UInt32 = 1
        var zero: UInt32 = 0
        status = AudioUnitSetProperty(au, kAudioOutputUnitProperty_EnableIO,
                                      kAudioUnitScope_Input, 1, &one, 4)
        if status != noErr { return fail("enable input", status) }
        status = AudioUnitSetProperty(au, kAudioOutputUnitProperty_EnableIO,
                                      kAudioUnitScope_Output, 0, &zero, 4)
        if status != noErr { return fail("disable output", status) }
        var dev = device
        status = AudioUnitSetProperty(au, kAudioOutputUnitProperty_CurrentDevice,
                                      kAudioUnitScope_Global, 0, &dev,
                                      UInt32(MemoryLayout<AudioDeviceID>.size))
        if status != noErr { return fail("bind device \(device)", status) }

        // The device's own stream format on the input element tells us the
        // channel count + rate; we ask for the same rate as Float32 planes.
        var deviceFormat = AudioStreamBasicDescription()
        var size = UInt32(MemoryLayout<AudioStreamBasicDescription>.size)
        status = AudioUnitGetProperty(au, kAudioUnitProperty_StreamFormat,
                                      kAudioUnitScope_Input, 1, &deviceFormat, &size)
        if status != noErr { return fail("read device format", status) }
        let channels = Int(deviceFormat.mChannelsPerFrame)
        let rate = deviceFormat.mSampleRate
        guard channels > 0, rate > 0 else { return fail("device format unusable", -1) }
        var clientFormat = AudioStreamBasicDescription(
            mSampleRate: rate, mFormatID: kAudioFormatLinearPCM,
            mFormatFlags: kAudioFormatFlagIsFloat | kAudioFormatFlagIsPacked | kAudioFormatFlagIsNonInterleaved,
            mBytesPerPacket: 4, mFramesPerPacket: 1, mBytesPerFrame: 4,
            mChannelsPerFrame: UInt32(channels), mBitsPerChannel: 32, mReserved: 0)
        status = AudioUnitSetProperty(au, kAudioUnitProperty_StreamFormat,
                                      kAudioUnitScope_Output, 1, &clientFormat, size)
        if status != noErr { return fail("set client format", status) }
        var maxFrames = UInt32(renderCapacity)
        status = AudioUnitSetProperty(au, kAudioUnitProperty_MaximumFramesPerSlice,
                                      kAudioUnitScope_Global, 0, &maxFrames, 4)
        if status != noErr { return fail("max frames", status) }
        var callback = AURenderCallbackStruct(
            inputProc: menuBandInputRenderCallback,
            inputProcRefCon: Unmanaged.passUnretained(self).toOpaque())
        status = AudioUnitSetProperty(au, kAudioOutputUnitProperty_SetInputCallback,
                                      kAudioUnitScope_Global, 0, &callback,
                                      UInt32(MemoryLayout<AURenderCallbackStruct>.size))
        if status != noErr { return fail("input callback", status) }
        status = AudioUnitInitialize(au)
        if status != noErr { return fail("initialize", status) }

        let list = AudioBufferList.allocate(maximumBuffers: channels)
        for i in 0..<channels {
            list[i].mNumberChannels = 1
            list[i].mDataByteSize = UInt32(renderCapacity * MemoryLayout<Float>.size)
            list[i].mData = UnsafeMutableRawPointer.allocate(
                byteCount: renderCapacity * MemoryLayout<Float>.size,
                alignment: MemoryLayout<Float>.alignment)
        }
        renderList = list
        inputUnit = au
        inputDevice = device
        deviceChannels = channels
        sampleRate = rate
        return true
    }

    private func closeInputUnit() {
        if let au = inputUnit {
            AudioOutputUnitStop(au)
            AudioUnitUninitialize(au)
            AudioComponentInstanceDispose(au)
            inputUnit = nil
        }
        if let list = renderList {
            for buffer in list { buffer.mData?.deallocate() }
            free(list.unsafeMutablePointer)
            renderList = nil
        }
        inputDevice = 0
        deviceChannels = 0
        sampleRate = 0
    }

    /// Called by the AUHAL on its IO thread whenever device input is ready.
    fileprivate func captureInput(_ flags: UnsafeMutablePointer<AudioUnitRenderActionFlags>,
                                  _ timestamp: UnsafePointer<AudioTimeStamp>,
                                  _ bus: UInt32, _ frameCount: UInt32) -> OSStatus {
        guard let au = inputUnit, let list = renderList else { return noErr }
        let frames = Int(frameCount)
        guard frames > 0, frames <= renderCapacity else { return noErr }
        lastCallbackFrames = frames
        for i in 0..<list.count {
            list[i].mDataByteSize = UInt32(frames * MemoryLayout<Float>.size)
        }
        let status = AudioUnitRender(au, flags, timestamp, bus, frameCount, list.unsafeMutablePointer)
        guard status == noErr else { return status }
        os_unfair_lock_lock(ringLock)
        captures += 1
        if !ring.isEmpty {
            // Keep the shifter's window of history intact: the reader may
            // look back `shiftWindow` frames, so cap how far the writer runs
            // ahead of it accordingly.
            if writeIndex + frames - readIndex > ringFrames - shiftWindow - 2 {
                overflows += 1
                readIndex = writeIndex + frames - ringFrames / 2
            }
            for ch in 0..<min(deviceChannels, list.count) {
                guard let src = list[ch].mData?.assumingMemoryBound(to: Float.self) else { continue }
                // Never let a non-finite sample into the graph: one NaN
                // through the limiter silences the whole master until relaunch.
                for i in 0..<frames where !src[i].isFinite { src[i] = 0; nanScrubbed += 1 }
                let dst = ring[ch]
                var w = writeIndex & (ringFrames - 1)
                var remaining = frames
                var offset = 0
                while remaining > 0 {
                    let run = min(remaining, ringFrames - w)
                    dst.advanced(by: w).update(from: src.advanced(by: offset), count: run)
                    w = (w + run) & (ringFrames - 1)
                    offset += run
                    remaining -= run
                }
            }
            writeIndex += frames
        }
        os_unfair_lock_unlock(ringLock)
        return noErr
    }

    // MARK: - Source-node pull (engine render thread)

    /// Mono sample at an absolute ring position (channel pick applied),
    /// linearly interpolated for fractional reads. Caller holds the lock.
    /// `pos` is an absolute ring position in DOUBLE precision: the reader's
    /// frame counter passes 2^24 after ~6 min, where a Float can no longer
    /// hold whole numbers — reads then land between/over samples and the
    /// voice aliases more the longer the app runs.
    @inline(__always)
    private func monoSample(at pos: Double, channel: Int, channels: Int) -> Float {
        let i0 = Int(pos.rounded(.down))
        let t = Float(pos - Double(i0))
        @inline(__always) func at(_ idx: Int) -> Float {
            let w = idx & (ringFrames - 1)
            if channel == 0 {
                var s: Float = 0
                for ch in 0..<channels { s += ring[ch][w] }
                return s / Float(channels)
            }
            return ring[min(channel, channels) - 1][w]
        }
        if t == 0 { return at(i0) }
        // 4-point Hermite (Catmull-Rom): far less high-frequency smear than
        // linear when the taps glide between samples during a bend.
        let xm1 = at(i0 - 1), x0 = at(i0), x1 = at(i0 + 1), x2 = at(i0 + 2)
        let c = (x1 - xm1) * 0.5
        let v = x0 - x1
        let w = c + v
        let a = w + v + (x2 - x0) * 0.5
        let b = w + a
        return ((a * t - b) * t + c) * t + x0
    }

    private func pull(frameCount: Int, into abl: UnsafeMutablePointer<AudioBufferList>,
                      wet: Bool = false) -> OSStatus {
        let out = UnsafeMutableAudioBufferListPointer(abl)
        guard let dst = out[0].mData?.assumingMemoryBound(to: Float.self) else { return noErr }
        os_unfair_lock_lock(ringLock)
        pulls += 1
        let available = writeIndex - readIndex
        leadAccum += max(0, available)
        let plan = readCursor.plan(read: readIndex, written: writeIndex,
                                   frames: frameCount, lead: leadFrames)
        if plan.underrun { underflows += 1 }
        if plan.dropped { drops += 1 }
        readIndex = plan.start
        let take = ring.isEmpty ? 0 : plan.count
        let channel = monitorChannel
        let channels = deviceChannels
        if take > 0, !ring.isEmpty {
            let w = Float(shiftWindow)
            // Phase step per sample: tap delays slide at (1 - ratio) so the
            // read speed is `ratio` × real time. Pitch up = delay shrinking.
            let step = (1 - shiftRatio) / w
            for i in 0..<take {
                let nowIdx = readIndex + i
                let now = Double(nowIdx)
                // Direct path: exact integer read, no interpolation at all.
                let direct: Float
                if channel == 0 {
                    var sum: Float = 0
                    let wi = nowIdx & (ringFrames - 1)
                    for ch in 0..<channels { sum += ring[ch][wi] }
                    direct = sum / Float(channels)
                } else {
                    direct = ring[min(channel, channels) - 1][nowIdx & (ringFrames - 1)]
                }
                // Ramp the shifter in/out over ~256 samples so engaging the
                // wheel never clicks.
                if shiftMix != shiftTargetMix {
                    shiftMix += (shiftTargetMix - shiftMix > 0 ? 1 : -1) / 256
                    shiftMix = max(0, min(1, shiftMix))
                }
                var sample = direct
                if shiftMix > 0 {
                    shiftPhase += step
                    if shiftPhase >= 1 { shiftPhase -= 1 } else if shiftPhase < 0 { shiftPhase += 1 }
                    let p2 = shiftPhase + 0.5 > 1 ? shiftPhase - 0.5 : shiftPhase + 0.5
                    let d1 = Double(shiftPhase * w), d2 = Double(p2 * w)
                    let g1 = sinf(.pi * shiftPhase), g2 = sinf(.pi * p2)
                    let t1 = monoSample(at: now - d1, channel: channel, channels: channels)
                    let t2 = monoSample(at: now - d2, channel: channel, channels: channels)
                    let shifted = t1 * g1 + t2 * g2
                    sample = direct * (1 - shiftMix) + shifted * shiftMix
                }
                // Gate: envelope follower (5 ms up / 150 ms down at 44.1 kHz),
                // gain eases toward the floor below threshold, snaps open above.
                if gateFloor != 1 {
                    let mag = abs(sample)
                    gateEnv = mag > gateEnv ? gateEnv + (mag - gateEnv) * 0.0045 : gateEnv + (mag - gateEnv) * 0.00015
                    let target: Float = gateEnv < gateThreshold ? gateFloor : 1
                    gateGain += (target - gateGain) * (target > gateGain ? 0.0075 : 0.00022)
                    sample *= gateGain
                }
                dst[i] = sample
                if let post = postRing {
                    post[postWriteIndex & (postFrames - 1)] = sample
                    postWriteIndex += 1
                }
            }
            readIndex += take
        }
        if take < frameCount {
            for i in take..<frameCount {
                dst[i] = 0
                // Keep the effects clock aligned with the dry output even
                // while capture is recovering; never replay an old tail.
                if let post = postRing {
                    post[postWriteIndex & (postFrames - 1)] = 0
                    postWriteIndex += 1
                }
            }
        }
        os_unfair_lock_unlock(ringLock)
        return noErr
    }

    /// Wet reader: the dry reader's output, a little behind, scaled by the
    /// monitor's enable/gain (the dry path has those on `monoMixer`).
    private func pullWet(frameCount: Int, into abl: UnsafeMutablePointer<AudioBufferList>) -> OSStatus {
        let out = UnsafeMutableAudioBufferListPointer(abl)
        guard let dst = out[0].mData?.assumingMemoryBound(to: Float.self) else { return noErr }
        os_unfair_lock_lock(ringLock)
        let scale: Float = enabled ? gain : 0
        var available = postWriteIndex - wetReadIndex
        if available < frameCount {
            wetUnderflows += 1
        } else if available > leadFrames * 8 + frameCount {
            wetReadIndex = postWriteIndex - leadFrames * 4
            available = postWriteIndex - wetReadIndex
        }
        let take = min(frameCount, max(0, available))
        if take > 0, let post = postRing {
            for i in 0..<take { dst[i] = post[(wetReadIndex + i) & (postFrames - 1)] * scale }
            wetReadIndex += take
        }
        os_unfair_lock_unlock(ringLock)
        if take < frameCount { for i in take..<frameCount { dst[i] = 0 } }
        return noErr
    }

    private func logStatsIfDue(force: Bool = false) {
        let now = ProcessInfo.processInfo.systemUptime
        guard force || now - lastStatsLog >= 4.9 else { return }
        if !force && now - lastStatsLog < 4.9 { return }
        lastStatsLog = now
        // Snapshot and clear counters together. The old watchdog raced the
        // two audio callbacks and could erase increments while reporting.
        os_unfair_lock_lock(ringLock)
        let (u, o, d, p, n, wu, c) = (underflows, overflows, drops, pulls, nanScrubbed, wetUnderflows, captures)
        let avgLead = p > 0 ? leadAccum / p : 0
        wetUnderflows = 0
        captures = 0
        underflows = 0
        overflows = 0
        drops = 0
        leadAccum = 0
        pulls = 0
        nanScrubbed = 0
        os_unfair_lock_unlock(ringLock)
        if attached, c == 0 {
            stalledIntervals += 1
            NSLog("MenuBand monitor: WATCHDOG — no input callbacks for \(stalledIntervals * 5) s on device \(inputDevice); reopening on the live input")
            watchdogQueue.async { [weak self] in
                DispatchQueue.main.async { self?.restartInput() }
            }
        } else {
            stalledIntervals = 0
        }
        let leadMs = sampleRate > 0 ? Double(avgLead) / sampleRate * 1000 : 0
        NSLog(String(format: "MenuBand monitor: health dev=%u captures=%d lead=%d frames (%.2f ms) pulls=%d underflows=%d drops=%d overflows=%d nan=%d wetUnderflows=%d enabled=%d gain=%.2f duck=%.2f (5 s)",
                     inputDevice, c, avgLead, leadMs, p, u, d, o, n, wu, enabled ? 1 : 0, gain, duck))
    }

    // MARK: - Ring memory

    private func allocateRing() {
        freeRing()
        os_unfair_lock_lock(ringLock)
        ring = (0..<deviceChannels).map { _ in
            let p = UnsafeMutablePointer<Float>.allocate(capacity: ringFrames)
            p.initialize(repeating: 0, count: ringFrames)
            return p
        }
        // Primed silence: the dry reader starts one lead behind the writer
        // (plus the shifter's look-back window of history); the wet reader
        // starts four leads behind the dry reader's output.
        writeIndex = shiftWindow + leadFrames
        readIndex = shiftWindow
        readCursor = MonitorReadCursor()
        postRing?.deallocate()
        let post = UnsafeMutablePointer<Float>.allocate(capacity: postFrames)
        post.initialize(repeating: 0, count: postFrames)
        postRing = post
        postWriteIndex = leadFrames * 4
        wetReadIndex = 0
        shiftPhase = 0
        shiftMix = 0
        os_unfair_lock_unlock(ringLock)
    }

    private func freeRing() {
        os_unfair_lock_lock(ringLock)
        for p in ring { p.deallocate() }
        ring = []
        postRing?.deallocate()
        postRing = nil
        writeIndex = 0
        readIndex = 0
        postWriteIndex = 0
        wetReadIndex = 0
        os_unfair_lock_unlock(ringLock)
    }

    deinit {
        closeInputUnit()
        freeRing()
        ringLock.deallocate()
    }
}

/// C-convention trampoline for the AUHAL input callback.
private let menuBandInputRenderCallback: AURenderCallback = {
    refCon, ioActionFlags, inTimeStamp, inBusNumber, inNumberFrames, _ in
    let monitor = Unmanaged<MenuBandInputMonitor>.fromOpaque(refCon).takeUnretainedValue()
    return monitor.captureInput(ioActionFlags, inTimeStamp, inBusNumber, inNumberFrames)
}

extension Notification.Name {
    static let menuBandInputMonitoringChanged =
        Notification.Name("MenuBandInputMonitoringChanged")
}
