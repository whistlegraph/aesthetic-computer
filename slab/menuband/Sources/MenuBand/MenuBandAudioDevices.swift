import AVFoundation
import Foundation
import CoreAudio
import AudioToolbox

/// One CoreAudio hardware device as the headphone icon's right-click
/// picker sees it. `uid` is the persistence key — AudioDeviceIDs are
/// transient and renumber across replug/reboot, the UID survives both.
struct MBAudioDevice {
    let id: AudioDeviceID
    let uid: String
    let name: String
    let inputChannels: Int
    let outputChannels: Int
}

/// Device enumeration plus the user's persisted audio-routing picks.
///
/// Menu Band's engines otherwise follow the system defaults (with a
/// Focusrite/Scarlett auto-preference on the record path). The headphone
/// icon's right-click menu writes the picks here; the engine plumbing
/// reads them:
///
///  - input pick    → re-resolved through `MenuBandSampleVoice`'s device
///    preference, which moves the SYSTEM default input (the mechanism the
///    Scarlett auto-select has always used) so the duplex-monitor engine
///    and the record engine both follow through their existing
///    configuration-change recovery.
///  - output pick   → bound directly onto the playback engine's output AU
///    by `applyOutputDeviceOverride`, so the user's system-wide output
///    setting stays untouched.
///  - monitor channel → AUHAL channel map on both input AUs (0 = mix all).
enum MenuBandAudioDevices {
    private static let inputUIDKey = "MBAudioInputDeviceUID"
    private static let outputUIDKey = "MBAudioOutputDeviceUID"
    private static let monitorChannelKey = "MBMonitorInputChannel"

    /// User-pinned input device UID. nil = automatic (prefer a Focusrite/
    /// Scarlett when attached, otherwise leave the system default alone).
    static var pinnedInputUID: String? {
        get { UserDefaults.standard.string(forKey: inputUIDKey) }
        set {
            if let newValue {
                UserDefaults.standard.set(newValue, forKey: inputUIDKey)
            } else {
                UserDefaults.standard.removeObject(forKey: inputUIDKey)
            }
        }
    }

    /// User-pinned output device UID. nil = follow the system default
    /// output (the launch behavior).
    static var pinnedOutputUID: String? {
        get { UserDefaults.standard.string(forKey: outputUIDKey) }
        set {
            if let newValue {
                UserDefaults.standard.set(newValue, forKey: outputUIDKey)
            } else {
                UserDefaults.standard.removeObject(forKey: outputUIDKey)
            }
        }
    }

    /// 1-based device input channel to monitor/record; 0 = mix of all
    /// channels (the pre-picker behavior).
    static var monitorChannel: Int {
        get { UserDefaults.standard.integer(forKey: monitorChannelKey) }
        set { UserDefaults.standard.set(newValue, forKey: monitorChannelKey) }
    }

    /// True once the user has ever touched the channel pick. Guards the
    /// channel-map writes so an untouched preference never writes at all —
    /// the AUHAL default (identity) IS the mix behavior, and a redundant
    /// property write can still emit a configuration-change echo.
    static var monitorChannelWasEverSet: Bool {
        UserDefaults.standard.object(forKey: monitorChannelKey) != nil
    }

    // MARK: - Enumeration

    /// Every hardware device that can play or capture audio, in CoreAudio
    /// order. Devices reporting zero channels both ways (some virtual
    /// drivers between states) are dropped.
    static func all() -> [MBAudioDevice] {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDevices,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var bytes: UInt32 = 0
        guard AudioObjectGetPropertyDataSize(
            AudioObjectID(kAudioObjectSystemObject), &address,
            0, nil, &bytes) == noErr, bytes > 0 else { return [] }
        var ids = [AudioDeviceID](
            repeating: 0, count: Int(bytes) / MemoryLayout<AudioDeviceID>.size)
        guard AudioObjectGetPropertyData(
            AudioObjectID(kAudioObjectSystemObject), &address,
            0, nil, &bytes, &ids) == noErr else { return [] }
        return ids.compactMap { id in
            guard let uid = deviceUID(id), let name = deviceName(id) else { return nil }
            let inputs = channelCount(id, scope: kAudioDevicePropertyScopeInput)
            let outputs = channelCount(id, scope: kAudioDevicePropertyScopeOutput)
            guard inputs > 0 || outputs > 0 else { return nil }
            return MBAudioDevice(
                id: id, uid: uid, name: name,
                inputChannels: inputs, outputChannels: outputs)
        }
    }

    static func device(uid: String) -> MBAudioDevice? {
        all().first { $0.uid == uid }
    }

    /// UID for a live AudioDeviceID. IDs renumber across replug — and a
    /// USB re-enumeration briefly lists the dying and fresh entries for
    /// the same interface side by side — so identity comparisons between
    /// devices must go through the UID, never the raw ID.
    static func uid(for id: AudioDeviceID) -> String? {
        deviceUID(id)
    }

    static func systemDefaultInputID() -> AudioDeviceID? {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultInputDevice,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var id = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        guard AudioObjectGetPropertyData(
            AudioObjectID(kAudioObjectSystemObject), &address,
            0, nil, &size, &id) == noErr, id != 0 else { return nil }
        return id
    }

    static func systemDefaultOutputID() -> AudioDeviceID? {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultOutputDevice,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var id = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        guard AudioObjectGetPropertyData(
            AudioObjectID(kAudioObjectSystemObject), &address,
            0, nil, &size, &id) == noErr, id != 0 else { return nil }
        return id
    }

    // MARK: - Channel map

    /// AUHAL channel-map write shared by the duplex-monitor engine and the
    /// record engine. `clientChannels` is the channel count of the stream
    /// the engine pulls from the AU's input element; each client channel is
    /// pointed at the picked 0-based device channel (clamped to what the
    /// device has), or restored to the identity mapping for the mix. Only
    /// takes effect on an uninitialized AUHAL, so callers write it while
    /// their engine is stopped.
    static func applyChannelMap(to au: AudioUnit, clientChannels: Int, label: String) {
        guard monitorChannelWasEverSet, clientChannels > 0 else { return }
        let channel = monitorChannel
        var map = [Int32](repeating: 0, count: clientChannels)
        for i in 0..<clientChannels {
            map[i] = channel == 0 ? Int32(i) : Int32(min(channel, clientChannels) - 1)
        }
        let status = AudioUnitSetProperty(
            au, kAudioOutputUnitProperty_ChannelMap,
            kAudioUnitScope_Output, 1,
            &map, UInt32(clientChannels * MemoryLayout<Int32>.size))
        NSLog("MenuBand audio: \(label) input channel map → \(channel == 0 ? "mix" : "ch \(channel)") (status \(status))")
    }

    // MARK: - Preferred interface sample rate

    /// The rate Menu Band runs a Focusrite at. 44.1 kHz by default while we
    /// test whether the 48 kHz output stream is what dies under usbaudiod
    /// (macOS 26). Applied at launch before the engine starts and restored
    /// by `resetInterface`.
    static var preferredInterfaceRate: Double {
        let v = UserDefaults.standard.double(forKey: "notepat.interfaceSampleRate")
        return v > 0 ? v : 44_100
    }

    static func nominalRate(of id: AudioDeviceID) -> Double {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyNominalSampleRate,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var rate: Double = 0
        var size = UInt32(MemoryLayout<Double>.size)
        AudioObjectGetPropertyData(id, &address, 0, nil, &size, &rate)
        return rate
    }

    /// Put the interface on the preferred rate (synchronously, ~1 s settle
    /// if it changes). Returns true when a change was made.
    @discardableResult
    static func applyPreferredRate(to id: AudioDeviceID) -> Bool {
        let want = preferredInterfaceRate
        let have = nominalRate(of: id)
        guard have > 0, abs(have - want) > 1 else { return false }
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyNominalSampleRate,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var rate = want
        let status = AudioObjectSetPropertyData(id, &address, 0, nil, UInt32(MemoryLayout<Double>.size), &rate)
        NSLog("MenuBand audio: interface \(id) rate \(have) → \(want) (status \(status))")
        Thread.sleep(forTimeInterval: 0.5)
        return status == noErr
    }

    // MARK: - Raw-device binding for helper engines

    /// On macOS 26 EVERY AVAudioEngine builds a private aggregate device
    /// around the default in/out the moment its I/O unit exists, and running
    /// IO through that aggregate restarts the Scarlett's USB streams — the
    /// output dies until a rebuild. Pin the engine's I/O unit to the raw
    /// default output (or the user's pick) BEFORE it starts, so the aggregate
    /// is never the device it runs on. Call this right before `start()`.
    @discardableResult
    static func bindToRawOutput(_ engine: AVAudioEngine, label: String) -> Bool {
        guard let au = engine.outputNode.audioUnit else { return false }
        var target: AudioDeviceID = 0
        if let uid = pinnedOutputUID, let dev = device(uid: uid), dev.outputChannels > 0 {
            target = dev.id
        } else {
            target = systemDefaultOutputID() ?? 0
        }
        guard target != 0 else { return false }
        var current = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        if AudioUnitGetProperty(au, kAudioOutputUnitProperty_CurrentDevice,
                                kAudioUnitScope_Global, 0, &current, &size) == noErr, current == target {
            return true
        }
        var dev = target
        let status = AudioUnitSetProperty(au, kAudioOutputUnitProperty_CurrentDevice,
                                          kAudioUnitScope_Global, 0, &dev, size)
        NSLog("MenuBand audio: \(label) engine → raw device \(target) (was \(current), status \(status))")
        return status == noErr
    }

    // MARK: - Interface reset

    /// Kick a USB interface whose output has gone dead while macOS still
    /// reports its streams active (Scarlett Solo on macOS 26 after a few
    /// bus blinks): flipping the nominal sample rate makes usbaudiod tear
    /// down and rebuild both streams — what a replug does, without the plug.
    /// Runs off the main thread; the engine absorbs the resulting
    /// configuration change like any device switch.
    private static let resetQueue = DispatchQueue(label: "menuband.interface-reset", qos: .userInitiated)
    private static let resetCoordinator = InterfaceResetCoordinator(
        perform: { id, completion in performInterfaceReset(id, completion: completion) },
        busyChanged: { busy in
            NotificationCenter.default.post(name: .menuBandInterfaceResetting, object: busy)
        })

    static func resetInterface(_ id: AudioDeviceID, completion: ((Bool) -> Void)? = nil) {
        if Thread.isMainThread {
            resetCoordinator.request(id, completion: completion)
        } else {
            DispatchQueue.main.async { resetCoordinator.request(id, completion: completion) }
        }
    }

    private static func performInterfaceReset(_ id: AudioDeviceID, completion: @escaping (Bool) -> Void) {
        resetQueue.async {
            var address = AudioObjectPropertyAddress(
                mSelector: kAudioDevicePropertyNominalSampleRate,
                mScope: kAudioObjectPropertyScopeGlobal,
                mElement: kAudioObjectPropertyElementMain)
            var rate: Double = 0
            var size = UInt32(MemoryLayout<Double>.size)
            guard AudioObjectGetPropertyData(id, &address, 0, nil, &size, &rate) == noErr, rate > 0 else {
                NSLog("MenuBand audio: interface reset — device \(id) has no nominal rate")
                DispatchQueue.main.async { completion(false) }
                return
            }
            let want = preferredInterfaceRate
            var other: Double = want == 44_100 ? 48_000 : 44_100
            let s1 = AudioObjectSetPropertyData(id, &address, 0, nil, size, &other)
            // Release the worker during USB teardown. The coordinator keeps
            // this device in flight until both the flip and settling finish.
            let originalRate = rate, alternateRate = other
            resetQueue.asyncAfter(deadline: .now() + 0.4) {
                var restoreAddress = AudioObjectPropertyAddress(
                    mSelector: kAudioDevicePropertyNominalSampleRate,
                    mScope: kAudioObjectPropertyScopeGlobal,
                    mElement: kAudioObjectPropertyElementMain)
                var back = want
                let s2 = AudioObjectSetPropertyData(id, &restoreAddress, 0, nil,
                                                    UInt32(MemoryLayout<Double>.size), &back)
                NSLog("MenuBand audio: interface reset on \(id): \(originalRate) → \(alternateRate) (\(s1)) → \(want) (\(s2))")
                DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
                    completion(s1 == noErr && s2 == noErr)
                }
            }
        }
    }

    // MARK: - Device-list observation

    private static var deviceListQueue = DispatchQueue(label: "menuband.devicelist")
    private static var deviceListBlock: AudioObjectPropertyListenerBlock?

    /// Fire `handler` on the main thread (debounced ~0.8 s) whenever the
    /// system's audio device list changes — plug/unplug of an interface.
    static func observeDeviceList(_ handler: @escaping () -> Void) {
        guard deviceListBlock == nil else { return }
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDevices,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var pending: DispatchWorkItem?
        let block: AudioObjectPropertyListenerBlock = { _, _ in
            pending?.cancel()
            let work = DispatchWorkItem { handler() }
            pending = work
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.8, execute: work)
        }
        deviceListBlock = block
        AudioObjectAddPropertyListenerBlock(
            AudioObjectID(kAudioObjectSystemObject), &address, deviceListQueue, block)
    }

    /// True when a Focusrite/Scarlett interface with inputs is attached.
    static func focusriteInputPresent() -> Bool {
        all().contains { $0.inputChannels > 0 &&
            ($0.name.localizedCaseInsensitiveContains("scarlett") ||
             $0.name.localizedCaseInsensitiveContains("focusrite")) }
    }

    // MARK: - Property plumbing

    private static func deviceUID(_ id: AudioDeviceID) -> String? {
        var value: Unmanaged<CFString>?
        var size = UInt32(MemoryLayout<Unmanaged<CFString>?>.size)
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyDeviceUID,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        guard AudioObjectGetPropertyData(
            id, &address, 0, nil, &size, &value) == noErr,
              let uid = value?.takeRetainedValue() else { return nil }
        return uid as String
    }

    private static func deviceName(_ id: AudioDeviceID) -> String? {
        var value: Unmanaged<CFString>?
        var size = UInt32(MemoryLayout<Unmanaged<CFString>?>.size)
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioObjectPropertyName,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        guard AudioObjectGetPropertyData(
            id, &address, 0, nil, &size, &value) == noErr,
              let name = value?.takeRetainedValue() else { return nil }
        return name as String
    }

    /// Sum of stream channels on one side of the device. This is the
    /// per-stream `AudioBufferList` walk — `kAudioDevicePropertyStreams`
    /// only says a side exists, this says how wide it is.
    private static func channelCount(
        _ id: AudioDeviceID, scope: AudioObjectPropertyScope) -> Int {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyStreamConfiguration,
            mScope: scope,
            mElement: kAudioObjectPropertyElementMain)
        var size: UInt32 = 0
        guard AudioObjectGetPropertyDataSize(
            id, &address, 0, nil, &size) == noErr, size > 0 else { return 0 }
        let raw = UnsafeMutableRawPointer.allocate(
            byteCount: Int(size),
            alignment: MemoryLayout<AudioBufferList>.alignment)
        defer { raw.deallocate() }
        guard AudioObjectGetPropertyData(
            id, &address, 0, nil, &size, raw) == noErr else { return 0 }
        let list = raw.assumingMemoryBound(to: AudioBufferList.self)
        return UnsafeMutableAudioBufferListPointer(list)
            .reduce(0) { $0 + Int($1.mNumberChannels) }
    }
}


extension Notification.Name {
    /// object = true while the interface's streams are being rebuilt, false when settled.
    static let menuBandInterfaceResetting = Notification.Name("MenuBandInterfaceResetting")
}
