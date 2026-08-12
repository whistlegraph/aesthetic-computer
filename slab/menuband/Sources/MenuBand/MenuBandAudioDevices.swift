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
