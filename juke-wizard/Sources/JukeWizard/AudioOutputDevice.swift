import AppKit
import CoreAudio

/// The same Core Audio output-device surface used by macOS Sound settings.
/// Selecting here intentionally changes the Mac's default output so every
/// JukeWizard source (AVAudioPlayer, juked/Spotify, and room receivers) agrees.
enum MacAudioOutput {
    struct Device: Equatable {
        let id: AudioDeviceID
        let uid: String
        let name: String
        let transport: UInt32

        var symbolName: String {
            switch transport {
            case kAudioDeviceTransportTypeBluetooth, kAudioDeviceTransportTypeBluetoothLE:
                return "headphones"
            case kAudioDeviceTransportTypeUSB, kAudioDeviceTransportTypeThunderbolt:
                return "cable.connector"
            case kAudioDeviceTransportTypeAirPlay:
                return "airplayaudio"
            case kAudioDeviceTransportTypeHDMI, kAudioDeviceTransportTypeDisplayPort:
                return "display"
            case kAudioDeviceTransportTypeBuiltIn:
                return "laptopcomputer"
            default:
                return "speaker.wave.2"
            }
        }
    }

    struct DeviceError: LocalizedError {
        let action: String
        let status: OSStatus
        var errorDescription: String? { "Could not \(action) (Core Audio \(status))" }
    }

    static func devices() -> [Device] {
        let address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDevices,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        guard let ids = deviceIDs(address: address) else { return [] }
        let active = defaultDeviceID()
        return ids.compactMap { id in
            guard outputChannelCount(id) > 0 else { return nil }
            let name = string(id, selector: kAudioObjectPropertyName) ?? "Audio Device"
            let uid = string(id, selector: kAudioDevicePropertyDeviceUID) ?? String(id)
            let transport = uint32(id, selector: kAudioDevicePropertyTransportType) ?? 0
            return Device(id: id, uid: uid, name: name, transport: transport)
        }.sorted {
            if $0.id == active { return true }
            if $1.id == active { return false }
            return $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending
        }
    }

    static func defaultDeviceID() -> AudioDeviceID? {
        uint32(AudioObjectID(kAudioObjectSystemObject),
               selector: kAudioHardwarePropertyDefaultOutputDevice)
    }

    static func select(_ device: Device) throws {
        try setSystemDevice(device.id, selector: kAudioHardwarePropertyDefaultOutputDevice,
                            action: "select \(device.name)")
        // Alert sounds should follow the same destination, matching the Sound
        // control-center selector. Some virtual devices reject this secondary
        // property, so the main music route remains authoritative.
        try? setSystemDevice(device.id, selector: kAudioHardwarePropertyDefaultSystemOutputDevice,
                             action: "route system sounds")
    }

    private static func setSystemDevice(_ id: AudioDeviceID, selector: AudioObjectPropertySelector,
                                        action: String) throws {
        var mutableID = id
        var address = AudioObjectPropertyAddress(
            mSelector: selector,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        let status = AudioObjectSetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address,
                                                0, nil,
                                                UInt32(MemoryLayout<AudioDeviceID>.size), &mutableID)
        guard status == noErr else { throw DeviceError(action: action, status: status) }
    }

    private static func outputChannelCount(_ id: AudioDeviceID) -> UInt32 {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyStreamConfiguration,
            mScope: kAudioObjectPropertyScopeOutput,
            mElement: kAudioObjectPropertyElementMain)
        var size: UInt32 = 0
        guard AudioObjectGetPropertyDataSize(id, &address, 0, nil, &size) == noErr, size > 0 else { return 0 }
        let raw = UnsafeMutableRawPointer.allocate(
            byteCount: Int(size), alignment: MemoryLayout<AudioBufferList>.alignment)
        defer { raw.deallocate() }
        guard AudioObjectGetPropertyData(id, &address, 0, nil, &size, raw) == noErr else { return 0 }
        return UnsafeMutableAudioBufferListPointer(raw.assumingMemoryBound(to: AudioBufferList.self))
            .reduce(0) { $0 + $1.mNumberChannels }
    }

    private static func string(_ id: AudioObjectID,
                               selector: AudioObjectPropertySelector) -> String? {
        var address = AudioObjectPropertyAddress(
            mSelector: selector,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var value: Unmanaged<CFString>?
        var size = UInt32(MemoryLayout<Unmanaged<CFString>?>.size)
        guard AudioObjectGetPropertyData(id, &address, 0, nil, &size, &value) == noErr,
              let value else { return nil }
        return value.takeUnretainedValue() as String
    }

    private static func uint32(_ id: AudioObjectID,
                               selector: AudioObjectPropertySelector) -> UInt32? {
        var address = AudioObjectPropertyAddress(
            mSelector: selector,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var value: UInt32 = 0
        var size = UInt32(MemoryLayout<UInt32>.size)
        let status = AudioObjectGetPropertyData(id, &address, 0, nil, &size, &value)
        return status == noErr ? value : nil
    }

    private static func deviceIDs(address original: AudioObjectPropertyAddress) -> [AudioDeviceID]? {
        var address = original
        var size: UInt32 = 0
        let system = AudioObjectID(kAudioObjectSystemObject)
        guard AudioObjectGetPropertyDataSize(system, &address, 0, nil, &size) == noErr else { return nil }
        var output = [AudioDeviceID](repeating: 0,
                                    count: Int(size) / MemoryLayout<AudioDeviceID>.size)
        let status = output.withUnsafeMutableBytes { bytes in
            AudioObjectGetPropertyData(system, &address, 0, nil, &size, bytes.baseAddress!)
        }
        return status == noErr ? output : nil
    }
}

/// Refreshes the hardware list immediately before AppKit opens the menu, so a
/// newly connected Bluetooth headset appears without relaunching JukeWizard.
final class AudioOutputPopUpButton: NSPopUpButton {
    var prepareMenu: (() -> Void)?
    override func mouseDown(with event: NSEvent) {
        prepareMenu?()
        super.mouseDown(with: event)
    }
}
