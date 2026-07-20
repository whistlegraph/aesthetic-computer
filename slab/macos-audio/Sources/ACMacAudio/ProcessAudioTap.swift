import AppKit
import AudioToolbox
import AVFoundation
import CoreAudio
import Foundation

/// A first-party macOS process tap. It captures one app without changing the
/// system output device and can mute only that app while room receivers play.
@available(macOS 14.2, *)
public final class ACProcessAudioTap: @unchecked Sendable {
    public typealias AudioHandler = @Sendable (AVAudioPCMBuffer) -> Void

    public let applicationName: String
    public let processID: pid_t?
    public let muteOriginal: Bool
    public var onLog: (@Sendable (String) -> Void)?

    private let queue = DispatchQueue(label: "computer.aesthetic.process-audio-tap", qos: .userInitiated)
    private var tapID = AudioObjectID(kAudioObjectUnknown)
    private var aggregateID = AudioObjectID(kAudioObjectUnknown)
    private var ioProcID: AudioDeviceIOProcID?
    private var format: AVAudioFormat?
    private var handler: AudioHandler?

    public init(applicationName: String, muteOriginal: Bool = true) {
        self.applicationName = applicationName
        self.processID = nil
        self.muteOriginal = muteOriginal
    }

    /// Capture a non-AppKit process such as a headless audio daemon.
    public init(processID: pid_t, name: String, muteOriginal: Bool = true) {
        self.applicationName = name
        self.processID = processID
        self.muteOriginal = muteOriginal
    }

    public func start(handler: @escaping AudioHandler) throws {
        guard tapID == kAudioObjectUnknown else { return }
        let objectID = try processID.map(Self.processObjectID(pid:))
            ?? Self.processObjectID(named: applicationName)
        let description = CATapDescription(stereoMixdownOfProcesses: [objectID])
        description.uuid = UUID()
        description.name = "Aesthetic Room · \(applicationName)"
        description.muteBehavior = muteOriginal ? .mutedWhenTapped : .unmuted
        description.isPrivate = true

        var createdTap = AudioObjectID(kAudioObjectUnknown)
        var status = AudioHardwareCreateProcessTap(description, &createdTap)
        guard status == noErr else { throw ACAudioRoomError.coreAudio("Create \(applicationName) process tap", status) }
        tapID = createdTap

        do {
            var stream = try Self.read(tapID, selector: kAudioTapPropertyFormat,
                                      defaultValue: AudioStreamBasicDescription())
            guard let format = AVAudioFormat(streamDescription: &stream) else {
                throw ACAudioRoomError.unavailable("Core Audio returned an unreadable tap format")
            }
            self.format = format
            let outputID: AudioDeviceID = try Self.read(
                AudioObjectID(kAudioObjectSystemObject),
                selector: kAudioHardwarePropertyDefaultSystemOutputDevice,
                defaultValue: AudioDeviceID(kAudioObjectUnknown))
            let outputUID = try Self.readString(outputID, selector: kAudioDevicePropertyDeviceUID)
            let aggregateDescription: [String: Any] = [
                kAudioAggregateDeviceNameKey: "Aesthetic Room Tap",
                kAudioAggregateDeviceUIDKey: "computer.aesthetic.audio-room.\(UUID().uuidString)",
                kAudioAggregateDeviceMainSubDeviceKey: outputUID,
                kAudioAggregateDeviceIsPrivateKey: true,
                kAudioAggregateDeviceIsStackedKey: false,
                kAudioAggregateDeviceTapAutoStartKey: true,
                kAudioAggregateDeviceSubDeviceListKey: [[kAudioSubDeviceUIDKey: outputUID]],
                kAudioAggregateDeviceTapListKey: [[
                    kAudioSubTapUIDKey: description.uuid.uuidString,
                    kAudioSubTapDriftCompensationKey: true,
                ]],
            ]
            status = AudioHardwareCreateAggregateDevice(aggregateDescription as CFDictionary, &aggregateID)
            guard status == noErr else { throw ACAudioRoomError.coreAudio("Create tap aggregate", status) }
            self.handler = handler
            status = AudioDeviceCreateIOProcIDWithBlock(&ioProcID, aggregateID, queue) {
                [weak self] _, inputData, _, _, _ in
                guard let self, let format = self.format,
                      let buffer = AVAudioPCMBuffer(pcmFormat: format, bufferListNoCopy: inputData,
                                                    deallocator: nil) else { return }
                self.handler?(buffer)
            }
            guard status == noErr else { throw ACAudioRoomError.coreAudio("Create tap callback", status) }
            status = AudioDeviceStart(aggregateID, ioProcID)
            guard status == noErr else { throw ACAudioRoomError.coreAudio("Start process tap", status) }
            onLog?("capturing \(applicationName) at \(Int(format.sampleRate)) Hz")
        } catch {
            stop()
            throw error
        }
    }

    public func stop() {
        if aggregateID != kAudioObjectUnknown {
            _ = AudioDeviceStop(aggregateID, ioProcID)
            if let ioProcID { _ = AudioDeviceDestroyIOProcID(aggregateID, ioProcID) }
            _ = AudioHardwareDestroyAggregateDevice(aggregateID)
        }
        if tapID != kAudioObjectUnknown { _ = AudioHardwareDestroyProcessTap(tapID) }
        ioProcID = nil
        aggregateID = kAudioObjectUnknown
        tapID = kAudioObjectUnknown
        format = nil
        handler = nil
    }

    private static func processObjectID(named name: String) throws -> AudioObjectID {
        guard let app = NSWorkspace.shared.runningApplications.first(where: {
            $0.localizedName?.localizedCaseInsensitiveCompare(name) == .orderedSame ||
            $0.bundleIdentifier?.localizedCaseInsensitiveContains(name) == true
        }) else {
            throw ACAudioRoomError.unavailable("\(name) is not running")
        }
        return try processObjectID(pid: app.processIdentifier)
    }

    private static func processObjectID(pid sourcePID: pid_t) throws -> AudioObjectID {
        var pid = sourcePID
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyTranslatePIDToProcessObject,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var size = UInt32(MemoryLayout<AudioObjectID>.size)
        var result = AudioObjectID(kAudioObjectUnknown)
        let status = withUnsafePointer(to: &pid) { qualifier in
            AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address,
                                       UInt32(MemoryLayout<pid_t>.size), qualifier, &size, &result)
        }
        guard status == noErr, result != kAudioObjectUnknown else {
            throw ACAudioRoomError.coreAudio("Resolve PID \(sourcePID) audio process", status)
        }
        return result
    }

    private static func read<T>(_ object: AudioObjectID, selector: AudioObjectPropertySelector,
                                defaultValue: T) throws -> T {
        var address = AudioObjectPropertyAddress(mSelector: selector,
                                                 mScope: kAudioObjectPropertyScopeGlobal,
                                                 mElement: kAudioObjectPropertyElementMain)
        var size = UInt32(MemoryLayout<T>.size)
        var value = defaultValue
        let status = withUnsafeMutableBytes(of: &value) { storage in
            AudioObjectGetPropertyData(object, &address, 0, nil, &size, storage.baseAddress!)
        }
        guard status == noErr else { throw ACAudioRoomError.coreAudio("Read Core Audio property", status) }
        return value
    }

    private static func readString(_ object: AudioObjectID,
                                   selector: AudioObjectPropertySelector) throws -> CFString {
        var address = AudioObjectPropertyAddress(mSelector: selector,
                                                 mScope: kAudioObjectPropertyScopeGlobal,
                                                 mElement: kAudioObjectPropertyElementMain)
        var size = UInt32(MemoryLayout<CFString?>.size)
        var value: CFString?
        let status = withUnsafeMutablePointer(to: &value) {
            AudioObjectGetPropertyData(object, &address, 0, nil, &size, $0)
        }
        guard status == noErr, let value else {
            throw ACAudioRoomError.coreAudio("Read Core Audio string property", status)
        }
        return value
    }

    deinit { stop() }
}
