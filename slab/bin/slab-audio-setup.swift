#!/usr/bin/env swift
// slab-audio-setup.swift — build the Core Audio plumbing slab-call-record needs
// to capture BOTH sides of a FaceTime / Zoom / etc. call.
//
// Creates (idempotently — destroys + recreates by UID):
//   • Aggregate device "Meeting"        = <mic> + BlackHole 2ch   (the recorder reads this)
//   • Multi-Output device "Meeting Out" = <speakers> + BlackHole 2ch (so you still HEAR the call)
//
// During a call: set System Output → "Meeting Out", then hit Start call in the
// slab menubar. ffmpeg captures "Meeting": your mic on the low channel(s), the
// far end (looped back through BlackHole) on the high channel(s).
//
// Run: swift slab/bin/slab-audio-setup.swift   (no sudo needed)

import CoreAudio
import Foundation

let MEETING_UID = "computer.aesthetic.slab.meeting"
let MEETING_OUT_UID = "computer.aesthetic.slab.meeting-out"
let BLACKHOLE_MATCH = "BlackHole"

// MARK: - Core Audio property helpers

func addr(_ selector: AudioObjectPropertySelector,
          _ scope: AudioObjectPropertyScope = kAudioObjectPropertyScopeGlobal,
          _ element: AudioObjectPropertyElement = kAudioObjectPropertyElementMain)
-> AudioObjectPropertyAddress {
  AudioObjectPropertyAddress(mSelector: selector, mScope: scope, mElement: element)
}

func allDeviceIDs() -> [AudioObjectID] {
  var a = addr(kAudioHardwarePropertyDevices)
  var size: UInt32 = 0
  guard AudioObjectGetPropertyDataSize(AudioObjectID(kAudioObjectSystemObject), &a, 0, nil, &size) == noErr else { return [] }
  let count = Int(size) / MemoryLayout<AudioObjectID>.size
  var ids = [AudioObjectID](repeating: 0, count: count)
  guard AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &a, 0, nil, &size, &ids) == noErr else { return [] }
  return ids
}

func stringProp(_ id: AudioObjectID, _ selector: AudioObjectPropertySelector) -> String? {
  var a = addr(selector)
  var size = UInt32(MemoryLayout<CFString?>.size)
  var cf: CFString? = nil
  let st = withUnsafeMutablePointer(to: &cf) {
    AudioObjectGetPropertyData(id, &a, 0, nil, &size, $0)
  }
  guard st == noErr, let s = cf else { return nil }
  return s as String
}

func deviceName(_ id: AudioObjectID) -> String { stringProp(id, kAudioObjectPropertyName) ?? "?" }
func deviceUID(_ id: AudioObjectID) -> String? { stringProp(id, kAudioDevicePropertyDeviceUID) }

// Channel count for a scope (input or output).
func channelCount(_ id: AudioObjectID, scope: AudioObjectPropertyScope) -> Int {
  var a = addr(kAudioDevicePropertyStreamConfiguration, scope)
  var size: UInt32 = 0
  guard AudioObjectGetPropertyDataSize(id, &a, 0, nil, &size) == noErr, size > 0 else { return 0 }
  let bl = UnsafeMutableRawPointer.allocate(byteCount: Int(size), alignment: MemoryLayout<AudioBufferList>.alignment)
  defer { bl.deallocate() }
  guard AudioObjectGetPropertyData(id, &a, 0, nil, &size, bl) == noErr else { return 0 }
  let abl = UnsafeMutableAudioBufferListPointer(bl.assumingMemoryBound(to: AudioBufferList.self))
  return abl.reduce(0) { $0 + Int($1.mNumberChannels) }
}

func hasInput(_ id: AudioObjectID) -> Bool { channelCount(id, scope: kAudioObjectPropertyScopeInput) > 0 }
func hasOutput(_ id: AudioObjectID) -> Bool { channelCount(id, scope: kAudioObjectPropertyScopeOutput) > 0 }

func defaultDevice(_ selector: AudioObjectPropertySelector) -> AudioObjectID? {
  var a = addr(selector)
  var id = AudioObjectID(0)
  var size = UInt32(MemoryLayout<AudioObjectID>.size)
  guard AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &a, 0, nil, &size, &id) == noErr, id != 0 else { return nil }
  return id
}

// MARK: - Create / destroy aggregates

func destroyByUID(_ uid: String) {
  for id in allDeviceIDs() where deviceUID(id) == uid {
    let st = AudioHardwareDestroyAggregateDevice(id)
    if st == noErr { print("  · removed existing \(uid)") }
  }
}

func createAggregate(name: String, uid: String, subUIDs: [String], masterUID: String, stacked: Bool) -> Bool {
  let subList = subUIDs.map { [kAudioSubDeviceUIDKey: $0] }
  var desc: [String: Any] = [
    kAudioAggregateDeviceNameKey as String: name,
    kAudioAggregateDeviceUIDKey as String: uid,
    kAudioAggregateDeviceSubDeviceListKey as String: subList,
    kAudioAggregateDeviceMasterSubDeviceKey as String: masterUID,
    kAudioAggregateDeviceIsPrivateKey as String: 0,   // visible in Audio MIDI Setup
  ]
  if stacked { desc[kAudioAggregateDeviceIsStackedKey as String] = 1 }
  var newID = AudioObjectID(0)
  let st = AudioHardwareCreateAggregateDevice(desc as CFDictionary, &newID)
  if st != noErr {
    print("  ✗ failed to create \(name) (OSStatus \(st))")
    return false
  }
  print("  ✓ created \(name) [\(uid)] from \(subUIDs.joined(separator: " + "))")
  return true
}

// MARK: - Main

print("slab-audio-setup — wiring call-recording devices\n")

let ids = allDeviceIDs()

// Find BlackHole (must be installed first).
guard let blackhole = ids.first(where: { deviceName($0).localizedCaseInsensitiveContains(BLACKHOLE_MATCH) }),
      let blackholeUID = deviceUID(blackhole) else {
  print("✗ BlackHole not found. Install it first:")
  print("    brew install --cask blackhole-2ch")
  exit(1)
}
print("found BlackHole: \(deviceName(blackhole)) [\(blackholeUID)]")

// Pick the mic: prefer the system default input, but never BlackHole itself.
let micCandidate = defaultDevice(kAudioHardwarePropertyDefaultInputDevice)
let mic: AudioObjectID? = {
  if let m = micCandidate, m != blackhole, hasInput(m) { return m }
  return ids.first(where: { $0 != blackhole && hasInput($0) })
}()
guard let micID = mic, let micUID = deviceUID(micID) else {
  print("✗ no usable microphone found"); exit(1)
}
print("found mic:       \(deviceName(micID)) [\(micUID)]")

// Pick speakers: system default output, never BlackHole.
let spkCandidate = defaultDevice(kAudioHardwarePropertyDefaultOutputDevice)
let spk: AudioObjectID? = {
  if let s = spkCandidate, s != blackhole, hasOutput(s) { return s }
  return ids.first(where: { $0 != blackhole && hasOutput($0) })
}()
guard let spkID = spk, let spkUID = deviceUID(spkID) else {
  print("✗ no usable speakers found"); exit(1)
}
print("found speakers:  \(deviceName(spkID)) [\(spkUID)]\n")

// Aggregate "Meeting" — mic is master (clock source), BlackHole second.
// Channel order follows sub-device order: mic channels first, BlackHole after.
print("Aggregate \"Meeting\" (recorder input):")
destroyByUID(MEETING_UID)
let agg = createAggregate(name: "Meeting", uid: MEETING_UID,
                          subUIDs: [micUID, blackholeUID], masterUID: micUID, stacked: false)

// Multi-Output "Meeting Out" — speakers master so you still hear, BlackHole taps the audio.
print("\nMulti-Output \"Meeting Out\" (call playback + tap):")
destroyByUID(MEETING_OUT_UID)
let mo = createAggregate(name: "Meeting Out", uid: MEETING_OUT_UID,
                         subUIDs: [spkUID, blackholeUID], masterUID: spkUID, stacked: true)

print("")
if agg && mo {
  print("✓ done. To record a call:")
  print("    1. System Settings → Sound → Output → \"Meeting Out\"")
  print("    2. Start the FaceTime call, hit Start call in the slab menubar")
  print("    3. Stop call when done — far end is on the high channel(s)")
  exit(0)
} else {
  exit(1)
}
