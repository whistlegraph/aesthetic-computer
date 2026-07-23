#!/usr/bin/env swift
// Forward a macOS CoreMIDI input to AC Native BIOS over one small UDP packet
// per Note On/Off, CC, or pitch-bend message.
//
// Usage: swift xbox/tools/midi-bridge.swift <xbox-host> [input-name]
// Example: swift xbox/tools/midi-bridge.swift xbox.local reface

import CoreMIDI
import Foundation
import Network

guard CommandLine.arguments.count >= 2 else {
  FileHandle.standardError.write(Data(
    "usage: midi-bridge.swift <xbox-host> [input-name; default reface]\n".utf8))
  exit(2)
}

let host = CommandLine.arguments[1]
let wantedName = (CommandLine.arguments.count > 2 ? CommandLine.arguments[2] : "reface").lowercased()
let queue = DispatchQueue(label: "computer.aesthetic.xbox-midi")
guard let port = NWEndpoint.Port(rawValue: 51337) else { fatalError("invalid UDP port") }
let connection = NWConnection(host: NWEndpoint.Host(host), port: port, using: .udp)
var sequence: UInt32 = 0

func midiName(_ endpoint: MIDIEndpointRef) -> String {
  var value: Unmanaged<CFString>?
  guard MIDIObjectGetStringProperty(endpoint, kMIDIPropertyDisplayName, &value) == noErr,
        let text = value?.takeRetainedValue() else { return "MIDI input \(endpoint)" }
  return text as String
}

func send(_ status: UInt8, _ data1: UInt8, _ data2: UInt8) {
  sequence &+= 1
  let sentUs = Int64(Date().timeIntervalSince1970 * 1_000_000)
  let payload = String(format: "ACM1 %u %lld %02X %u %u", sequence, sentUs,
    status, data1, data2)
  connection.send(content: Data(payload.utf8), completion: .contentProcessed { error in
    if let error { FileHandle.standardError.write(Data("UDP send: \(error)\n".utf8)) }
  })
}

func forward(_ bytes: [UInt8]) {
  var offset = 0
  while offset < bytes.count {
    let status = bytes[offset]
    if status < 0x80 { offset += 1; continue }
    let command = status & 0xf0
    let length: Int
    switch command {
    case 0x80, 0x90, 0xb0, 0xe0: length = 3
    case 0xc0, 0xd0: length = 2
    default: length = status >= 0xf8 ? 1 : 3
    }
    guard offset + length <= bytes.count else { return }
    if command == 0x80 || command == 0x90 || command == 0xb0 || command == 0xe0 {
      send(status, bytes[offset + 1] & 0x7f, bytes[offset + 2] & 0x7f)
      let channel = Int(status & 0x0f) + 1
      if command == 0x90 && bytes[offset + 2] > 0 {
        print("note on  ch\(channel) \(bytes[offset + 1]) vel \(bytes[offset + 2])")
      } else if command == 0x80 || (command == 0x90 && bytes[offset + 2] == 0) {
        print("note off ch\(channel) \(bytes[offset + 1])")
      } else if command == 0xe0 {
        print("bend     ch\(channel) \(Int(bytes[offset + 1]) | Int(bytes[offset + 2]) << 7)")
      } else {
        print("cc       ch\(channel) \(bytes[offset + 1]):\(bytes[offset + 2])")
      }
    }
    offset += length
  }
}

var client = MIDIClientRef()
guard MIDIClientCreateWithBlock("AC Xbox MIDI Bridge" as CFString, &client, { _ in }) == noErr else {
  fatalError("could not create CoreMIDI client")
}
var inputPort = MIDIPortRef()
guard MIDIInputPortCreateWithBlock(client, "AC Xbox Input" as CFString, &inputPort,
  { packetList, _ in
    var packet = packetList.pointee.packet
    for _ in 0..<packetList.pointee.numPackets {
      let count = Int(packet.length)
      let bytes = withUnsafeBytes(of: packet.data) { Array($0.prefix(count)) }
      forward(bytes)
      packet = MIDIPacketNext(&packet).pointee
    }
  }) == noErr else { fatalError("could not create CoreMIDI input port") }

let sources = (0..<MIDIGetNumberOfSources()).compactMap { index -> (MIDIEndpointRef, String)? in
  let source = MIDIGetSource(index)
  return source == 0 ? nil : (source, midiName(source))
}
guard let selected = sources.first(where: { $0.1.lowercased().contains(wantedName) }) else {
  let names = sources.map(\.1).joined(separator: ", ")
  FileHandle.standardError.write(Data(
    "no MIDI input matching '\(wantedName)'; available: \(names.isEmpty ? "none" : names)\n".utf8))
  exit(1)
}
guard MIDIPortConnectSource(inputPort, selected.0, nil) == noErr else {
  fatalError("could not connect \(selected.1)")
}

connection.stateUpdateHandler = { state in
  switch state {
  case .ready:
    print("AC MIDI bridge ready: \(selected.1) -> \(host):51337")
  case .failed(let error):
    FileHandle.standardError.write(Data("UDP failed: \(error)\n".utf8))
    exit(1)
  default: break
  }
}
connection.start(queue: queue)
dispatchMain()
