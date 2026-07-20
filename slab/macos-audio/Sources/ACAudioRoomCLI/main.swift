import ACMacAudio
import AVFoundation
import Foundation

private func value(after flag: String, in args: [String]) -> String? {
    guard let index = args.firstIndex(of: flag), index + 1 < args.count else { return nil }
    return args[index + 1]
}

private func usage() -> Never {
    print("""
    ac-audio-room — synchronized Mac-to-Mac audio owned by Aesthetic Computer

      ac-audio-room receive --host neo.local --channel left|right|mono|stereo [--name blueberry] [--gain 1]
      ac-audio-room send --app Spotify [--latency 700] [--keep-original]
      ac-audio-room tone [--left-hz 440] [--right-hz 660]

    All commands accept --port \(ACRoomWire.defaultPort).
    """)
    exit(2)
}

private func emit(_ line: String) {
    FileHandle.standardError.write(Data((line + "\n").utf8))
}

let args = Array(CommandLine.arguments.dropFirst())
guard let command = args.first else { usage() }
let port = UInt16(value(after: "--port", in: args) ?? "") ?? ACRoomWire.defaultPort

signal(SIGINT) { _ in exit(0) }
signal(SIGTERM) { _ in exit(0) }

do {
    switch command {
    case "receive":
        let host = value(after: "--host", in: args) ?? "neo.local"
        let roleName = value(after: "--channel", in: args) ?? "stereo"
        guard let channel = ACRoomWire.Channel(name: roleName) else { usage() }
        let name = value(after: "--name", in: args) ?? Host.current().localizedName ?? "Mac"
        let gain = Float(value(after: "--gain", in: args) ?? "") ?? 1
        let receiver = ACAudioRoomReceiver(configuration: .init(host: host, port: port,
                                                                 name: name, channel: channel, gain: gain))
        receiver.onLog = { emit("room receive · \($0)") }
        try receiver.start()
        dispatchMain()

    case "send":
        guard #available(macOS 14.2, *) else {
            throw ACAudioRoomError.unavailable("Per-app capture requires macOS 14.2 or newer")
        }
        let app = value(after: "--app", in: args) ?? "Spotify"
        let latency = UInt64(value(after: "--latency", in: args) ?? "") ?? 700
        let muteOriginal = !args.contains("--keep-original")
        let sender = ACAudioRoomSender(configuration: .init(port: port, latencyMilliseconds: latency))
        sender.onLog = { emit("room send · \($0)") }
        try sender.start()
        let tap = ACProcessAudioTap(applicationName: app, muteOriginal: muteOriginal)
        tap.onLog = { emit("room tap · \($0)") }
        try tap.start { sender.send($0) }
        emit("room send · \(app) is live; original output \(muteOriginal ? "muted" : "kept")")
        dispatchMain()

    case "tone":
        let leftHz = Double(value(after: "--left-hz", in: args) ?? "") ?? 440
        let rightHz = Double(value(after: "--right-hz", in: args) ?? "") ?? 660
        let sender = ACAudioRoomSender(configuration: .init(port: port, latencyMilliseconds: 500))
        sender.onLog = { emit("room tone · \($0)") }
        try sender.start()
        let format = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                   sampleRate: ACRoomWire.sampleRate,
                                   channels: 2, interleaved: false)!
        var phaseL = 0.0
        var phaseR = 0.0
        let frames = AVAudioFrameCount(960)
        let timer = DispatchSource.makeTimerSource(queue: .global(qos: .userInitiated))
        timer.schedule(deadline: .now(), repeating: .milliseconds(20))
        timer.setEventHandler {
            guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
                  let data = buffer.floatChannelData else { return }
            buffer.frameLength = frames
            for index in 0..<Int(frames) {
                data[0][index] = Float(sin(phaseL) * 0.12)
                data[1][index] = Float(sin(phaseR) * 0.12)
                phaseL += 2 * .pi * leftHz / ACRoomWire.sampleRate
                phaseR += 2 * .pi * rightHz / ACRoomWire.sampleRate
            }
            sender.send(buffer)
        }
        timer.resume()
        emit("room tone · left \(Int(leftHz)) Hz / right \(Int(rightHz)) Hz")
        dispatchMain()

    default: usage()
    }
} catch {
    fputs("ac-audio-room: \(error.localizedDescription)\n", stderr)
    exit(1)
}
