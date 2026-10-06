#if DEBUG
import Foundation

// Isolated screen fixtures; story export and AI permission checks also run on a phone.
// Never connect a fixture to a cloud thread.
enum NativeScreenFixture {
    static var mode: String { ProcessInfo.processInfo.environment["WALKIE_NATIVE_SCREEN_FIXTURE"] ?? "" }
    static var enabled: Bool {
        #if targetEnvironment(simulator)
        return ["history", "recording", "gestures", "working", "story", "audio", "consent"].contains(mode)
        #else
        return mode == "story" || mode == "consent"
        #endif
    }
    static var script: String {
        let source = (mode == "audio" ? "let ticks=0;export function sim({sound}){if(ticks++%30===0)sound.synth({type:'sine',tone:440,duration:0.3,volume:0.15});}" : "") + "export function paint({wipe,ink,screen}){wipe(25,23,46);ink(220,240,160).circle(screen.width/2,screen.height/2,35,true);ink(255,140,200).circle(screen.width/2+12,screen.height/2-12,8,true);}"
        let formatter = ISO8601DateFormatter(); formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        let sound: [String: Any] = ["durationMs": 3200, "frames": (0..<40).map { i -> [String: Any] in ["atMs": i * 80, "rms": 0.05, "pitchHz": 700 + i % 10 * 35] }]
        let input = ["transcript": "OK go like…", "sound": sound] as [String: Any]
        let request = "Sound request\nINPUT DATA:\n" + String(data: try! JSONSerialization.data(withJSONObject: input), encoding: .utf8)!
        let versions: [[String: Any]] = [
            ["id": 0, "parent": NSNull(), "source": "", "request": NSNull(), "createdAt": formatter.string(from: Date(timeIntervalSince1970: 1_780_000_000).addingTimeInterval(-420))],
            ["id": 1, "parent": 0, "source": source, "request": request, "createdAt": formatter.string(from: Date(timeIntervalSince1970: 1_780_000_000).addingTimeInterval(-180))],
            ["id": 2, "parent": 1, "source": source, "request": "Make it pink", "createdAt": formatter.string(from: Date(timeIntervalSince1970: 1_780_000_000).addingTimeInterval(-120))],
            ["id": 3, "parent": 1, "source": source, "request": "Add a little moon", "createdAt": formatter.string(from: Date(timeIntervalSince1970: 1_780_000_000).addingTimeInterval(-30))]
        ]
        let ledger: [String: Any] = ["format": 1, "head": 3, "versions": versions]
        let json = String(data: try! JSONSerialization.data(withJSONObject: ledger), encoding: .utf8)!
        let busy = mode == "working" ? "window.__walkiewareFixtureBusy='Someone is eating them';" : ""
        let cost = "localStorage.removeItem('whistlegraph-fixture-source-receipt-cost');localStorage.setItem('whistlegraph-fixture-source-receipts',JSON.stringify([{receipt:{format:1,id:'fixture-cost',repairs:0,status:'success',rounds:[{usage:{costUSD:0.42}}]}}]));"
        return busy + cost + "window.__whistlegraphFixture=true;window.__walkiewareDisableThread=true;localStorage.setItem('whistlegraph-fixture-source-versions',JSON.stringify(\(json)));localStorage.setItem('whistlegraph-fixture-source',\(json).versions[3].source);"
    }
}
#endif
