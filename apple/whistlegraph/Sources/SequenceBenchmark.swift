import Foundation
import WebKit
import CryptoKit

/// Opt-in debug harness. No ordinary sessions, credentials, or speech are saved.
@MainActor enum SequenceBenchmark {
    static var enabled: Bool {
        #if DEBUG
        return ProcessInfo.processInfo.environment["WHISTLEGRAPH_SEQUENCE_TEST"] == "1"
        #else
        return false
        #endif
    }
    static let runID = UUID(uuidString: ProcessInfo.processInfo.environment["WHISTLEGRAPH_RUN_ID"] ?? "") ?? UUID()
    static func capture(_ input: [String: Any], rect: [String: Double], view: WKWebView) async {
        guard enabled, let index = input["index"] as? Int, (1...32).contains(index),
              let documents = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first else { return }
        var report = input
        var checks = input["checks"] as? [String: Bool] ?? [:]
        let directory = documents.appendingPathComponent("sequence-" + runID.uuidString)
        let prefix = String(format: "move-%02d", index)
        var hashes: [String] = []
        let evidenceOnly = input["evidenceOnly"] as? Bool == true
        let frameCount = evidenceOnly ? 90 : (input["motionReview"] as? Bool == true ? 24 : 3)
        let started = ProcessInfo.processInfo.systemUptime
        var frameTimes: [Double] = []
        do {
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
            let crop = CGRect(x: rect["x"] ?? 0, y: rect["y"] ?? 0, width: rect["width"] ?? 0, height: rect["height"] ?? 0).intersection(view.bounds)
            guard !crop.isEmpty, !crop.isNull else { throw NSError(domain: "Sequence", code: 1) }
            for frame in 0..<frameCount {
                let config = WKSnapshotConfiguration(); config.rect = crop
                let data: Data = try await withCheckedThrowingContinuation { continuation in
                    view.takeSnapshot(with: config) { image, error in
                        if let data = image?.pngData() { continuation.resume(returning: data) }
                        else { continuation.resume(throwing: error ?? NSError(domain: "Snapshot", code: 1)) }
                    }
                }
                frameTimes.append((ProcessInfo.processInfo.systemUptime - started) * 1000)
                hashes.append(SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined())
                try data.write(to: directory.appendingPathComponent("\(prefix)-frame-\(frame).png"), options: .atomic)
                if frame < frameCount - 1 { try await Task.sleep(nanoseconds: (evidenceOnly || frameCount > 3) ? 75_000_000 : 350_000_000) }
            }
            checks[evidenceOnly ? "sequenceCaptured" : (frameCount > 3 ? "motionFramesCaptured" : "threeFramesCaptured")] = hashes.count == frameCount
            if input["requiresMotion"] as? Bool != false { checks["animationChanged"] = Set(hashes).count > 1 }
        } catch {
            checks["threeFramesCaptured"] = false
            report["captureError"] = error.localizedDescription
        }
        report["checks"] = checks
        report["frameHashes"] = hashes
        report["frameTimesMs"] = frameTimes
        report["runID"] = runID.uuidString
        report["passed"] = !checks.isEmpty && checks.values.allSatisfy { $0 }
        if let data = try? JSONSerialization.data(withJSONObject: report, options: [.prettyPrinted, .sortedKeys]) {
            try? data.write(to: directory.appendingPathComponent(prefix + ".json"), options: .atomic)
            try? data.write(to: documents.appendingPathComponent("whistlegraph-sequence-latest.json"), options: .atomic)
        }
        if evidenceOnly {
            print("[whistlegraph-sequence] captured \(hashes.count) frames for review")
            return
        }
        // Host behavioral tests must approve this exact run and move before continuing.
        var verdict: [String: Any]?
        for _ in 0..<600 {
            if let data = try? Data(contentsOf: documents.appendingPathComponent("whistlegraph-sequence-verdict.json")),
               let value = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
               value["runID"] as? String == runID.uuidString, value["index"] as? Int == index {
                verdict = value; break
            }
            try? await Task.sleep(nanoseconds: 1_000_000_000)
        }
        checks["featureTests"] = verdict?["passed"] as? Bool == true
        report["featureTests"] = verdict ?? ["error":"Host feature checks timed out"]
        report["checks"] = checks
        report["verified"] = true
        report["passed"] = checks.values.allSatisfy { $0 }
        if let data = try? JSONSerialization.data(withJSONObject: report, options: [.prettyPrinted, .sortedKeys]) {
            try? data.write(to: directory.appendingPathComponent(prefix + ".json"), options: .atomic)
            try? data.write(to: documents.appendingPathComponent("whistlegraph-sequence-latest.json"), options: .atomic)
        }
        print("[whistlegraph-sequence] move \(index)/32 \(report["passed"] as? Bool == true ? "passed" : "failed")")
        let passed = report["passed"] as? Bool == true
        try? await view.evaluateJavaScript("window.__whistlegraphSequenceCaptureDone?.({passed:\(passed ? "true" : "false")});")
    }
}
