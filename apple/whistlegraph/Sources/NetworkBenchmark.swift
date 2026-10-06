import Foundation

/// Explicit debug probe. No account token or generated content is sent.
final class NetworkProbeMetrics: NSObject, URLSessionTaskDelegate, @unchecked Sendable {
    private let lock = NSLock()
    private var values: [[String: Any]] = []
    func urlSession(_ session: URLSession, task: URLSessionTask, didFinishCollecting metrics: URLSessionTaskMetrics) {
        let rows = metrics.transactionMetrics.map { metric -> [String: Any] in
            func ms(_ from: Date?, _ to: Date?) -> Any {
                guard let from, let to else { return NSNull() }
                return Int(to.timeIntervalSince(from) * 1000)
            }
            return ["dnsMs": ms(metric.domainLookupStartDate, metric.domainLookupEndDate),
                    "connectMs": ms(metric.connectStartDate, metric.connectEndDate),
                    "tlsMs": ms(metric.secureConnectionStartDate, metric.secureConnectionEndDate),
                    "requestToHeadersMs": ms(metric.requestStartDate, metric.responseStartDate),
                    "reusedConnection": metric.isReusedConnection,
                    "protocol": metric.networkProtocolName ?? "unknown"]
        }
        lock.lock(); values = rows; lock.unlock()
    }
    func read() -> [[String: Any]] { lock.lock(); defer { lock.unlock() }; return values }
}

@MainActor enum NetworkBenchmark {
    static func run() async {
        #if DEBUG
        guard ProcessInfo.processInfo.environment["WHISTLEGRAPH_NETWORK_TEST"] == "1" else { return }
        let config = URLSessionConfiguration.ephemeral
        config.timeoutIntervalForRequest = 15
        let session = URLSession(configuration: config)
        defer { session.invalidateAndCancel() }
        var rows: [[String: Any]] = []
        for url in ["https://aesthetic.computer/aesel.json", "https://openrouter.ai/api/v1/models"] {
            for attempt in 1...2 {
                let delegate = NetworkProbeMetrics()
                var request = URLRequest(url: URL(string: url)!)
                request.httpMethod = "HEAD"; request.cachePolicy = .reloadIgnoringLocalCacheData
                let began = ProcessInfo.processInfo.systemUptime
                do {
                    let (_, response) = try await session.data(for: request, delegate: delegate)
                    rows.append(["url": url, "attempt": attempt, "status": (response as? HTTPURLResponse)?.statusCode ?? 0,
                        "totalMs": Int((ProcessInfo.processInfo.systemUptime - began) * 1000), "transactions": delegate.read()])
                } catch { rows.append(["url":url, "attempt":attempt, "error":error.localizedDescription]) }
            }
        }
        if let directory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first,
           let data = try? JSONSerialization.data(withJSONObject: ["device": "physical iPhone", "scope": "HEAD transport probes; excludes authenticated inference and provider compute", "probes":rows], options: [.prettyPrinted,.sortedKeys]) {
            try? data.write(to: directory.appendingPathComponent("whistlegraph-network.json"), options: .atomic)
        }
        print("[whistlegraph] network probes complete")
        #endif
    }
}
