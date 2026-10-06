import Foundation

@main struct AccountDeletionClientCheck {
    @MainActor static func main() async throws {
        var generation = 1, requests = 0, clears = 0, status = 200
        var signedIn = true, switchDuringGET = false, switchDuringPOST = false, cleanupFails = false
        let validPreview = "{\"handle\":\"fixture\",\"email\":\"fixture@example.test\",\"graceDays\":14,\"handleHoldDays\":90,\"braincells\":1000000,\"counts\":{\"whistlegraphs\":3}}"
        let validSchedule = "{\"state\":\"scheduled\",\"purgeAfter\":\"2026-10-20T00:00:00.000Z\",\"mailed\":true}"
        var postBody = validSchedule
        func client() -> AccountDeletionClient {
            AccountDeletionClient(credential: { signedIn ? .init(bearer: "mock-token", generation: generation) : nil },
                current: { $0.generation == generation }, send: { request in
                    requests += 1
                    precondition(request.value(forHTTPHeaderField: "Authorization") == "Bearer mock-token")
                    precondition(request.url?.host == "aesthetic.computer")
                    let get = request.httpMethod == "GET"
                    if get { precondition(request.url?.query?.contains("preview") == true) }
                    else { precondition(request.httpMethod == "POST" && request.httpBody == Data("{}".utf8)) }
                    if (get && switchDuringGET) || (!get && switchDuringPOST) { generation += 1 }
                    let response = HTTPURLResponse(url: request.url!, statusCode: status, httpVersion: nil, headerFields: nil)!
                    return (Data((get ? validPreview : postBody).utf8), response)
                }, clear: {
                    clears += 1
                    if cleanupFails { throw URLError(.cannotRemoveFile) }
                })
        }
        func rejected(_ run: () async throws -> Void) async {
            do { try await run(); preconditionFailure("Expected failure") } catch {}
        }
        let first = client()
        await rejected { _ = try await first.confirm() }
        precondition(requests == 0 && clears == 0, "Preview required before destructive request")
        signedIn = false
        await rejected { _ = try await first.load() }
        precondition(requests == 0)
        signedIn = true
        let preview = try await first.load()
        precondition(preview.counts["whistlegraphs"] == 3 && preview.braincells == 1_000_000 && clears == 0)
        generation += 1
        let before = requests
        await rejected { _ = try await first.confirm() }
        precondition(requests == before && clears == 0, "Switched account requires its own preview")
        switchDuringGET = true
        await rejected { _ = try await first.load() }
        precondition(first.preview == nil)
        switchDuringGET = false
        _ = try await first.load(); status = 503
        await rejected { _ = try await first.confirm() }
        precondition(clears == 0, "Server failure never erases local data")
        status = 200; postBody = "{\"state\":\"scheduled\",\"purgeAfter\":\"bad-date\"}"
        await rejected { _ = try await first.confirm() }
        precondition(clears == 0, "Malformed acknowledgement never erases local data")
        postBody = validSchedule; switchDuringPOST = true
        await rejected { _ = try await first.confirm() }
        precondition(clears == 0, "In-flight account change preserves current account's work")
        switchDuringPOST = false
        _ = try await first.load()
        let result = try await first.confirm()
        precondition(result.state == "scheduled" && result.localErasureError == nil && clears == 1)
        let after = requests
        await rejected { _ = try await first.confirm() }
        precondition(requests == after && clears == 1, "Consumed preview cannot schedule twice")
        let partial = client(); _ = try await partial.load(); cleanupFails = true
        let scheduled = try await partial.confirm()
        precondition(scheduled.state == "scheduled" && scheduled.localErasureError != nil, "Partial local cleanup must still report acknowledged server deletion")
        print("AccountDeletionClientCheck passed (11 cases; mock requests only)")
    }
}
