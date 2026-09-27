import Foundation

final class DeletionProtocol: URLProtocol {
    static var status = 200
    static var body = "{\"result\":\"Deleted!\"}"
    static var requests = 0
    override class func canInit(with request: URLRequest) -> Bool { true }
    override class func canonicalRequest(for request: URLRequest) -> URLRequest { request }
    override func startLoading() {
        Self.requests += 1
        precondition(request.httpMethod == "POST")
        precondition(request.url?.path == "/api/delete-erase-and-forget-me")
        precondition(request.value(forHTTPHeaderField: "Authorization") == "Bearer fixture-token")
        let response = HTTPURLResponse(url: request.url!, statusCode: Self.status, httpVersion: nil, headerFields: nil)!
        client?.urlProtocol(self, didReceive: response, cacheStoragePolicy: .notAllowed)
        client?.urlProtocol(self, didLoad: Data(Self.body.utf8))
        client?.urlProtocolDidFinishLoading(self)
    }
    override func stopLoading() {}
}

@main struct AccountDeletionChecks {
    static func main() async throws {
        let config = URLSessionConfiguration.ephemeral
        config.protocolClasses = [DeletionProtocol.self]
        let session = URLSession(configuration: config)
        do { try await AccountDeletion.delete(token: "", session: session); preconditionFailure("Empty credential accepted") } catch {}
        precondition(DeletionProtocol.requests == 0)
        try await AccountDeletion.delete(token: "fixture-token", session: session)
        for (status, body) in [(500, "{}"), (401, "{}"), (200, "{\"result\":\"pending\"}"), (200, "not-json")] {
            DeletionProtocol.status = status; DeletionProtocol.body = body
            let before = DeletionProtocol.requests
            do { try await AccountDeletion.delete(token: "fixture-token", session: session); preconditionFailure("Unconfirmed deletion accepted") } catch {}
            precondition(DeletionProtocol.requests == before + 1, "Deletion must not be automatically replayed")
        }
        print("Account deletion requires credentials and confirmed server success; errors never replay deletion.")
    }
}
