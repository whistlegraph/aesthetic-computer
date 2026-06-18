import Foundation
import MultipeerConnectivity

/// Peer-to-peer link between Menu Bands — no router, no shared Wi-Fi, no ssh.
///
/// Multipeer Connectivity auto-uses Bluetooth + peer-to-peer Wi-Fi (AWDL, the
/// AirDrop transport). Each Menu Band advertises and browses the same service;
/// they connect directly. A 1 Hz ping-pong keeps a wall-clock offset to each
/// peer (NTP-style), so one machine can tell another "play this at <your
/// clock> T" and both land on the same real instant.
///
/// Messages are JSON dicts of strings (mirrors the distributed-notification
/// `userInfo` shape, so the same play parser handles both transports). Sync
/// messages (`ping`/`pong`) are handled internally; everything else is handed
/// to `onMessage`.
final class MenuBandFleet: NSObject {
    static let serviceType = "acmenuband"             // ≤15 chars, must match Info.plist Bonjour

    private let myPeer: MCPeerID
    private let session: MCSession
    private let advertiser: MCNearbyServiceAdvertiser
    private let browser: MCNearbyServiceBrowser

    /// peerClock − myClock (seconds), smoothed, per peer.
    private var offsets: [MCPeerID: Double] = [:]
    private var pingTimer: Timer?

    /// Delivered on the main thread for any non-sync message (e.g. a play).
    var onMessage: (([String: Any]) -> Void)?
    /// Connection changes, for logging/UI.
    var onPeersChanged: (([String]) -> Void)?

    override init() {
        let name = (Host.current().localizedName ?? "menuband")
        myPeer = MCPeerID(displayName: String(name.prefix(60)))
        session = MCSession(peer: myPeer, securityIdentity: nil, encryptionPreference: .none)
        advertiser = MCNearbyServiceAdvertiser(peer: myPeer, discoveryInfo: nil,
                                               serviceType: Self.serviceType)
        browser = MCNearbyServiceBrowser(peer: myPeer, serviceType: Self.serviceType)
        super.init()
        session.delegate = self
        advertiser.delegate = self
        browser.delegate = self
    }

    func start() {
        advertiser.startAdvertisingPeer()
        browser.startBrowsingForPeers()
    }

    var connectedPeers: [MCPeerID] { session.connectedPeers }

    /// Offset to the first connected peer (this fleet is two-machine for now).
    var firstPeerOffset: Double? {
        session.connectedPeers.first.flatMap { offsets[$0] }
    }

    func send(_ dict: [String: Any], to peers: [MCPeerID]? = nil) {
        let targets = peers ?? session.connectedPeers
        guard !targets.isEmpty,
              let data = try? JSONSerialization.data(withJSONObject: dict) else { return }
        try? session.send(data, toPeers: targets, with: .reliable)
    }

    // MARK: - Clock sync
    private func startPinging() {
        guard pingTimer == nil else { return }
        let t = Timer(timeInterval: 1.0, repeats: true) { [weak self] _ in self?.pingAll() }
        RunLoop.main.add(t, forMode: .common)
        pingTimer = t
        pingAll()
    }

    private func pingAll() {
        let peers = session.connectedPeers
        if peers.isEmpty { return }
        send(["t": "ping", "t0": Date().timeIntervalSince1970], to: peers)
    }
}

extension MenuBandFleet: MCSessionDelegate {
    func session(_ session: MCSession, peer peerID: MCPeerID, didChange state: MCSessionState) {
        DispatchQueue.main.async {
            if state == .connected { self.startPinging() }
            else if state == .notConnected { self.offsets[peerID] = nil }
            self.onPeersChanged?(self.session.connectedPeers.map { $0.displayName })
        }
    }

    func session(_ session: MCSession, didReceive data: Data, fromPeer peerID: MCPeerID) {
        guard let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let t = obj["t"] as? String else { return }
        switch t {
        case "ping":
            // Reply immediately, echoing t0 and stamping our receive/send time.
            let now = Date().timeIntervalSince1970
            send(["t": "pong", "t0": obj["t0"] ?? 0, "t1": now, "t2": Date().timeIntervalSince1970],
                 to: [peerID])
        case "pong":
            let t3 = Date().timeIntervalSince1970
            guard let t0 = obj["t0"] as? Double, let t1 = obj["t1"] as? Double,
                  let t2 = obj["t2"] as? Double else { return }
            let offset = ((t1 - t0) + (t2 - t3)) / 2.0     // peerClock − myClock
            DispatchQueue.main.async {
                if let prev = self.offsets[peerID] {
                    self.offsets[peerID] = prev * 0.7 + offset * 0.3   // smooth
                } else {
                    self.offsets[peerID] = offset
                }
            }
        default:
            DispatchQueue.main.async { self.onMessage?(obj) }
        }
    }

    func session(_ s: MCSession, didReceive stream: InputStream, withName n: String, fromPeer p: MCPeerID) {}
    func session(_ s: MCSession, didStartReceivingResourceWithName n: String, fromPeer p: MCPeerID, with progress: Progress) {}
    func session(_ s: MCSession, didFinishReceivingResourceWithName n: String, fromPeer p: MCPeerID, at u: URL?, withError e: Error?) {}
}

extension MenuBandFleet: MCNearbyServiceAdvertiserDelegate {
    func advertiser(_ advertiser: MCNearbyServiceAdvertiser,
                    didReceiveInvitationFromPeer peerID: MCPeerID,
                    withContext context: Data?,
                    invitationHandler: @escaping (Bool, MCSession?) -> Void) {
        invitationHandler(true, session)            // auto-accept
    }
}

extension MenuBandFleet: MCNearbyServiceBrowserDelegate {
    func browser(_ browser: MCNearbyServiceBrowser, foundPeer peerID: MCPeerID,
                 withDiscoveryInfo info: [String: String]?) {
        // Tie-break so only one side invites (lower name invites the higher).
        if myPeer.displayName < peerID.displayName {
            browser.invitePeer(peerID, to: session, withContext: nil, timeout: 30)
        }
    }
    func browser(_ browser: MCNearbyServiceBrowser, lostPeer peerID: MCPeerID) {}
}
