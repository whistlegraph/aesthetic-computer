import AppKit
import CoreBluetooth
import CoreLocation
import CoreWLAN

/// 🏷 Favicons that badge the constellation's stars: who makes, hosts or runs
/// each thing (apple.com, digitalocean.com, sonos.com …). Fetched once per
/// domain, straight from the domain (touch icon, then favicon.ico), with
/// DuckDuckGo's icon service behind them for sites that refuse a bare
/// client. Kept as 64-pixel PNGs in ~/.local/share/blueberry-wallpaper/favicons;
/// a domain that gave nothing is asked again a day later.
enum Favicons {
    /// Bumped whenever an icon arrives, so the constellation knows to rebuild.
    private(set) static var version = 0
    private static var memory: [String: CGImage] = [:]
    /// Icons that are mostly dark ink (DigitalOcean's ring, tangled's mark):
    /// they get a light plate so they read on the dark field.
    private(set) static var dim = Set<String>()
    private static var asking = Set<String>()
    private static let dir = NSHomeDirectory() + "/.local/share/blueberry-wallpaper/favicons"

    static func image(_ domain: String) -> CGImage? {
        guard !domain.isEmpty, domain.range(of: "^[a-z0-9.-]+$", options: .regularExpression) != nil else { return nil }
        if let hit = memory[domain] { return hit }
        let png = "\(dir)/\(domain).png", miss = "\(dir)/\(domain).miss"
        if let img = NSImage(contentsOfFile: png)?.cgImage(forProposedRect: nil, context: nil, hints: nil) {
            memory[domain] = img
            if isDim(img) { dim.insert(domain) }
            return img
        }
        let missAge = (try? FileManager.default.attributesOfItem(atPath: miss)[.modificationDate] as? Date).flatMap { $0 }
        if let missAge, Date().timeIntervalSince(missAge) < 86400 { return nil }
        guard !asking.contains(domain) else { return nil }
        asking.insert(domain)
        fetch(domain, [
            "https://\(domain)/apple-touch-icon.png",
            "https://\(domain)/favicon.ico",
            "https://icons.duckduckgo.com/ip3/\(domain).ico",
        ])
        return nil
    }

    private static func fetch(_ domain: String, _ urls: [String]) {
        guard let first = urls.first, let url = URL(string: first) else {
            try? FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
            FileManager.default.createFile(atPath: "\(dir)/\(domain).miss", contents: Data())
            DispatchQueue.main.async { asking.remove(domain) }
            return
        }
        var req = URLRequest(url: url, timeoutInterval: 10)
        req.setValue("Mozilla/5.0 (Macintosh) blueberry-wallpaper", forHTTPHeaderField: "User-Agent")
        URLSession.shared.dataTask(with: req) { data, response, _ in
            let ok = (response as? HTTPURLResponse)?.statusCode == 200
            guard ok, let data, let small = squared(data) else { return fetch(domain, Array(urls.dropFirst())) }
            try? FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
            if let png = NSBitmapImageRep(cgImage: small).representation(using: .png, properties: [:]) {
                try? png.write(to: URL(fileURLWithPath: "\(dir)/\(domain).png"))
            }
            DispatchQueue.main.async {
                memory[domain] = small
                if isDim(small) { dim.insert(domain) }
                asking.remove(domain)
                version += 1
            }
        }.resume()
    }

    /// Mean lightness of the visible pixels below a third: dark ink.
    private static func isDim(_ image: CGImage) -> Bool {
        let side = 16
        var px = [UInt8](repeating: 0, count: side * side * 4)
        guard let ctx = CGContext(data: &px, width: side, height: side, bitsPerComponent: 8, bytesPerRow: side * 4,
                                  space: CGColorSpace(name: CGColorSpace.sRGB)!,
                                  bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) else { return false }
        ctx.draw(image, in: CGRect(x: 0, y: 0, width: side, height: side))
        var light = 0.0, seen = 0.0
        for i in stride(from: 0, to: px.count, by: 4) where px[i + 3] > 64 {
            let a = Double(px[i + 3]) / 255
            light += (0.299 * Double(px[i]) + 0.587 * Double(px[i + 1]) + 0.114 * Double(px[i + 2])) / 255 / a
            seen += 1
        }
        return seen > 0 && light / seen < 0.33
    }

    /// The largest picture in the file, drawn into a 64-pixel square.
    private static func squared(_ data: Data) -> CGImage? {
        guard let image = NSImage(data: data), image.size.width >= 8 else { return nil }
        let best = image.representations.max { $0.pixelsWide < $1.pixelsWide }
        let side = 64
        guard let ctx = CGContext(data: nil, width: side, height: side, bitsPerComponent: 8, bytesPerRow: 0,
                                  space: CGColorSpace(name: CGColorSpace.sRGB)!,
                                  bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) else { return nil }
        ctx.interpolationQuality = .high
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
        let rect = NSRect(x: 0, y: 0, width: side, height: side)
        if let best { best.draw(in: rect) } else { image.draw(in: rect) }
        NSGraphicsContext.restoreGraphicsState()
        return ctx.makeImage()
    }
}

/// 📶 What this Mac can hear itself: Bluetooth devices nearby, paired or not,
/// from a ten-second CoreBluetooth scan every half minute, and the Wi-Fi
/// network's name through CoreWLAN (which macOS only tells an app with
/// Location access). Written to ~/.local/share/blueberry-wallpaper/radio.json
/// for the room MCP, and read directly by the constellation.
///
///     { observedAt, power, ssid, nearby: [{ id, name, vendor, kind, rssi, lastSeen }] }
final class Radio: NSObject, CBCentralManagerDelegate, CLLocationManagerDelegate {
    static let shared = Radio()

    struct Heard { var name: String; var vendor: String; var kind: String; var rssi: Int; var last: Date }
    private(set) var heard: [UUID: Heard] = [:]
    private(set) var power = true
    private(set) var ssid = ""
    /// Bumped when what the constellation would show changes.
    private(set) var version = 0
    private var shown = ""
    private var central: CBCentralManager?
    private let location = CLLocationManager()
    private var started = false

    /// Gone this long, a device is no longer in the room.
    static let window: TimeInterval = 120
    /// Weaker than this is the next room over.
    static let floor = -85

    func start() {
        guard !started else { return }
        started = true
        location.delegate = self
        location.requestWhenInUseAuthorization()
        central = CBCentralManager(delegate: self, queue: .main, options: [CBCentralManagerOptionShowPowerAlertKey: false])
        Timer.scheduledTimer(withTimeInterval: 30, repeats: true) { [weak self] _ in self?.cycle() }
        cycle()
    }

    private func cycle() {
        readSSID()
        if let central, central.state == .poweredOn {
            central.scanForPeripherals(withServices: nil, options: [CBCentralManagerScanOptionAllowDuplicatesKey: true])
            DispatchQueue.main.asyncAfter(deadline: .now() + 10) { [weak self] in
                self?.central?.stopScan()
                self?.settle()
            }
        } else {
            settle()
        }
    }

    private func readSSID() {
        ssid = CWWiFiClient.shared().interface()?.ssid() ?? ""
    }

    func locationManagerDidChangeAuthorization(_ manager: CLLocationManager) { readSSID(); settle() }

    func centralManagerDidUpdateState(_ central: CBCentralManager) {
        power = central.state != .poweredOff
        if central.state == .poweredOn { cycle() }
    }

    func centralManager(_ central: CBCentralManager, didDiscover peripheral: CBPeripheral,
                        advertisementData: [String: Any], rssi RSSI: NSNumber) {
        let rssi = RSSI.intValue
        guard rssi < 0, rssi > Self.floor else { return }
        let name = (advertisementData[CBAdvertisementDataLocalNameKey] as? String) ?? peripheral.name ?? ""
        let maker = advertisementData[CBAdvertisementDataManufacturerDataKey] as? Data ?? Data()
        let (vendor, kind) = Self.identify(maker, name: name)
        var h = heard[peripheral.identifier] ?? Heard(name: name, vendor: vendor, kind: kind, rssi: rssi, last: Date())
        if !name.isEmpty { h.name = name }
        if !vendor.isEmpty { h.vendor = vendor }
        if !kind.isEmpty { h.kind = kind }
        h.rssi = rssi
        h.last = Date()
        heard[peripheral.identifier] = h
    }

    /// Bluetooth SIG company ids, and Apple's Continuity message types, which
    /// say what an unnamed Apple device is without saying whose it is.
    private static let companies: [UInt16: String] = [
        0x004C: "apple.com", 0x0006: "microsoft.com", 0x0075: "samsung.com", 0x00E0: "google.com", 0x0087: "garmin.com",
        0x012D: "sony.com", 0x038F: "mi.com", 0x0157: "huami.com", 0x02E5: "espressif.com", 0x0059: "nordicsemi.com",
        0x0171: "amazon.com", 0x0499: "ruuvi.com", 0x05A7: "sonos.com", 0x0310: "sgl.com", 0x009E: "bose.com",
        0x0057: "harman.com", 0x00D2: "dialog-semiconductor.com", 0x0301: "tile.com", 0x067C: "tile.com", 0x0822: "8bitdo.com",
    ]
    private static func identify(_ data: Data, name: String) -> (String, String) {
        guard data.count >= 2 else { return ("", "") }
        let company = UInt16(data[data.startIndex]) | UInt16(data[data.startIndex + 1]) << 8
        let vendor = companies[company] ?? ""
        guard company == 0x004C, data.count >= 3 else { return (vendor, "") }
        switch data[data.startIndex + 2] {
        case 0x07: return (vendor, "headphones")   // proximity pairing: AirPods, Beats
        case 0x12: return (vendor, "tag")          // Find My, separated from its owner
        case 0x09, 0x0A: return (vendor, "tv")     // AirPlay target
        case 0x10, 0x0F, 0x0C: return (vendor, "phone") // Nearby Info / Action / Handoff: an iPhone, iPad or Mac
        default: return (vendor, "")
        }
    }

    /// In the room now, loudest first.
    var nearby: [(id: UUID, h: Heard)] {
        let now = Date()
        return heard.filter { now.timeIntervalSince($0.value.last) < Self.window }
            .map { ($0.key, $0.value) }.sorted { $0.h.rssi > $1.h.rssi }
    }

    private func settle() {
        let now = Date()
        heard = heard.filter { now.timeIntervalSince($0.value.last) < Self.window * 5 }
        let near = nearby
        // Rebuild only when the names or counts change, not with every RSSI wobble.
        let look = "\(power)\(ssid)" + near.map { "\($0.h.name)|\($0.h.kind)|\($0.h.vendor)" }.sorted().joined(separator: ",")
        if look != shown { shown = look; version += 1 }
        let json: [String: Any] = [
            "observedAt": now.timeIntervalSince1970, "power": power, "ssid": ssid,
            "nearby": near.map { ["id": $0.id.uuidString, "name": $0.h.name, "vendor": $0.h.vendor, "kind": $0.h.kind,
                                  "rssi": $0.h.rssi, "lastSeen": $0.h.last.timeIntervalSince1970] as [String: Any] },
        ]
        let path = NSHomeDirectory() + "/.local/share/blueberry-wallpaper/radio.json"
        if let data = try? JSONSerialization.data(withJSONObject: json, options: [.sortedKeys]) {
            try? data.write(to: URL(fileURLWithPath: path), options: .atomic)
        }
    }
}
