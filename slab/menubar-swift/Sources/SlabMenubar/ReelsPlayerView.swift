// ReelsPlayerView — the Reels player: one management window where the
// oskiewar reels can be watched and their Instagram numbers read side by
// side. Left: every reel the ledger or the local queue knows, newest
// first, with a per-segment rollup (publish.mjs segmentReport) up top.
// Right: the selected reel playing (AVKit, straight off the staged mp4)
// over its caption, render meta, and the per-post insight grid.
//
// Data comes from ReelStats.swift — local JSON only, live-reloading when
// the ledger is rewritten or the queue gains a render. A reel whose
// insights are still null reads "—" (not measured), never zero; a reel
// whose render was cleaned from tmp still lists, with its published CDN
// copy and permalink as openable links (opened in the browser — this app
// never fetches them itself).
import AppKit
import SwiftUI
import AVKit

/// One shared window, SlabAboutWindow's lifecycle: show() focuses the
/// existing panel or builds a fresh one; closing tears everything down
/// (including the store's file watchers and any playing video).
final class ReelsPlayerWindow: NSObject, NSWindowDelegate {
    private static var shared: ReelsPlayerWindow?

    private let window: NSWindow
    private let store = ReelsStore()

    static func show() {
        let controller = shared ?? ReelsPlayerWindow()
        shared = controller
        NSApp.activate(ignoringOtherApps: true)
        controller.store.reload()
        controller.window.makeKeyAndOrderFront(nil)
    }

    private override init() {
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 920, height: 620),
            styleMask: [.titled, .closable, .resizable],
            backing: .buffered,
            defer: false
        )
        super.init()
        window.title = "Reels"
        window.minSize = NSSize(width: 700, height: 440)
        window.isReleasedWhenClosed = false
        window.delegate = self
        window.contentView = NSHostingView(rootView: ReelsRootView(store: store))
        window.center()
    }

    func windowWillClose(_ notification: Notification) {
        // Drop the SwiftUI tree now so onDisappear pauses playback and the
        // store's watchers cancel with it.
        window.contentView = nil
        ReelsPlayerWindow.shared = nil
    }
}

extension AppDelegate {
    @objc func openReelsPlayer() { ReelsPlayerWindow.show() }
}

// MARK: - views

struct ReelsRootView: View {
    @ObservedObject var store: ReelsStore
    @State private var selectedID: String?

    private var selected: Reel? {
        store.reels.first { $0.id == selectedID } ?? store.reels.first
    }

    var body: some View {
        if store.reels.isEmpty {
            emptyState
        } else {
            HSplitView {
                reelList
                    .frame(minWidth: 330, maxWidth: 430)
                if let reel = selected {
                    ReelDetailView(reel: reel)
                        .id(reel.id)   // fresh detail (and player) per reel
                        .frame(minWidth: 320, maxWidth: .infinity, maxHeight: .infinity)
                }
            }
        }
    }

    private var reelList: some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack {
                let published = store.reels.filter { $0.post != nil }.count
                Text("\(store.reels.count) reels · \(published) published")
                    .font(.system(size: 12, weight: .semibold))
                Spacer()
                Button(action: { store.reload() }) {
                    Image(systemName: "arrow.clockwise")
                }
                .buttonStyle(PlainButtonStyle())
                .help("Re-read the ledger and queue now (they also reload on change).")
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 8)

            if !store.segments.isEmpty {
                VStack(alignment: .leading, spacing: 1) {
                    ForEach(store.segments) { row in
                        Text(segmentLine(row))
                            .font(.system(size: 10, design: .monospaced))
                            .foregroundColor(.secondary)
                    }
                }
                .padding(.horizontal, 12)
                .padding(.bottom, 6)
                .help("Per-segment rollup over every live post with measured insights — the same arithmetic as publish.mjs segmentReport().")
            }
            Divider()

            ScrollView {
                LazyVStack(spacing: 0) {
                    ForEach(store.reels) { reel in
                        ReelRowView(reel: reel, isSelected: reel.id == selected?.id) {
                            selectedID = reel.id
                        }
                        Divider().padding(.leading, 12)
                    }
                }
            }
        }
    }

    /// `fgc       3 posts   1528 views    509/post   0.46%`
    private func segmentLine(_ row: ReelSegmentRow) -> String {
        let name = row.segment.padding(toLength: 10, withPad: " ", startingAt: 0)
        let posts = "\(row.posts) post\(row.posts == 1 ? " " : "s")"
        return name
            + posts.padding(toLength: 9, withPad: " ", startingAt: 0)
            + "\(Int(row.views)) views".padding(toLength: 13, withPad: " ", startingAt: 0)
            + "\(row.viewsPerPost)/post".padding(toLength: 11, withPad: " ", startingAt: 0)
            + String(format: "%.2f%%", row.interactionRate)
    }

    private var emptyState: some View {
        VStack(spacing: 8) {
            Image(systemName: "film.stack")
                .font(.system(size: 34))
                .foregroundColor(.secondary)
            Text("No reels found").font(.headline)
            Text("Looked for publish ledgers at\n"
                 + ReelStats.ledgerSources.map(\.path).joined(separator: "\n")
                 + "\nand staged renders under\n\(ReelStats.queueRoot)")
                .font(.caption)
                .foregroundColor(.secondary)
                .multilineTextAlignment(.center)
            Text("Point elsewhere in ~/.config/slab/reels.json — {\"ledger\": …, \"queue\": …}")
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .padding(24)
    }
}

/// One list row: thumbnail, id, segment + day + duration, and a one-line
/// status — measured numbers, "not measured", or "unpublished".
private struct ReelRowView: View {
    let reel: Reel
    let isSelected: Bool
    let select: () -> Void

    var body: some View {
        Button(action: select) {
            HStack(spacing: 10) {
                ReelThumb(path: reel.thumbnail)
                VStack(alignment: .leading, spacing: 3) {
                    Text(reel.displayTitle)
                        .font(.system(size: 12, weight: .semibold, design: .monospaced))
                        .lineLimit(1)
                    HStack(spacing: 6) {
                        AccountChip(account: reel.account)
                        // Segment is a factory-only concept; the per-account
                        // ledgers have none, and an empty chip reads as "?".
                        if !reel.segment.isEmpty { SegmentChip(segment: reel.segment) }
                        Text(metaLine).font(.system(size: 10)).foregroundColor(.secondary)
                    }
                    Text(statusLine)
                        .font(.system(size: 10, design: .monospaced))
                        .foregroundColor(.secondary)
                        .lineLimit(1)
                }
                Spacer(minLength: 0)
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 6)
            .contentShape(Rectangle())
        }
        .buttonStyle(PlainButtonStyle())
        .background(isSelected ? Color.accentColor.opacity(0.16) : Color.clear)
    }

    private var metaLine: String {
        var bits: [String] = []
        if !reel.day.isEmpty { bits.append(reel.day) }
        if let seconds = reel.meta?.seconds { bits.append(String(format: "%.1fs", seconds)) }
        if reel.localVideo == nil { bits.append("no local render") }
        return bits.joined(separator: " · ")
    }

    private var statusLine: String {
        guard let post = reel.post else { return "staged · unpublished" }
        guard let insights = post.insights else { return "published · not measured yet" }
        return "\(whole(insights.views)) views · reach \(whole(insights.reach))"
            + " · skip \(pct(insights.skipRate))"
    }
}

/// The right pane: player (or a "render not on disk" placeholder) above
/// caption, render meta, insight grid, and the openable links.
private struct ReelDetailView: View {
    let reel: Reel

    var body: some View {
        VStack(spacing: 0) {
            if let path = reel.localVideo {
                ReelVideo(path: path)
                    .aspectRatio(aspect, contentMode: .fit)
                    .frame(maxWidth: .infinity, maxHeight: 400)
                    .background(Color.black)
            } else {
                missingRender
            }
            Divider()
            ScrollView {
                VStack(alignment: .leading, spacing: 10) {
                    header
                    if let caption = reel.caption, !caption.isEmpty {
                        Text(caption).font(.system(size: 11)).foregroundColor(.secondary)
                    }
                    Text(renderLine).font(.system(size: 10, design: .monospaced))
                        .foregroundColor(.secondary)
                    insightGrid
                    footer
                    links
                }
                .padding(12)
                .frame(maxWidth: .infinity, alignment: .leading)
            }
        }
    }

    private var aspect: CGFloat {
        guard let w = reel.meta?.width, let h = reel.meta?.height, h > 0 else { return 9 / 16 }
        return CGFloat(w / h)
    }

    private var missingRender: some View {
        VStack(spacing: 6) {
            Image(systemName: "film").font(.system(size: 28)).foregroundColor(.secondary)
            Text("local render not on disk").font(.system(size: 11)).foregroundColor(.secondary)
            if reel.publishedURL != nil {
                Text("the published copy opens in the browser below")
                    .font(.system(size: 10)).foregroundColor(.secondary)
            }
        }
        .frame(maxWidth: .infinity, minHeight: 160)
    }

    private var header: some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(reel.displayTitle).font(.system(size: 14, weight: .bold, design: .monospaced))
            if reel.displayTitle != reel.id {
                // The media id is what matches this row to Meta's dashboard,
                // so it stays visible even when the row is titled by filename.
                Text(reel.id).font(.system(size: 9, design: .monospaced))
                    .foregroundColor(.secondary)
                    .help("Instagram media id — matches this reel in Meta's dashboard.")
            }
            HStack(spacing: 6) {
                AccountChip(account: reel.account)
                if !reel.segment.isEmpty { SegmentChip(segment: reel.segment) }
                if let name = reel.segmentName {
                    Text(name).font(.system(size: 10)).foregroundColor(.secondary)
                }
                if let audio = reel.audioName {
                    Text("♪ \(audio)").font(.system(size: 10)).foregroundColor(.secondary)
                }
                if reel.postCount > 1 {
                    Text("posted ×\(reel.postCount)")
                        .font(.system(size: 10)).foregroundColor(.secondary)
                        .help("This id went out more than once; the numbers below are the latest post's.")
                }
            }
        }
    }

    /// `14.4s · 1080×1920 · 60fps · 9.4 MB` — em-dash cells when the queue
    /// meta is gone with the render.
    private var renderLine: String {
        let meta = reel.meta
        let dims: String
        if let w = meta?.width, let h = meta?.height {
            dims = "\(Int(w))×\(Int(h))"
        } else { dims = "—" }
        return [
            meta?.seconds.map { String(format: "%.1fs", $0) } ?? "—",
            dims,
            meta?.fps.map { "\(Int($0))fps" } ?? "—",
            meta?.megabytes.map { String(format: "%.1f MB", $0) } ?? "—",
        ].joined(separator: " · ")
    }

    private var insightGrid: some View {
        let insights = reel.insights
        let cells: [(String, String)] = [
            ("views", whole(insights?.views)),
            ("reach", whole(insights?.reach)),
            ("likes", whole(insights?.likes)),
            ("comments", whole(insights?.comments)),
            ("saves", whole(insights?.saved)),
            ("shares", whole(insights?.shares)),
            ("interactions", whole(insights?.totalInteractions)),
            ("skip rate", pct(insights?.skipRate)),
            ("avg watch", seconds(fromMs: insights?.avgWatchTimeMs)),
        ]
        return LazyVGrid(columns: [GridItem(.adaptive(minimum: 74), spacing: 8)], spacing: 10) {
            ForEach(cells, id: \.0) { cell in
                VStack(spacing: 2) {
                    Text(cell.1)
                        .font(.system(size: 15, weight: .semibold, design: .monospaced))
                    Text(cell.0).font(.system(size: 9)).foregroundColor(.secondary)
                }
                .frame(maxWidth: .infinity)
            }
        }
        .padding(.vertical, 4)
    }

    private var footer: some View {
        Group {
            if reel.post == nil {
                Text("staged — not published yet")
            } else if reel.insights == nil {
                Text("published — insights not measured yet (nothing pulled, not zero views)")
            } else if let at = shortDate(reel.post?.insightsAt) {
                Text("insights pulled \(at)")
            } else {
                Text("insights pulled")
            }
        }
        .font(.system(size: 10))
        .foregroundColor(.secondary)
    }

    private var links: some View {
        HStack(spacing: 12) {
            if let permalink = reel.permalink {
                LinkButton(title: "Open on Instagram", url: permalink)
            }
            if let published = reel.publishedURL {
                LinkButton(title: "Published mp4", url: published)
            }
            if let path = reel.localVideo {
                Button("Show in Finder") {
                    NSWorkspace.shared.activateFileViewerSelecting(
                        [URL(fileURLWithPath: path)])
                }
            }
        }
        .font(.system(size: 11))
    }
}

/// Local playback only — the staged mp4 straight off disk. Autoplays on
/// select, pauses when the reel changes or the window closes.
private struct ReelVideo: View {
    @State private var player: AVPlayer

    init(path: String) {
        _player = State(initialValue: AVPlayer(url: URL(fileURLWithPath: path)))
    }

    var body: some View {
        VideoPlayer(player: player)
            .onAppear { player.play() }
            .onDisappear { player.pause() }
    }
}

/// Which account a reel went out on — the first thing to know now that one
/// window holds several. Outlined rather than filled so it reads as a
/// different KIND of fact than the segment chip beside it.
private struct AccountChip: View {
    let account: String

    var body: some View {
        Text(account.isEmpty ? "—" : account)
            .font(.system(size: 9, weight: .semibold, design: .monospaced))
            .padding(.horizontal, 5)
            .padding(.vertical, 1)
            .overlay(RoundedRectangle(cornerRadius: 4).stroke(hue.opacity(0.75), lineWidth: 1))
            .foregroundColor(hue)
    }

    private var hue: Color {
        var hash: UInt32 = 2166136261
        for byte in account.utf8 { hash = (hash ^ UInt32(byte)) &* 16777619 }
        return Color(hue: Double(hash % 360) / 360, saturation: 0.5, brightness: 0.9)
    }
}

/// A little segment tag in a deterministic hue, so fgc/retro/gamedev rows
/// sort themselves visually.
private struct SegmentChip: View {
    let segment: String

    var body: some View {
        Text(segment.isEmpty ? "?" : segment)
            .font(.system(size: 9, weight: .semibold))
            .padding(.horizontal, 5)
            .padding(.vertical, 1)
            .background(hue.opacity(0.22))
            .cornerRadius(4)
    }

    private var hue: Color {
        var hash: UInt32 = 2166136261
        for byte in segment.utf8 { hash = (hash ^ UInt32(byte)) &* 16777619 }
        return Color(hue: Double(hash % 360) / 360, saturation: 0.55, brightness: 0.85)
    }
}

private struct LinkButton: View {
    let title: String
    let url: URL

    var body: some View {
        Button(title) { NSWorkspace.shared.open(url) }
            .help(url.absoluteString)
    }
}

/// Row thumbnail off the queue's tiny thumbnail-10-percent.jpg (cover as
/// fallback); a film glyph when this machine has no render.
private struct ReelThumb: View {
    let path: String?
    @State private var image: NSImage?

    var body: some View {
        ZStack {
            Color.secondary.opacity(0.12)
            if let image = image {
                Image(nsImage: image)
                    .resizable()
                    .aspectRatio(contentMode: .fill)
            } else {
                Image(systemName: "film").foregroundColor(.secondary)
            }
        }
        .frame(width: 36, height: 64)
        .clipped()
        .cornerRadius(4)
        .onAppear {
            if image == nil, let path = path { image = NSImage(contentsOfFile: path) }
        }
    }
}

// MARK: - formatting ("—" is "not measured", never zero)

private func whole(_ value: Double?) -> String {
    guard let value = value else { return "—" }
    return String(Int(value.rounded()))
}

private func pct(_ value: Double?) -> String {
    guard let value = value else { return "—" }
    return String(format: "%.1f%%", value)
}

private func seconds(fromMs value: Double?) -> String {
    guard let value = value else { return "—" }
    return String(format: "%.1fs", value / 1000)
}

private func shortDate(_ iso: String?) -> String? {
    guard let iso = iso else { return nil }
    let fractional = ISO8601DateFormatter()
    fractional.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
    guard let date = fractional.date(from: iso) ?? ISO8601DateFormatter().date(from: iso)
    else { return String(iso.prefix(10)) }
    let out = DateFormatter()
    out.dateStyle = .medium
    out.timeStyle = .short
    return out.string(from: date)
}
