import AppKit
import PDFKit
import WebKit
import UniformTypeIdentifiers

/// Live-transcription score card backed by Verovio. The view stacks a
/// transparent WKWebView (Resources/verovio/sheet.html) under a
/// PlayheadOverlay NSView. Verovio handles the engraving + bitmap; the
/// overlay handles real-time animations. The view is also a drag
/// SOURCE (drag the page off → PDF on desktop with the score
/// embedded) and a drag DESTINATION (drop a previously-exported PDF
/// back in → restores the composition for playback).
final class SheetMusicView: NSView, WKNavigationDelegate, WKScriptMessageHandler,
                            NSDraggingSource, NSFilePromiseProviderDelegate {
    weak var menuBand: MenuBandController?

    private var webView: WKWebView!
    private var overlay: PlayheadOverlay!
    private var isLoaded = false
    private var pendingEvents: [String] = []
    private var dragMouseDownAt: NSPoint?
    /// Latest MusicXML, refreshed by every JS-side layout message.
    /// Read at drag-start to embed in the exported PDF. Cached here
    /// rather than fetched async from the WKWebView so the drag
    /// source can render the PDF eagerly and not depend on the
    /// popover (and webView) staying alive across the drag.
    private var cachedMusicXML: String = ""
    /// Latest pre-rendered PDF on disk, refreshed asynchronously
    /// after every layout message via `WKWebView.createPDF`. Read
    /// synchronously by `beginPDFDrag` so the drag source kicks off
    /// instantly with no blocking print operation on the main
    /// thread. The previous file-promise + `NSPrintOperation.run()`
    /// path hung the popover from inside `mouseDragged` because
    /// AppKit doesn't tolerate nested event loops during tracking.
    private var cachedPDFURL: URL?
    private var pdfRenderInFlight = false
    /// Idle-debounce timer for PDF refreshes. Each layout message
    /// (= every recordNote rerender) schedules a refresh, but the
    /// refresh itself momentarily toggles `body.exporting` on the
    /// visible page to strip the noise filter for PDF size — which
    /// flashes the cream background to white during the createPDF
    /// window. Coalescing on a quiet ~250ms idle window means a
    /// burst of keypresses produces one flash (when they stop)
    /// instead of one per note.
    private var pdfRefreshTimer: Timer?
    private static let pdfRefreshDebounce: TimeInterval = 0.25

    // MARK: - "Menu Band PDF" embedded-score format
    //
    // A Menu Band PDF is a regular PDF whose standard Info
    // dictionary carries the round-trip payload:
    //
    //   • Subject  — full MusicXML representation of the score
    //                (Verovio's MusicXML output; round-trips
    //                losslessly through `loadData` → SVG/MIDI).
    //   • Keywords — `["MenuBandFormat:1"]`. Identifies the PDF
    //                as a Menu Band score and carries the schema
    //                version. Bump on any incompatible change.
    //   • Title    — "Menu Band score" (cosmetic, shown by Finder).
    //   • Creator  — "Menu Band" (cosmetic, shown by Finder).
    //
    // Why standard fields and not custom Info keys: an earlier
    // version stored the XML under `/MenuBandMusicXML` and the
    // version under `/MenuBandFormat`. PDFKit's
    // `documentAttributes` setter happily wrote those, but the
    // SERIALIZER emitted them as PDF *strings* — `(MenuBandMusicXML)`
    // — instead of PDF *names* — `/MenuBandMusicXML`. PDF spec
    // requires Info dict keys to be names, so CoreGraphics's
    // stricter parser rejected the whole dict on read with
    // "found non-name key while building dictionary". `Subject`
    // and `Keywords` are predefined PDFDocumentAttribute keys that
    // PDFKit serializes correctly, so the round-trip works.
    //
    // Round-trip:
    //   drag OUT — `schedulePDFRefresh` writes the four fields via
    //              `PDFDocument.documentAttributes` + `write(to:)`.
    //   drag IN  — `loadAndPlay` reads `Subject` (with a legacy-
    //              format fallback that grep-extracts the old
    //              `/MenuBandMusicXML` parenthesis-string from the
    //              raw bytes), hands the XML to
    //              `Sheet.loadXMLAndExportMIDI`, and routes
    //              Verovio's MIDI render through `MidiFilePlayer`
    //              so the score auto-plays.
    private static let pdfFormatKeyword = "MenuBandFormat:1"
    /// Legacy custom-key fallback — read-only, for PDFs exported
    /// by versions before the move to standard Info fields.
    private static let pdfMusicXMLKey = "MenuBandMusicXML"

    /// 8.5×11 letter aspect ratio so the card looks like a real
    /// printable page. Used by the popover to size width from height.
    static let letterAspect: CGFloat = 11.0 / 8.5

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        // Layer-backed so we can transform the whole sheet (webView +
        // overlay together) for the drag-out crumple animation. Also
        // gives us a paper-colored backing — if WebKit's compositor
        // ever drops the page surface mid-rerender, the gap behind
        // shows cream (matching #page's CSS background) instead of
        // a stark white flash that reads as a flicker.
        wantsLayer = true
        layer?.backgroundColor = NSColor(
            calibratedRed: 248.0/255.0, green: 239.0/255.0,
            blue: 217.0/255.0, alpha: 1.0
        ).cgColor
        setUpWebView()
        setUpOverlay()
        registerForDraggedTypes([.fileURL])
    }

    required init?(coder: NSCoder) { nil }

    override func layout() {
        super.layout()
        webView.frame = bounds
        overlay.frame = bounds
        // Once the popover is actually on screen and the webView has
        // a non-zero size, prime the PDF cache. Without this guard
        // a too-early `createPDF` on a 0×0 webView hangs forever and
        // poisons pdfRenderInFlight. Re-prime on size changes too —
        // the layout messages from Verovio renders fire only on note
        // changes, not on plain resizes.
        if isLoaded && webView.bounds.width > 0 && webView.bounds.height > 0
            && cachedPDFURL == nil && !pdfRenderInFlight {
            schedulePDFRefresh()
        }
    }

    private func setUpWebView() {
        let config = WKWebViewConfiguration()
        config.preferences.setValue(true, forKey: "allowFileAccessFromFileURLs")
        // Bridge: JS posts {type: 'layout', measures: [...]} after each
        // render. Swift uses those boxes to position the playhead.
        let userContent = WKUserContentController()
        userContent.add(self, name: "sheet")
        config.userContentController = userContent
        let webView = WKWebView(frame: bounds, configuration: config)
        webView.autoresizingMask = [.width, .height]
        webView.setValue(false, forKey: "drawsBackground")
        webView.navigationDelegate = self
        addSubview(webView)
        self.webView = webView
        loadSheet()
    }

    private func setUpOverlay() {
        let overlay = PlayheadOverlay(frame: bounds)
        overlay.autoresizingMask = [.width, .height]
        addSubview(overlay)
        self.overlay = overlay
    }

    // MARK: - Hit testing
    //
    // The WKWebView and PlayheadOverlay are subviews; without
    // intervention, clicks/drags route to them and SheetMusicView's
    // mouseDown/mouseDragged never fire. Override hitTest so every
    // event over the page surface lands on SheetMusicView so it can
    // start a drag from anywhere on the score. (The old in-popover
    // clear button is gone — drag to Trash to clear.)

    override func hitTest(_ point: NSPoint) -> NSView? {
        return bounds.contains(point) ? self : nil
    }

    override func mouseDown(with event: NSEvent) {
        dragMouseDownAt = convert(event.locationInWindow, from: nil)
    }

    override func mouseDragged(with event: NSEvent) {
        guard let start = dragMouseDownAt else { return }
        let p = convert(event.locationInWindow, from: nil)
        // Threshold so a casual click doesn't start a drag.
        if abs(p.x - start.x) > 4 || abs(p.y - start.y) > 4 {
            beginPDFDrag(with: event)
            dragMouseDownAt = nil
        }
    }

    override func mouseUp(with event: NSEvent) {
        dragMouseDownAt = nil
    }

    // MARK: - Drag SOURCE — page → PDF on desktop
    //
    // Eager strategy: render the PDF to a temp file BEFORE calling
    // beginDraggingSession, then drag the file URL. The popover panel
    // can close mid-drag (any number of paths trigger that) — when it
    // does, this view + its WKWebView are torn down, and any deferred
    // print operation crashes the UI before producing a file. Eager
    // render also makes the drag look instantaneous to apps that
    // accept file URLs directly (Finder, Mail, browser uploads).

    private func beginPDFDrag(with event: NSEvent) {
        NSLog("SheetMusicView: beginPDFDrag — cachedPDFURL=\(cachedPDFURL?.lastPathComponent ?? "nil"), inFlight=\(pdfRenderInFlight)")
        // If a debounced refresh is pending, run it now so the
        // exported PDF reflects whatever the user just played up
        // to this moment. createPDF is async — by the time it
        // completes, the drag will already be in flight reading
        // the existing cachedPDFURL, but the next drag picks up
        // the fresh one. Acceptable: one drag may be slightly
        // stale, but no drag is ever lost.
        if pdfRefreshTimer != nil {
            pdfRefreshTimer?.invalidate()
            pdfRefreshTimer = nil
            performPDFRefresh()
        }
        guard let pdfURL = cachedPDFURL else {
            // First-render race: kick a refresh and bail. The next
            // drag attempt will land instantly. (In practice the
            // sheet always renders before the user has anything
            // worth dragging, so this branch is rare.)
            schedulePDFRefresh()
            return
        }
        let item = NSDraggingItem(pasteboardWriter: pdfURL as NSURL)
        let snap = snapshotImage()
        item.setDraggingFrame(bounds, contents: snap)
        // The actual sheet view crumples away while the drag image
        // travels with the cursor — feels like ripping the page off.
        // The crumple sound itself is reserved for the Trash drop
        // (handled in draggingSession:endedAt:operation:) so it
        // reads specifically as "this score is being thrown away."
        animateCrumpleAway()
        beginDraggingSession(with: [item], event: event, source: self)
    }

    // MARK: - Crumple animation
    //
    // On drag-out we transform the layer-backed SheetMusicView away
    // (scale + rotation + fade) so the popover visibly empties as
    // the user pulls the page out. The dragImage is the snapshot
    // that travels with the cursor; this is the *source* view
    // animation, not the drag visual.
    //
    // Successful drop (.copy or .delete) → clearScore() rebuilds
    // an empty staff and `restoreLayerInstant()` snaps back to
    // identity so the next score is visible immediately.
    //
    // Cancelled drop (operation == []) → `animateCrumpleBack()`
    // springs the original sheet back into place.

    private func animateCrumpleAway() {
        guard let layer = layer else { return }
        layer.removeAllAnimations()
        let dir: CGFloat = Bool.random() ? 1 : -1
        let duration: CFTimeInterval = 0.22
        let scale = CABasicAnimation(keyPath: "transform.scale")
        scale.fromValue = 1.0
        scale.toValue = 0.05
        let rotation = CABasicAnimation(keyPath: "transform.rotation.z")
        rotation.fromValue = 0.0
        rotation.toValue = dir * 0.6
        let fade = CABasicAnimation(keyPath: "opacity")
        fade.fromValue = 1.0
        fade.toValue = 0.0
        let group = CAAnimationGroup()
        group.animations = [scale, rotation, fade]
        group.duration = duration
        group.timingFunction = CAMediaTimingFunction(name: .easeIn)
        group.fillMode = .forwards
        group.isRemovedOnCompletion = false
        layer.add(group, forKey: "crumpleAway")
    }

    private func animateCrumpleBack() {
        guard let layer = layer else { return }
        layer.removeAllAnimations()
        // Spring back to identity from wherever the away animation
        // left us. Reading presentationLayer here would be more
        // accurate but the away animation always lands at the same
        // end state, so hard-coding fromValue is simpler + matches.
        let duration: CFTimeInterval = 0.28
        let scale = CABasicAnimation(keyPath: "transform.scale")
        scale.fromValue = 0.05
        scale.toValue = 1.0
        let rotation = CABasicAnimation(keyPath: "transform.rotation.z")
        rotation.fromValue = layer.value(forKeyPath: "transform.rotation.z") ?? 0.0
        rotation.toValue = 0.0
        let fade = CABasicAnimation(keyPath: "opacity")
        fade.fromValue = 0.0
        fade.toValue = 1.0
        let group = CAAnimationGroup()
        group.animations = [scale, rotation, fade]
        group.duration = duration
        group.timingFunction = CAMediaTimingFunction(name: .easeOut)
        layer.add(group, forKey: "crumpleBack")
        layer.opacity = 1.0
        layer.transform = CATransform3DIdentity
    }

    private func restoreLayerInstant() {
        guard let layer = layer else { return }
        layer.removeAllAnimations()
        layer.opacity = 1.0
        layer.transform = CATransform3DIdentity
    }

    /// Kick off an async PDF render of the current sheet. Coalesces
    /// rapid layout bursts so we don't pile up redundant renders
    /// when a chord arrives. Updates `cachedPDFURL` when the file
    /// is on disk with the MusicXML round-trip attribute injected.
    private func schedulePDFRefresh() {
        // Debounce: every layout message scheduled this; coalesce
        // a burst of recordNote rerenders into one PDF refresh that
        // fires once the user has stopped playing. The actual
        // refresh body is in `performPDFRefresh()`.
        pdfRefreshTimer?.invalidate()
        pdfRefreshTimer = Timer.scheduledTimer(
            withTimeInterval: Self.pdfRefreshDebounce, repeats: false
        ) { [weak self] _ in
            self?.performPDFRefresh()
        }
    }

    private func performPDFRefresh() {
        pdfRefreshTimer = nil
        guard !pdfRenderInFlight else {
            NSLog("SheetMusicView: performPDFRefresh skipped (in-flight)")
            return
        }
        guard webView.bounds.width > 0, webView.bounds.height > 0 else {
            // createPDF on a 0×0 webView never completes. Skipping
            // here (without setting the in-flight flag) keeps the
            // pipeline unpoisoned until a real layout brings the
            // view to size.
            NSLog("SheetMusicView: performPDFRefresh skipped (zero bounds)")
            return
        }
        pdfRenderInFlight = true
        // Defensive timeout — if createPDF ever hangs (e.g. WKWebView
        // misbehaves on a transient size change), reset the in-flight
        // flag so the next layout message can try again.
        DispatchQueue.main.asyncAfter(deadline: .now() + 5) { [weak self] in
            guard let self = self else { return }
            if self.pdfRenderInFlight {
                NSLog("SheetMusicView: createPDF timed out — resetting in-flight")
                self.pdfRenderInFlight = false
            }
        }
        let xmlAtRender = cachedMusicXML
        let stamp = Self.fileTimestamp.string(from: Date())
        let tmpDir = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
            .appendingPathComponent("MenuBand-PDFDrags", isDirectory: true)
        try? FileManager.default.createDirectory(
            at: tmpDir, withIntermediateDirectories: true)
        let pdfURL = tmpDir.appendingPathComponent("Menu-Band-\(stamp).pdf")
        NSLog("SheetMusicView: PDF refresh starting → \(pdfURL.lastPathComponent), webView.bounds=\(webView.bounds), xml=\(xmlAtRender.count) chars")

        // Fetch the wall-clock event list FIRST so it's available
        // when we write the PDF. Chained to avoid a race between
        // the async eval and the async createPDF — without this
        // chain the events JSON could be missing or stale when
        // the file is finalised.
        webView.evaluateJavaScript("Sheet.getPlaybackEvents()") { [weak self] eventsResult, _ in
            guard let self = self else { return }
            let eventsJSON = (eventsResult as? String) ?? "[]"
            // Toggle the export-only stylesheet (kills the SVG noise
            // filter + box shadows, which can't be vectorized into
            // PDF and otherwise rasterize to 200+ MB at Retina DPI).
            // Removed in the createPDF completion below.
            self.webView.evaluateJavaScript(
                "document.body.classList.add('exporting')",
                completionHandler: nil)
            self.runCreatePDF(to: pdfURL,
                              eventsJSON: eventsJSON,
                              xmlAtRender: xmlAtRender)
        }
    }

    /// Inner half of the async chain — performs the actual PDF
    /// render once the events list has been captured.
    private func runCreatePDF(to pdfURL: URL,
                              eventsJSON: String,
                              xmlAtRender: String) {
        // Modern WKWebView async API — renders the page to a PDF
        // off the main runloop without entering a nested event
        // loop, so calling this during tracking is safe.
        webView.createPDF { [weak self] result in
            guard let self = self else { return }
            self.pdfRenderInFlight = false
            self.webView.evaluateJavaScript(
                "document.body.classList.remove('exporting')",
                completionHandler: nil)
            switch result {
            case .success(let data):
                NSLog("SheetMusicView: createPDF success — \(data.count) bytes")
                do {
                    try data.write(to: pdfURL)
                } catch {
                    NSLog("SheetMusicView: PDF write failed — \(error)")
                    return
                }
                if let pdf = PDFDocument(url: pdfURL) {
                    var attrs = pdf.documentAttributes ?? [:]
                    attrs[PDFDocumentAttribute.subjectAttribute] = xmlAtRender
                    attrs[PDFDocumentAttribute.keywordsAttribute] = [
                        Self.pdfFormatKeyword,
                        "events:\(eventsJSON)",
                    ]
                    attrs[PDFDocumentAttribute.creatorAttribute] = "Menu Band"
                    attrs[PDFDocumentAttribute.titleAttribute] = "Menu Band score"
                    pdf.documentAttributes = attrs
                    pdf.write(to: pdfURL)
                }
                // Don't delete the previous cached file even though
                // it's superseded — `cachedPDFURL` is what the
                // pasteboard writer hands to the OS at drag-start,
                // and the drop target may not consult it until tens
                // of ms later (especially for cross-app drops). A
                // delete here races with that read and turns the
                // dropped PDF into a "file not found" silent fail.
                // /tmp is cleaned at boot, so the leak is bounded.
                self.cachedPDFURL = pdfURL
            case .failure(let error):
                NSLog("SheetMusicView: createPDF failed — \(error)")
            }
        }
    }

    private func snapshotImage() -> NSImage {
        let rep = bitmapImageRepForCachingDisplay(in: bounds) ?? NSBitmapImageRep()
        cacheDisplay(in: bounds, to: rep)
        let img = NSImage(size: bounds.size)
        img.addRepresentation(rep)
        return img
    }

    // NSDraggingSource

    func draggingSession(
        _ session: NSDraggingSession,
        sourceOperationMaskFor context: NSDraggingContext
    ) -> NSDragOperation {
        // `.delete` enables Trash as a drop target — dragging onto
        // the Dock's Trash now clears the staff just like dropping
        // on Finder did (and replaces the old in-popover trash
        // button). `.copy` keeps the regular drop-to-export path
        // working anywhere else.
        return [.copy, .delete]
    }

    /// Clear the staff once the drag ends — only on a successful
    /// drop (operation != []), so an aborted drag (drop on the
    /// originating popover, drop outside any target, Esc-cancel)
    /// leaves the in-progress score untouched. Treat the drop as
    /// "I'm done with this take, start the next one fresh."
    func draggingSession(
        _ session: NSDraggingSession,
        endedAt screenPoint: NSPoint,
        operation: NSDragOperation
    ) {
        guard operation != [] else {
            // Cancelled: spring the crumpled-away sheet back into place.
            animateCrumpleBack()
            return
        }
        // Trash drop = .delete. That's the specific "throw this
        // away" gesture, so the paper-crumple SFX fires only here
        // — a Finder/Mail/etc drop (.copy) clears the staff silently.
        if operation.contains(.delete) {
            CrumpleSound.shared.play()
        }
        restoreLayerInstant()
        clearScore()
    }

    // NSFilePromiseProviderDelegate is no longer needed (eager render
    // path uses NSURL pasteboard writers directly), but keeping the
    // protocol conformance keeps compile-time API stable for any
    // callers we may re-introduce later. Stub the required methods
    // with do-nothing fallbacks.

    func filePromiseProvider(
        _ filePromiseProvider: NSFilePromiseProvider,
        fileNameForType fileType: String
    ) -> String {
        return "Menu-Band-\(Self.fileTimestamp.string(from: Date())).pdf"
    }

    func filePromiseProvider(
        _ filePromiseProvider: NSFilePromiseProvider,
        writePromiseTo url: URL,
        completionHandler: @escaping (Error?) -> Void
    ) {
        completionHandler(nil)
    }

    private static let fileTimestamp: DateFormatter = {
        let df = DateFormatter()
        df.dateFormat = "yyyyMMdd-HHmmss"
        return df
    }()

    // MARK: - Drag DESTINATION — PDF dragged in → restore score

    override func draggingEntered(_ sender: NSDraggingInfo) -> NSDragOperation {
        return acceptablePDFURL(from: sender) != nil ? .copy : []
    }

    override func draggingUpdated(_ sender: NSDraggingInfo) -> NSDragOperation {
        return acceptablePDFURL(from: sender) != nil ? .copy : []
    }

    override func performDragOperation(_ sender: NSDraggingInfo) -> Bool {
        guard let url = acceptablePDFURL(from: sender) else { return false }
        return loadAndPlay(pdfURL: url)
    }

    /// Public entry point for "PDF score → re-render staff + auto-
    /// play through the synth." Used by both `performDragOperation`
    /// (drop on the score area inside the popover) AND by
    /// AppDelegate's status-item drop handler (drop on the menubar
    /// piano icon, mirroring the .mid drag-onto-menubar flow). Returns
    /// false on any failure (not a Menu Band PDF, no MusicXML, JS
    /// eval errored). The visual side renders best-effort even when
    /// MIDI export fails — the user at least sees the staff redrawn.
    @discardableResult
    func loadAndPlay(pdfURL: URL) -> Bool {
        let exists = FileManager.default.fileExists(atPath: pdfURL.path)
        guard exists else {
            NSLog("SheetMusicView: drop \(pdfURL.lastPathComponent) — file no longer exists at \(pdfURL.path)")
            return false
        }
        guard let xml = Self.extractMusicXML(from: pdfURL), !xml.isEmpty else {
            NSLog("SheetMusicView: drop \(pdfURL.lastPathComponent) — no Menu Band MusicXML found in PDF (neither /Subject nor legacy /MenuBandMusicXML)")
            return false
        }
        NSLog("SheetMusicView: loadAndPlay \(pdfURL.lastPathComponent) — \(xml.count) chars of MusicXML")
        // Read the wall-clock event list (Format v2). Verovio's MIDI
        // render quantises onsets to the 16th-grid which throws
        // playback timing off — the events list is what the user
        // actually played, sample-accurate. Falls back to MIDI route
        // when the PDF predates the events embedding.
        let events = Self.extractPlaybackEvents(from: pdfURL)
        // Hand the embedded MusicXML to Verovio: it re-renders the
        // staff (same code path as the live transcription, just
        // seeded from XML instead of accumulated noteOn events) and
        // returns the same content as a MIDI base64 string. We then
        // route that MIDI through MidiFilePlayer just like the .mid
        // drag-onto-menubar flow, so Swift's synth + the popover
        // visualizer stay perfectly in lockstep with the playback.
        let escaped = xml
            .replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "`", with: "\\`")
            .replacingOccurrences(of: "$", with: "\\$")
        let js = """
        (function() {
            try {
                return Sheet.loadXMLAndExportMIDI(`\(escaped)`);
            } catch (e) {
                console.error('drag-in', e);
                return '';
            }
        })()
        """
        webView.evaluateJavaScript(js) { [weak self] result, error in
            guard let self = self else { return }
            if let error = error {
                NSLog("SheetMusicView: drag-in eval failed — \(error)")
                return
            }
            // Prefer exact-timing playback from the embedded events list.
            if let events = events, !events.isEmpty {
                NSLog("SheetMusicView: drag-in playing \(events.count) events with sample-accurate timing")
                self.playEvents(events)
                return
            }
            // Legacy fallback: route Verovio's quantised MIDI through
            // MidiFilePlayer. Only used for PDFs exported before the
            // events list was embedded.
            guard let base64 = result as? String, !base64.isEmpty else {
                NSLog("SheetMusicView: drag-in returned no MIDI — playback skipped (Verovio renderToMIDI may have failed; staff still re-rendered)")
                return
            }
            NSLog("SheetMusicView: drag-in produced \(base64.count) chars of base64 MIDI — handing to player (legacy path)")
            self.playImportedMIDI(base64: base64)
        }
        return true
    }

    /// One auto-play event extracted from a Menu Band PDF.
    /// Times are seconds-from-first-onset (relative); replay
    /// converts them to wall-clock deadlines via DispatchQueue.
    struct PlaybackEvent {
        let midi: UInt8
        let on: TimeInterval
        let off: TimeInterval
    }

    private var pendingPlaybackWork: [DispatchWorkItem] = []
    /// Last events list loaded from a PDF drag-in. Retained so the
    /// popover's play/stop chrome can restart playback after the
    /// user hit stop without re-dropping the PDF.
    private var lastLoadedEvents: [PlaybackEvent]?

    /// True while a scheduled playback is mid-flight. Mirrors the
    /// `pendingPlaybackWork` queue and is what the popover keys
    /// off when deciding whether to show the stop or play button.
    var isPlaybackActive: Bool {
        !pendingPlaybackWork.isEmpty
    }

    /// True when a Menu Band PDF has been loaded (a usable events
    /// list is in hand). The popover uses this to decide whether to
    /// surface playback transport controls at all.
    var hasPlaybackEvents: Bool {
        !(lastLoadedEvents?.isEmpty ?? true)
    }

    /// Fires whenever playback starts, stops, or completes so the
    /// popover can toggle its play/stop buttons.
    var onPlaybackStateChanged: (() -> Void)?

    /// Stop any in-flight playback. Safe to call when nothing is
    /// playing. Cancels every scheduled note-on/off, silences any
    /// playback-lit notes, and clears `pendingPlaybackWork`.
    func stopPlayback() {
        guard !pendingPlaybackWork.isEmpty else { return }
        for w in pendingPlaybackWork { w.cancel() }
        pendingPlaybackWork.removeAll()
        menuBand?.releaseAllPlaybackNotes()
        // Clear any leftover staff highlights so the next play
        // starts from a clean slate.
        send("Sheet.playbackResetHighlight && Sheet.playbackResetHighlight()")
        onPlaybackStateChanged?()
    }

    /// Restart playback from the head of the last-loaded events.
    /// No-op when no PDF has been loaded.
    func restartPlayback() {
        guard let events = lastLoadedEvents, !events.isEmpty else { return }
        playEvents(events)
    }

    /// Wall-clock-accurate replay. Each event is a single
    /// `asyncAfter` for note-on plus another for note-off, so the
    /// menubar piano lights red at exactly the moment the user
    /// originally played the note. Supersedes any in-flight
    /// playback (.mid drop, prior PDF, etc) before scheduling.
    private func playEvents(_ events: [PlaybackEvent]) {
        guard let menuBand = self.menuBand else {
            NSLog("SheetMusicView: no MenuBandController — can't auto-play")
            return
        }
        // Remember the events so the popover's play button can
        // restart playback after stop without re-dropping the PDF.
        lastLoadedEvents = events
        // Stop everything in flight — MIDI player events, prior
        // PDF events, and any held playback notes — before we
        // schedule the new run.
        MidiFilePlayer.stop()
        for w in pendingPlaybackWork { w.cancel() }
        pendingPlaybackWork.removeAll()
        menuBand.releaseAllPlaybackNotes()

        let now = DispatchTime.now()
        var lastOff: TimeInterval = 0
        for event in events {
            let midi = event.midi
            let onWork = DispatchWorkItem { [weak menuBand, weak self] in
                menuBand?.startPlaybackNote(midi, velocity: 100, displayNote: midi)
                // Light up the corresponding g.note on the rendered
                // staff. Per-pitch cursor in JS advances on each call
                // so repeats hit successive notes in source order.
                self?.send("Sheet.playbackOn(\(midi))")
            }
            let offWork = DispatchWorkItem { [weak menuBand, weak self] in
                menuBand?.stopPlaybackNote(midi, displayNote: midi)
                self?.send("Sheet.playbackOff(\(midi))")
            }
            pendingPlaybackWork.append(onWork)
            pendingPlaybackWork.append(offWork)
            // Nanosecond-precision DispatchTime arithmetic — the
            // earlier `.milliseconds(Int(...))` rounded sub-ms
            // detail off every onset/release.
            DispatchQueue.main.asyncAfter(
                deadline: now + .nanoseconds(Int(event.on * 1_000_000_000)),
                execute: onWork)
            DispatchQueue.main.asyncAfter(
                deadline: now + .nanoseconds(Int(event.off * 1_000_000_000)),
                execute: offWork)
            lastOff = max(lastOff, event.off)
        }
        // Tail panic + state-cleanup in case any note's off scheduling
        // slipped. Also flips the playback-active state to false so the
        // popover's transport chrome can switch from "stop" to "play".
        let tail = DispatchWorkItem { [weak menuBand, weak self] in
            menuBand?.releaseAllPlaybackNotes()
            guard let self = self else { return }
            self.pendingPlaybackWork.removeAll()
            self.onPlaybackStateChanged?()
        }
        pendingPlaybackWork.append(tail)
        DispatchQueue.main.asyncAfter(
            deadline: now + .nanoseconds(Int((lastOff + 0.05) * 1_000_000_000)),
            execute: tail)
        NSLog("SheetMusicView: scheduled \(events.count) playback events over \(String(format: "%.2f", lastOff))s")
        onPlaybackStateChanged?()
    }

    private static func extractPlaybackEvents(from url: URL) -> [PlaybackEvent]? {
        // Format v2 stores the JSON event list as a Keyword entry
        // prefixed with "events:". PDFKit returns Keywords as either
        // an array of strings or a single comma-joined string
        // depending on how the PDF was authored — handle both.
        guard let pdf = PDFDocument(url: url),
              let attrs = pdf.documentAttributes else { return nil }
        let raw = attrs[PDFDocumentAttribute.keywordsAttribute]
        var keywords: [String] = []
        if let arr = raw as? [String] {
            keywords = arr
        } else if let s = raw as? String {
            keywords = s.split(separator: ",").map {
                $0.trimmingCharacters(in: .whitespaces)
            }
        }
        guard let entry = keywords.first(where: { $0.hasPrefix("events:") }) else {
            return nil
        }
        let json = String(entry.dropFirst("events:".count))
        guard let data = json.data(using: .utf8),
              let arr = try? JSONSerialization.jsonObject(with: data) as? [[String: Any]]
        else { return nil }
        // "Still held at export time" notes carry off: null. Take
        // them as "held until the latest released-note's release"
        // so the sustain matches what the user actually held until
        // they grabbed the drag. Falls back to on + 0.5s when no
        // other note has a release timestamp to anchor against.
        let maxRelease = arr.compactMap { $0["off"] as? Double }.max() ?? 0
        return arr.compactMap { dict -> PlaybackEvent? in
            guard let m = dict["midi"] as? Int,
                  let on = dict["on"] as? Double else { return nil }
            let off: Double
            if let explicit = dict["off"] as? Double {
                off = explicit
            } else {
                off = max(maxRelease, on + 0.5)
            }
            return PlaybackEvent(midi: UInt8(max(0, min(127, m))),
                                 on: on, off: off)
        }
    }

    /// Decode Verovio's base64-encoded MIDI rendering into a temp
    /// `.mid` file and play it through `MidiFilePlayer` so each
    /// note onset/release fires the same `startTapNote` /
    /// `stopTapNote` callbacks the real piano + the .mid drag flow
    /// use. The popover staff lights up in step with the synth as
    /// the dragged-in score plays itself back.
    private func playImportedMIDI(base64: String) {
        guard let menuBand = self.menuBand else {
            NSLog("SheetMusicView: no MenuBandController — can't auto-play")
            return
        }
        guard let data = Data(base64Encoded: base64,
                              options: .ignoreUnknownCharacters) else {
            NSLog("SheetMusicView: MIDI base64 decode failed")
            return
        }
        let tmpDir = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
            .appendingPathComponent("MenuBand-PDFDrags", isDirectory: true)
        try? FileManager.default.createDirectory(
            at: tmpDir, withIntermediateDirectories: true)
        let stamp = Self.fileTimestamp.string(from: Date())
        let midiURL = tmpDir.appendingPathComponent("Menu-Band-import-\(stamp).mid")
        do {
            try data.write(to: midiURL)
        } catch {
            NSLog("SheetMusicView: MIDI write failed — \(error)")
            return
        }
        // Stop any in-flight playback (.mid drop or a previous
        // drag-in) and silence held notes before kicking off the
        // new one — superseding playback shouldn't strand notes.
        MidiFilePlayer.stop()
        menuBand.releaseAllHeldNotes()
        let started = MidiFilePlayer.play(
            url: midiURL,
            onNoteOn: { [weak menuBand, weak self] midi, velocity in
                menuBand?.startTapNote(midi, velocity: velocity,
                                       pan: 0, displayNote: midi)
                self?.send("Sheet.playbackOn(\(midi))")
            },
            onNoteOff: { [weak menuBand, weak self] midi in
                menuBand?.stopTapNote(midi)
                self?.send("Sheet.playbackOff(\(midi))")
            },
            onFinish: { [weak menuBand] in
                menuBand?.releaseAllHeldNotes()
            }
        )
        if !started {
            NSLog("SheetMusicView: imported MIDI had no playable events")
        }
    }

    /// Pull the MusicXML payload out of a Menu Band PDF.
    /// Modern format: `documentAttributes[.subjectAttribute]` carries
    /// the XML directly. Legacy format (early prototypes): the XML
    /// was written under a custom `/MenuBandMusicXML` key — PDFKit
    /// serialized those incorrectly so PDFDocument can't read them
    /// back, but the bytes ARE in the file. As a fallback we scan
    /// the raw PDF for the legacy parenthesis-string form. Returns
    /// nil if neither form yields a non-empty payload that looks
    /// like MusicXML.
    private static func extractMusicXML(from url: URL) -> String? {
        // Modern path: standard /Subject Info field.
        if let pdf = PDFDocument(url: url),
           let attrs = pdf.documentAttributes,
           let subject = attrs[PDFDocumentAttribute.subjectAttribute] as? String,
           subject.contains("score-partwise") || subject.contains("<score") {
            return subject
        }
        // Legacy fallback: grep the raw bytes for the old custom
        // key that PDFKit wrote as a (string) instead of /Name.
        // Format on disk:
        //   (MenuBandMusicXML) (<?xml ...>...) /Title (...) ...
        // We find the literal "(MenuBandMusicXML)", skip whitespace
        // + the next "(" that opens the value, then collect until
        // the matching ")" — handling escaped \( and \) along the way.
        guard let data = try? Data(contentsOf: url) else { return nil }
        guard let needle = "(MenuBandMusicXML)".data(using: .ascii),
              let needleRange = data.range(of: needle) else { return nil }

        // Walk forward looking for the opening '(' of the value.
        var i = needleRange.upperBound
        while i < data.count, data[i] == 0x20 || data[i] == 0x0A
                          || data[i] == 0x0D || data[i] == 0x09 {
            i += 1
        }
        guard i < data.count, data[i] == UInt8(ascii: "(") else { return nil }
        i += 1
        let valueStart = i

        // Collect bytes until we hit the matching unescaped ')'.
        // PDF strings allow \\, \(, \) escapes plus nested balanced
        // parens (rare in practice). Track depth defensively.
        var depth = 1
        var bytes: [UInt8] = []
        while i < data.count {
            let b = data[i]
            if b == UInt8(ascii: "\\") && i + 1 < data.count {
                let next = data[i + 1]
                // PDF string escapes: \n \r \t \b \f \( \) \\ + octal
                switch next {
                case UInt8(ascii: "n"): bytes.append(0x0A)
                case UInt8(ascii: "r"): bytes.append(0x0D)
                case UInt8(ascii: "t"): bytes.append(0x09)
                case UInt8(ascii: "b"): bytes.append(0x08)
                case UInt8(ascii: "f"): bytes.append(0x0C)
                case UInt8(ascii: "("), UInt8(ascii: ")"), UInt8(ascii: "\\"):
                    bytes.append(next)
                default:
                    // Octal escape \ddd — collect up to 3 octal digits.
                    if next >= UInt8(ascii: "0") && next <= UInt8(ascii: "7") {
                        var octal = 0
                        var consumed = 0
                        var j = i + 1
                        while j < data.count, consumed < 3,
                              data[j] >= UInt8(ascii: "0"),
                              data[j] <= UInt8(ascii: "7") {
                            octal = octal * 8 + Int(data[j] - UInt8(ascii: "0"))
                            j += 1
                            consumed += 1
                        }
                        bytes.append(UInt8(octal & 0xFF))
                        i = j
                        continue
                    }
                    // Unknown escape → drop the backslash, keep next.
                    bytes.append(next)
                }
                i += 2
                continue
            }
            if b == UInt8(ascii: "(") { depth += 1 }
            if b == UInt8(ascii: ")") {
                depth -= 1
                if depth == 0 { break }
            }
            bytes.append(b)
            i += 1
        }
        _ = valueStart
        guard !bytes.isEmpty else { return nil }
        return String(data: Data(bytes), encoding: .utf8)
    }

    private func acceptablePDFURL(from sender: NSDraggingInfo) -> URL? {
        guard let urls = sender.draggingPasteboard.readObjects(
            forClasses: [NSURL.self],
            options: [.urlReadingFileURLsOnly: true]
        ) as? [URL] else { return nil }
        return urls.first { $0.pathExtension.lowercased() == "pdf" }
    }

    private func loadSheet() {
        guard let htmlURL = Bundle.module.url(
            forResource: "sheet",
            withExtension: "html"
        ) else {
            NSLog("SheetMusicView: sheet.html not found in bundle")
            return
        }
        let dir = htmlURL.deletingLastPathComponent()
        webView.loadFileURL(htmlURL, allowingReadAccessTo: dir)
    }

    // MARK: - WKNavigationDelegate

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        pollForReady(retries: 60)
    }

    private func pollForReady(retries: Int) {
        guard retries > 0 else { return }
        webView.evaluateJavaScript("typeof Sheet !== 'undefined' && Sheet && (typeof verovio !== 'undefined') && (verovio.module && verovio.module.calledRun === true)") { [weak self] result, _ in
            guard let self = self else { return }
            if (result as? Bool) == true {
                self.isLoaded = true
                let pending = self.pendingEvents
                self.pendingEvents.removeAll()
                for js in pending {
                    self.webView.evaluateJavaScript(js, completionHandler: nil)
                }
                // Don't prime the cache here — at sheet-ready the
                // view may not be on screen yet (bounds=0). The
                // override of `layout()` kicks the first refresh
                // once the popover opens and gives the webView a
                // real size.
            } else {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                    self.pollForReady(retries: retries - 1)
                }
            }
        }
    }

    // MARK: - WKScriptMessageHandler

    func userContentController(
        _ userContentController: WKUserContentController,
        didReceive message: WKScriptMessage
    ) {
        guard message.name == "sheet",
              let dict = message.body as? [String: Any],
              let type = dict["type"] as? String else { return }
        switch type {
        case "layout":
            if let xml = dict["musicXML"] as? String { cachedMusicXML = xml }
            handleLayout(dict)
            // Re-render the PDF in the background so the next drag
            // pulls the freshest version. Coalesced via the in-flight
            // flag so a burst of layouts doesn't pile up renders.
            schedulePDFRefresh()
        default:
            break
        }
    }

    private func handleLayout(_ dict: [String: Any]) {
        guard let measures = dict["measures"] as? [[String: Any]],
              let pageWidth = dict["pageWidth"] as? CGFloat,
              let pageHeight = dict["pageHeight"] as? CGFloat else { return }
        let parsed: [PlayheadOverlay.Measure] = measures.compactMap { m in
            guard let layerX = m["layerX"] as? CGFloat,
                  let layerWidth = m["layerWidth"] as? CGFloat,
                  let staffY = m["staffY"] as? CGFloat,
                  let staffHeight = m["staffHeight"] as? CGFloat else { return nil }
            let beats = (m["beats"] as? [Any])?.compactMap { $0 as? CGFloat } ?? []
            return PlayheadOverlay.Measure(
                layerX: layerX,
                layerWidth: layerWidth,
                staffY: staffY,
                staffHeight: staffHeight,
                beatAnchors: beats
            )
        }
        overlay.applyLayout(measures: parsed, pageSize: NSSize(width: pageWidth, height: pageHeight))
    }

    // MARK: - Public API (called by MenuBandPopover)

    // FIXME(timing/octaves): playback fidelity is still rough.
    //
    // Today's flow records each onset as a quarter (or whatever
    // `durationOf` rounds it to) and Verovio's renderToMIDI replays
    // them sequentially at 120 BPM. Two visible failures:
    //
    //   • Sparse playing → notes back-to-back during playback, so
    //     a slow performance plays back fast.
    //   • Verovio's MIDI octaves can drift from what the user
    //     played (under investigation — see MidiFilePlayer log).
    //
    // Planned fix (jas, 2026-05-09): anchor the score timeline to
    // the FIRST note onset, start a measure timer (4/4, default
    // 120 BPM unless metronome is active), quantize all subsequent
    // onsets to a 16th-note grid, and run a sliding 15-second
    // buffer (~7.5 measures at 120 BPM = roughly one staff page).
    // The MusicXML then carries a `<sound tempo="…"/>` directive
    // so Verovio's renderToMIDI replays at the same tempo the user
    // performed at, with rests preserving inter-note silences.
    func recordNote(pitch: UInt8) {
        // Capture wall-clock at the moment Swift sees the input,
        // not when the JS bridge delivers our message. The bridge
        // latency is variable (5–50ms) — without an explicit time
        // every recorded onset jitters by that amount and replay
        // sounds sloppy.
        let t = CACurrentMediaTime()
        NSLog("SheetMusicView: recordNote midi=\(pitch) t=\(String(format: "%.4f", t))")
        send("Sheet.noteOn(\(pitch), \(t))")
    }

    func releaseNote(pitch: UInt8) {
        let t = CACurrentMediaTime()
        send("Sheet.noteOff(\(pitch), \(t))")
    }

    /// Forward a metronome beat both to the JS layer (so it can
    /// snap onsets to ticks) and to the overlay (which advances
    /// the playhead one quarter-beat across the visible measures).
    func beat() {
        send("Sheet.beat()")
        overlay.advanceBeat()
    }

    func clearScore() {
        send("Sheet.clear()")
        overlay.reset()
    }

    func printScore() {
        let info = NSPrintInfo.shared.copy() as! NSPrintInfo
        info.orientation = .portrait
        info.topMargin = 36
        info.bottomMargin = 36
        info.leftMargin = 36
        info.rightMargin = 36
        let op = webView.printOperation(with: info)
        op.showsPrintPanel = true
        op.showsProgressPanel = true
        op.run()
    }

    private func send(_ js: String) {
        if !isLoaded {
            pendingEvents.append(js)
            return
        }
        webView.evaluateJavaScript(js, completionHandler: nil)
    }
}

/// Transparent layer painted on top of the Verovio SVG. Renders the
/// playhead — a soft vertical line that sweeps across each measure
/// in time with metronome beats — plus any future per-frame chrome
/// (hit flashes, sustain bars, etc.). Stays in `flipped: true` so
/// JS-derived box coordinates (HTML top-left origin) map directly.
private final class PlayheadOverlay: NSView {
    /// Per-measure geometry posted by the JS layer after each
    /// Verovio render. layerX/layerWidth define the playable
    /// horizontal region (no clef/time-sig padding); beatAnchors
    /// holds the X coordinate of each notated beat (one per
    /// quarter-rest/note placed by Verovio inside the layer).
    /// staffY/staffHeight cover only the 5-line staff bbox.
    struct Measure {
        let layerX: CGFloat
        let layerWidth: CGFloat
        let staffY: CGFloat
        let staffHeight: CGFloat
        let beatAnchors: [CGFloat]
    }

    private struct Layout {
        let pageSize: NSSize
        let measures: [Measure]
    }

    private var layout: Layout?
    private let beatsPerMeasure: Int = 4
    private var displayLink: CVDisplayLink?

    /// Beats received since reset. The playhead's continuous position
    /// is `(beatIndex - 1) + clamp((now - lastBeatAt) / beatInterval, 0..1)`
    /// — i.e. it sweeps smoothly from beat N's position to beat N+1's
    /// position over the course of one beat interval. Real-time
    /// interpolation keeps the line moving at constant velocity
    /// instead of jumping per-tick.
    private var beatIndex: Int = 0
    private var lastBeatAt: TimeInterval = 0
    private var beatInterval: TimeInterval = 0.5  // bootstrap until two beats observed
    /// Bright-flash strength (0–1). Pulses to 1 on each beat and
    /// decays so the line briefly glows when a beat lands.
    private var flash: CGFloat = 0
    /// Pre-resolved playhead geometry from the most recent frame —
    /// cached here so the draw call is just a paint of an NSRect.
    private var headX: CGFloat = -1
    private var headY: CGFloat = 0
    private var headHeight: CGFloat = 0

    /// Beat tick markers — small vertical bars that drop from above
    /// the staff onto each beat position the playhead crosses, then
    /// fade. Reads as a metronome dropping ticks onto the page.
    private struct BeatTick {
        let x: CGFloat
        let staffY: CGFloat
        let staffHeight: CGFloat
        let firedAt: TimeInterval
    }
    private var beatTicks: [BeatTick] = []
    private let beatTickLifetime: TimeInterval = 1.0
    private let beatTickDropDistance: CGFloat = 14

    override var isFlipped: Bool { true }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let now = CACurrentMediaTime()
        // Dropped beat ticks under the playhead — small vertical
        // bars that fall from above the staff onto each beat
        // position the metronome lands on, then fade out.
        for tick in beatTicks {
            let age = now - tick.firedAt
            let lifeFrac = min(1, age / beatTickLifetime)
            // Drop from -beatTickDropDistance above the staff to
            // 0 over the first ~150ms, then sit and fade.
            let dropProgress = min(1, age / 0.15)
            let yOffset = -beatTickDropDistance * (1 - dropProgress)
            // Fade: opaque-ish for the first 200ms, decaying
            // smoothly to 0 at lifetime.
            let alpha: CGFloat = max(0, 1 - lifeFrac) * 0.55
            let bar = NSRect(
                x: tick.x - 1,
                y: tick.staffY + yOffset,
                width: 2,
                height: tick.staffHeight + (1 - dropProgress) * 6
            )
            NSColor.controlAccentColor.withAlphaComponent(alpha).setFill()
            NSBezierPath(roundedRect: bar, xRadius: 1, yRadius: 1).fill()
        }

        guard layout != nil, headX >= 0, headHeight > 0 else { return }
        let glowWidth = CGFloat(8) + flash * 12
        let glowRect = NSRect(
            x: headX - glowWidth / 2,
            y: headY,
            width: glowWidth,
            height: headHeight
        )
        NSColor.systemRed.withAlphaComponent(0.18 + flash * 0.25).setFill()
        NSBezierPath(roundedRect: glowRect,
                     xRadius: glowWidth / 2,
                     yRadius: glowWidth / 2).fill()
        let lineWidth: CGFloat = 1.4
        let lineRect = NSRect(
            x: headX - lineWidth / 2,
            y: headY,
            width: lineWidth,
            height: headHeight
        )
        NSColor.systemRed.withAlphaComponent(0.85).setFill()
        NSBezierPath(rect: lineRect).fill()
    }

    func applyLayout(measures: [Measure], pageSize: NSSize) {
        layout = Layout(pageSize: pageSize, measures: measures)
        updateHead(at: CACurrentMediaTime())
        needsDisplay = true
    }

    func advanceBeat() {
        let now = CACurrentMediaTime()
        if lastBeatAt > 0 {
            let observed = now - lastBeatAt
            beatInterval = beatInterval * 0.4 + observed * 0.6
        }
        lastBeatAt = now
        beatIndex += 1
        flash = 1
        if let pos = beatPosition(beatIndex - 1) {
            beatTicks.append(BeatTick(
                x: pos.x,
                staffY: pos.staffY,
                staffHeight: pos.staffHeight,
                firedAt: now
            ))
            beatTicks.removeAll { now - $0.firedAt > beatTickLifetime }
        }
        ensureDisplayLink()
    }

    /// Resolve a 0-based beat index to a screen-space position
    /// using either the per-beat anchors Verovio reported or, if
    /// the measure didn't post enough anchors, an evenly-divided
    /// layer width as fallback. Measures with no anchors at all
    /// fall back to layerX as a single position.
    private func beatPosition(_ beat: Int) -> (x: CGFloat, staffY: CGFloat, staffHeight: CGFloat)? {
        guard let layout, !layout.measures.isEmpty else { return nil }
        let totalBeats = layout.measures.count * beatsPerMeasure
        let wrapped = ((beat % totalBeats) + totalBeats) % totalBeats
        let measureIdx = wrapped / beatsPerMeasure
        let beatInMeasure = wrapped % beatsPerMeasure
        let m = layout.measures[measureIdx]
        let scaleX = bounds.width / max(layout.pageSize.width, 1)
        let scaleY = bounds.height / max(layout.pageSize.height, 1)
        let xRaw: CGFloat
        if !m.beatAnchors.isEmpty {
            // Snap to a real Verovio-reported beat anchor when
            // we have one for this position, otherwise fall back
            // to evenly-spaced inside the layer.
            let anchorIdx = min(beatInMeasure, m.beatAnchors.count - 1)
            xRaw = m.beatAnchors[anchorIdx]
        } else {
            let frac = CGFloat(beatInMeasure) / CGFloat(beatsPerMeasure)
            xRaw = m.layerX + m.layerWidth * frac
        }
        return (
            x: xRaw * scaleX,
            staffY: m.staffY * scaleY,
            staffHeight: m.staffHeight * scaleY
        )
    }

    func reset() {
        beatIndex = 0
        flash = 0
        lastBeatAt = 0
        headX = -1
        beatTicks.removeAll()
        stopDisplayLink()
        needsDisplay = true
    }

    /// Compute where the playhead should be RIGHT NOW given the
    /// continuous time-since-last-beat and the cached interval.
    /// Called every display-link frame so the line moves at a
    /// constant velocity between ticks.
    private func updateHead(at now: TimeInterval) {
        guard let layout, !layout.measures.isEmpty, beatIndex > 0 else {
            headX = -1
            return
        }
        let elapsed = max(0, now - lastBeatAt)
        // Clamp the inter-beat fraction to [0, 1.5]; if a beat is
        // late, the line continues a bit past its slot rather than
        // halting at the previous beat's position.
        let frac = min(elapsed / max(beatInterval, 0.05), 1.5)
        let absBeat = Double(beatIndex - 1) + frac
        let totalBeats = Double(layout.measures.count * beatsPerMeasure)
        guard totalBeats > 0 else { return }
        // Wrap the page so the head sweeps back to the top after
        // covering all printed measures. Modulo handles longer
        // play sessions without growing the page.
        let wrapped = absBeat.truncatingRemainder(dividingBy: totalBeats)
        let positiveWrapped = wrapped < 0 ? wrapped + totalBeats : wrapped
        // Linear-interpolate between two adjacent beat anchors.
        // Snapping per-quarter to Verovio's reported beat positions
        // means the playhead lines up exactly with where a played
        // note would land on the engraving — no off-by-clef-pad
        // drift that the layer-divided-by-4 math used to have.
        let lo = Int(floor(positiveWrapped))
        let hi = lo + 1
        let frac2 = positiveWrapped - Double(lo)
        guard let pLo = beatPosition(lo) else { return }
        let pHi = beatPosition(hi) ?? pLo
        headX = pLo.x + (pHi.x - pLo.x) * CGFloat(frac2)
        headY = pLo.staffY
        headHeight = pLo.staffHeight
    }

    private func ensureDisplayLink() {
        if displayLink != nil { return }
        var link: CVDisplayLink?
        CVDisplayLinkCreateWithActiveCGDisplays(&link)
        guard let link else { return }
        let context = Unmanaged.passUnretained(self).toOpaque()
        CVDisplayLinkSetOutputCallback(link, { (_, _, _, _, _, ctx) -> CVReturn in
            guard let ctx else { return kCVReturnSuccess }
            let me = Unmanaged<PlayheadOverlay>.fromOpaque(ctx).takeUnretainedValue()
            DispatchQueue.main.async { me.tick() }
            return kCVReturnSuccess
        }, context)
        CVDisplayLinkStart(link)
        displayLink = link
    }

    private func stopDisplayLink() {
        if let link = displayLink {
            CVDisplayLinkStop(link)
        }
        displayLink = nil
    }

    private func tick() {
        let now = CACurrentMediaTime()
        flash *= 0.90
        updateHead(at: now)
        needsDisplay = true
        // Idle the display link if more than ~3 beat intervals
        // have passed since the last metronome tick — user has
        // stopped the metronome and we don't need to keep
        // repainting an idle line.
        if lastBeatAt > 0,
           (now - lastBeatAt) > beatInterval * 3,
           flash < 0.02 {
            stopDisplayLink()
        }
    }
}
