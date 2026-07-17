// GlowController.swift — display focus flash.
//
// ONE quick system-accent bloom around the display input just arrived on —
// nothing lingers, nothing breathes, nothing reddens. Arrivals come from
// deskflow-core's own log ("entering screen", printed the instant a switch
// happens — ground truth on every machine in the chain) plus same-machine
// display hops observed from mouse motion. "leaving screen" cuts any
// in-flight flash dead so the light never trails the cursor.
//
// Ported from the fuser desktop-badge so MacPal machines (neo, blueberry)
// flash on Deskflow crossover the same way the minis' badge does. It is
// self-contained: it installs its own mouse monitors and tails the deskflow
// log, so it needs no wiring into PalCore's event handling — just keep one
// instance alive.
//
// Touch <home>/noglow to disable on a machine. Override the corner radius by
// writing a number of points to <home>/glowradius.
// Perf: the bloom is a quarter-res true-gaussian image rendered once per
// screen onto a single CALayer; the flash animates only layer opacity.

import AppKit

final class GlowController {
    private let home: String
    private var win: NSWindow?
    private let accentL = CALayer()
    private var curScreen = NSRect.zero
    private var lastLoc = NSPoint(x: -99999, y: -99999)
    private var flashSeq = 0   // invalidates the orderOut of a superseded flash
    private var dfHandle: FileHandle?
    private var dfLogSource: DispatchSourceFileSystemObject?
    private let dfLogPath = NSString(string: "~/Library/Logs/deskflow-core.log").expandingTildeInPath

    init(home: String) {
        self.home = home
        startDeskflowWatch()
        startCursorWatch()
    }

    // Mouse-move monitors keep "which display is current" fresh and flash on
    // same-machine display hops. Global covers events delivered to other apps
    // (the usual case for an accessory app); local covers our own.
    private func startCursorWatch() {
        let mask: NSEvent.EventTypeMask = [.mouseMoved, .leftMouseDragged, .leftMouseDown,
                                           .rightMouseDown, .scrollWheel]
        NSEvent.addGlobalMonitorForEvents(matching: mask) { [weak self] e in
            self?.ping(e.type)
        }
        NSEvent.addLocalMonitorForEvents(matching: mask) { [weak self] e in
            self?.ping(e.type)
            return e
        }
    }

    // Mouse events only steer which display is current — and flash on hops.
    func ping(_ type: NSEvent.EventType) {
        let m = NSEvent.mouseLocation
        let jump = hypot(m.x - lastLoc.x, m.y - lastLoc.y)
        lastLoc = m
        // Single-event teleports are Deskflow warp plumbing, not the hand.
        if (type == .mouseMoved || type == .leftMouseDragged), jump >= 150 { return }
        guard let screen = NSScreen.screens.first(where: { NSMouseInRect(m, $0.frame, false) }) else { return }
        if curScreen == .zero { curScreen = screen.frame; return }
        if screen.frame != curScreen {
            curScreen = screen.frame
            flash()
        }
    }

    // ── deskflow log = ground truth ───────────────────────────────────────
    private func startDeskflowWatch() {
        guard FileManager.default.fileExists(atPath: dfLogPath) else { return }
        openDeskflowLog()
        // Reopen after rotation/truncation (kickstarts recreate the file).
        Timer.scheduledTimer(withTimeInterval: 2.0, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            let size = (try? FileManager.default.attributesOfItem(atPath: self.dfLogPath)[.size] as? UInt64) ?? nil
            let pos = (try? self.dfHandle?.offset()) ?? nil
            if self.dfHandle == nil || (size != nil && pos != nil && size! < pos!) {
                self.dfLogSource?.cancel(); self.dfLogSource = nil
                try? self.dfHandle?.close(); self.dfHandle = nil
                self.openDeskflowLog()
            }
        }
    }

    private func openDeskflowLog() {
        guard let h = FileHandle(forReadingAtPath: dfLogPath) else { return }
        _ = try? h.seekToEnd()
        dfHandle = h
        let src = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: h.fileDescriptor, eventMask: [.extend, .write], queue: .main)
        src.setEventHandler { [weak self] in self?.drainDeskflowLog() }
        src.resume()
        dfLogSource = src
    }

    private func drainDeskflowLog() {
        guard let h = dfHandle, let data = try? h.readToEnd(), !data.isEmpty,
              let s = String(data: data, encoding: .utf8) else { return }
        for line in s.split(separator: "\n") {
            if line.contains("entering screen") {
                // 10ms grace so the warped cursor position is current before
                // we pick which display to flash.
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.01) { [weak self] in
                    self?.flash()
                }
            } else if line.contains("leaving screen") {
                cut()
            }
        }
    }

    // The whole show: snap to full accent, hold a beat, gone. ~half a second.
    private func flash() {
        guard !FileManager.default.fileExists(atPath: home + "/noglow") else { return }
        let m = NSEvent.mouseLocation
        guard let screen = NSScreen.screens.first(where: { NSMouseInRect(m, $0.frame, false) })
            ?? NSScreen.main else { return }
        curScreen = screen.frame
        if win == nil { build(screen) }
        guard let win = win else { return }
        if win.frame != screen.frame || accentL.contents == nil {
            win.setFrame(screen.frame, display: false)
            retarget(screen)
        }
        win.orderFrontRegardless()
        accentL.removeAllAnimations()
        let a = CAKeyframeAnimation(keyPath: "opacity")
        a.values = [0, 1, 1, 0]
        a.keyTimes = [0, 0.08, 0.35, 1]
        a.timingFunctions = [CAMediaTimingFunction(name: .linear),
                             CAMediaTimingFunction(name: .linear),
                             CAMediaTimingFunction(name: .easeOut)]
        a.duration = 0.5
        setOpacity(accentL, 0)
        accentL.add(a, forKey: "flash")
        flashSeq += 1
        let seq = flashSeq
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.55) { [weak self] in
            guard let self = self, self.flashSeq == seq else { return }
            self.win?.orderOut(nil)
        }
    }

    /// Controller-role feedback is intentionally distinct from an ordinary
    /// cursor crossover: the same edge bloom plus one short local glass ding.
    /// Called only after the role fan-out has completed successfully.
    func controlAcquired() {
        flash()
        NSSound(named: NSSound.Name("Glass"))?.play()
    }

    // LEAVE mid-flash: kill the light instantly — it never trails the cursor.
    private func cut() {
        flashSeq += 1
        accentL.removeAllAnimations()
        setOpacity(accentL, 0)
        win?.orderOut(nil)
    }

    private func setOpacity(_ l: CALayer, _ v: Float) {
        CATransaction.begin(); CATransaction.setDisableActions(true)
        l.opacity = v
        CATransaction.commit()
    }

    // Re-render the accent bloom for a (new) screen. Quarter-res true
    // gaussian — the layer scales it up, and blur survives upscaling.
    private func retarget(_ screen: NSScreen) {
        guard let cv = win?.contentView, let host = cv.layer else { return }
        CATransaction.begin(); CATransaction.setDisableActions(true)
        if accentL.superlayer == nil { host.addSublayer(accentL); accentL.opacity = 0 }
        accentL.frame = cv.bounds
        accentL.contentsGravity = .resize
        accentL.contents = glowImage(cv.bounds.size, radius: displayRadius(screen), color: accent)
            .layerContents(forContentsScale: 1)
        CATransaction.commit()
    }

    private func glowImage(_ size: NSSize, radius: CGFloat, color: NSColor) -> NSImage {
        let s: CGFloat = 0.25
        let sz = NSSize(width: max(size.width * s, 64), height: max(size.height * s, 64))
        return NSImage(size: sz, flipped: false) { rect in
            // True gaussian, zero banding: stroke the ring far offscreen and
            // let only its blurred SHADOW land in the image.
            guard let ctx = NSGraphicsContext.current?.cgContext else { return true }
            let off = rect.height * 2
            func ring(blur: CGFloat, alpha: CGFloat, width: CGFloat) {
                ctx.saveGState()
                ctx.setShadow(offset: CGSize(width: 0, height: -off), blur: blur * s,
                              color: color.withAlphaComponent(alpha).cgColor)
                let rr = radius * s
                let p = NSBezierPath(roundedRect: rect.insetBy(dx: 1, dy: 1)
                                        .offsetBy(dx: 0, dy: off),
                                     xRadius: rr, yRadius: rr)
                p.lineWidth = width * s
                color.setStroke()
                p.stroke()
                ctx.restoreGState()
            }
            ring(blur: 38, alpha: 0.50, width: 20)   // wide ambient wash
            ring(blur: 14, alpha: 0.55, width: 6)    // gentle rim, still fully soft
            return true
        }
    }

    // Square external panels get near-square glow corners; notched MacBooks
    // get the big curve. Override per-machine by writing a number of points
    // to <home>/glowradius.
    private func displayRadius(_ screen: NSScreen) -> CGFloat {
        if let s = try? String(contentsOfFile: home + "/glowradius", encoding: .utf8),
           let v = Double(s.trimmingCharacters(in: .whitespacesAndNewlines)) {
            return CGFloat(v)
        }
        if #available(macOS 12.0, *), screen.safeAreaInsets.top > 0 { return 16 }
        return 6
    }

    private func build(_ screen: NSScreen) {
        let w = NSWindow(contentRect: screen.frame, styleMask: [.borderless],
                         backing: .buffered, defer: false)
        w.isOpaque = false; w.backgroundColor = .clear
        w.ignoresMouseEvents = true   // pure light — never catches a click
        w.hasShadow = false
        w.level = .screenSaver        // above the menubar, like the recording border
        w.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
        let v = NSView(frame: NSRect(origin: .zero, size: screen.frame.size))
        v.wantsLayer = true   // hosts the glow layer
        w.contentView = v
        win = w
    }
}
