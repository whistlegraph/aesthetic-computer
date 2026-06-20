// main.swift — NSApplication bootstrap + the input-capturing Metal view.
// First-person controls: WASD move, mouse look, Space jump, Shift crouch,
// click to capture the pointer, Esc to release.

import AppKit
import MetalKit

// MTKView subclass that captures keyboard + mouse for FPS control and forwards
// it into the shared Game state.
final class GameView: MTKView {
    let game: Game
    private var captured = false

    init(frame: CGRect, device: MTLDevice, game: Game) {
        self.game = game
        super.init(frame: frame, device: device)
    }

    @available(*, unavailable)
    required init(coder: NSCoder) { fatalError() }

    override var acceptsFirstResponder: Bool { true }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        window?.acceptsMouseMovedEvents = true
        window?.makeFirstResponder(self)
    }

    private func setCaptured(_ on: Bool) {
        guard captured != on else { return }
        captured = on
        if on {
            CGAssociateMouseAndMouseCursorPosition(0)
            NSCursor.hide()
        } else {
            CGAssociateMouseAndMouseCursorPosition(1)
            NSCursor.unhide()
        }
    }

    override func mouseDown(with event: NSEvent) { setCaptured(true) }

    override func mouseMoved(with event: NSEvent) {
        guard captured else { return }
        game.look(dx: Float(event.deltaX), dy: Float(event.deltaY))
    }
    override func mouseDragged(with event: NSEvent) { mouseMoved(with: event) }

    override func keyDown(with event: NSEvent) {
        if event.keyCode == 53 { setCaptured(false); return } // Esc
        setKey(event, down: true)
    }
    override func keyUp(with event: NSEvent) { setKey(event, down: false) }

    override func flagsChanged(with event: NSEvent) {
        game.crouch = event.modifierFlags.contains(.shift)
    }

    private func setKey(_ event: NSEvent, down: Bool) {
        if event.keyCode == 49 { game.jump = down; return } // Space
        switch event.charactersIgnoringModifiers?.lowercased() {
        case "w": game.forwardKey = down
        case "s": game.backKey = down
        case "a": game.leftKey = down
        case "d": game.rightKey = down
        default: break
        }
    }
}

final class AppDelegate: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    var renderer: Renderer!
    var game: Game!

    func applicationDidFinishLaunching(_ notification: Notification) {
        guard let device = MTLCreateSystemDefaultDevice() else {
            fatalError("Metal is not supported on this device")
        }
        game = Game()

        let frame = NSRect(x: 0, y: 0, width: 1280, height: 800)
        let view = GameView(frame: frame, device: device, game: game)
        view.preferredFramesPerSecond = 60
        view.isPaused = false
        view.enableSetNeedsDisplay = false

        guard let r = Renderer(mtkView: view, game: game) else {
            fatalError("Renderer init failed")
        }
        renderer = r
        view.delegate = r

        window = NSWindow(
            contentRect: frame,
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered, defer: false)
        window.title = "arena — native"
        window.contentView = view
        window.center()
        window.makeKeyAndOrderFront(nil)
        window.makeFirstResponder(view)

        NSApp.setActivationPolicy(.regular)
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ app: NSApplication) -> Bool { true }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.run()
