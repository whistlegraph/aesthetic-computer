import SwiftUI
import UIKit

/// TrackDrum for the phone. The Mac edition (slab/tracktramp) turns a trackpad
/// into Menu Band's percussion surface; this one turns the screen into the same
/// instrument, standing the wide drum on its end so a portrait phone is all
/// drum instead of a letterboxed strip.
@main
struct TrackDrumApp: App {
    var body: some Scene {
        WindowGroup { TrackDrumScreen().ignoresSafeArea() }
    }
}

struct TrackDrumScreen: UIViewControllerRepresentable {
    func makeUIViewController(context: Context) -> TrackDrumController {
        TrackDrumController()
    }

    func updateUIViewController(_ controller: TrackDrumController, context: Context) {}
}

final class TrackDrumController: UIViewController {
    private let surface = TrackDrumSurfaceView(frame: .zero)
    private let session = TrackDrumAudioSession()
    // The engine pans on the drum's own long axis, which the turn stands
    // upright. Point it across the glass instead so left is left in the ears.
    private let performer = TrackDrumPerformer(
        panAxis: TrackDrumFit.panAxis(for: TrackDrumFit.turn)
    )

    // One surface, no chrome: no status bar, no home indicator, and system
    // edge gestures deferred so a stroke toward an edge plays the rim instead
    // of pulling down Control Center.
    override var prefersStatusBarHidden: Bool { true }
    override var prefersHomeIndicatorAutoHidden: Bool { true }
    override var preferredScreenEdgesDeferringSystemGestures: UIRectEdge { .all }

    override func loadView() { view = surface }

    override func viewDidLoad() {
        super.viewDidLoad()
        surface.onContacts = { [weak performer] contacts, timestamp, callbackTime in
            performer?.receive(contacts: contacts, timestamp: timestamp,
                               callbackTime: callbackTime)
        }
        // Both ends are on the main thread now, so a frame goes straight to the
        // view — no hop, and no chance of drawing a membrane older than the one
        // the audio just heard.
        performer.onActivity = { [weak surface] touches, charges, membrane in
            surface?.update(touches: touches, charges: charges, membrane: membrane)
        }
        session.onLost = { [weak self] in self?.performer.stop() }
        session.onReturned = { [weak self] in self?.startInstrument() }
        NotificationCenter.default.addObserver(
            self, selector: #selector(resignedActive),
            name: UIApplication.willResignActiveNotification, object: nil)
    }

    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        // An instrument you are staring at while thinking is not an idle phone.
        UIApplication.shared.isIdleTimerDisabled = true
        setNeedsUpdateOfHomeIndicatorAutoHidden()
        setNeedsUpdateOfScreenEdgesDeferringSystemGestures()
        startInstrument()
    }

    override func viewWillDisappear(_ animated: Bool) {
        UIApplication.shared.isIdleTimerDisabled = false
        performer.stop()
        super.viewWillDisappear(animated)
    }

    /// A finger held when the app leaves the foreground never gets its lift, and
    /// would hang a scratch on forever.
    @objc private func resignedActive() {
        performer.releaseAllContacts()
    }

    private func startInstrument() {
        do {
            try session.activate()
            try performer.start()
            // What the OS actually granted, not what we asked for. This is the
            // only honest way to know the drum's latency.
            NSLog("%@", session.report)
            FocusCueBeep.shared.play(rising: true)
        } catch {
            // The drum is the whole app; there is nothing to fall back to and
            // nothing useful to say in a sheet. Say it where a log reader can
            // find it at 2am.
            NSLog("🥁 TrackDrum audio unavailable: %@", String(describing: error))
        }
    }
}
