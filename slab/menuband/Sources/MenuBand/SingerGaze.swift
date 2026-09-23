import AVFoundation
import Vision

/// The room, seen from the lid camera, so the singing face can look at the
/// people in it. Runs only while a full-screen face is up (SingerFace.show
/// starts it, hide stops it). Publishes a normalized gaze target on the main
/// thread — x −1…1 as the viewer sees the screen (left→right), y −0.5…0.5 —
/// and how many people it sees. Frames are analyzed and dropped; nothing is
/// stored. Payload `gaze=0` keeps the camera off.
final class SingerGaze: NSObject, AVCaptureVideoDataOutputSampleBufferDelegate {
    static let shared = SingerGaze()

    private(set) var target: CGPoint?          // main thread
    private(set) var people = 0                // main thread
    private(set) var lastSeen = Date.distantPast

    private let session = AVCaptureSession()
    private let queue = DispatchQueue(label: "computer.aestheticcomputer.menuband.gaze")
    private let sequence = VNSequenceRequestHandler()
    private var configured = false
    private var frames = 0
    private var lastLog = Date.distantPast
    private var wanted = false

    func start() {
        wanted = true
        switch AVCaptureDevice.authorizationStatus(for: .video) {
        case .authorized:
            queue.async { self.run() }
        case .notDetermined:
            AVCaptureDevice.requestAccess(for: .video) { [weak self] ok in
                guard let self else { return }
                if ok, self.wanted { self.queue.async { self.run() } } else { NSLog("👁 gaze: camera denied") }
            }
        default:
            NSLog("👁 gaze: camera not authorized (System Settings › Privacy › Camera › Menu Band)")
        }
    }

    func stop() {
        wanted = false
        queue.async {
            if self.session.isRunning { self.session.stopRunning(); NSLog("👁 gaze: camera off") }
        }
        target = nil; people = 0
    }

    private func run() {
        if !configured {
            guard let device = AVCaptureDevice.default(.builtInWideAngleCamera, for: .video, position: .unspecified)
                    ?? AVCaptureDevice.default(for: .video),
                  let input = try? AVCaptureDeviceInput(device: device) else { NSLog("👁 gaze: no camera"); return }
            session.beginConfiguration()
            session.sessionPreset = .vga640x480
            if session.canAddInput(input) { session.addInput(input) }
            let out = AVCaptureVideoDataOutput()
            out.alwaysDiscardsLateVideoFrames = true
            out.videoSettings = [kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA]
            out.setSampleBufferDelegate(self, queue: queue)
            if session.canAddOutput(out) { session.addOutput(out) }
            session.commitConfiguration()
            configured = true
        }
        if wanted && !session.isRunning { session.startRunning(); NSLog("👁 gaze: camera on") }
    }

    func captureOutput(_ output: AVCaptureOutput, didOutput sampleBuffer: CMSampleBuffer, from connection: AVCaptureConnection) {
        frames += 1
        if frames % 3 != 0 { return }                       // ~10 looks a second is plenty for eyes
        guard let pixels = CMSampleBufferGetImageBuffer(sampleBuffer) else { return }
        let faces = VNDetectFaceRectanglesRequest()
        let bodies = VNDetectHumanRectanglesRequest()
        if #available(macOS 12.0, *) { bodies.upperBodyOnly = true }
        try? sequence.perform([faces, bodies], on: pixels, orientation: .up)
        // Prefer faces; from a body, aim at where its head is.
        var heads: [CGRect] = (faces.results ?? []).map { $0.boundingBox }
        if heads.isEmpty {
            heads = (bodies.results ?? []).map { b in
                let r = b.boundingBox
                return CGRect(x: r.midX - r.width * 0.15, y: r.maxY - r.height * 0.3, width: r.width * 0.3, height: r.height * 0.3)
            }
        }
        let count = heads.count
        let nearest = heads.max { $0.width * $0.height < $1.width * $1.height }
        DispatchQueue.main.async { [self] in
            people = count
            if let r = nearest {
                // The camera image is not mirrored: what is on the viewer's
                // right lands on the image's left. Vision's origin is bottom-left.
                let x = 1 - 2 * r.midX, y = (r.midY - 0.5) * 1.2
                target = CGPoint(x: max(-1, min(1, x)), y: max(-0.5, min(0.5, y)))
                lastSeen = Date()
            } else if Date().timeIntervalSince(lastSeen) > 1.5 {
                target = nil
            }
            if Date().timeIntervalSince(lastLog) > 5 {
                lastLog = Date()
                if let t = target { NSLog("👁 gaze: %d in the room · looking %.2f, %.2f", count, t.x, t.y) }
                else { NSLog("👁 gaze: nobody in the room") }
            }
        }
    }
}
