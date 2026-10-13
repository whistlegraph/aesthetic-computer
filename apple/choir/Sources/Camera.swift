import SwiftUI
import AVFoundation
import CoreImage

/// A camera that shows the chosen filter live, for snapping a picture into the
/// chat. The photo arrives filtered, with its original kept for re-filtering.
@MainActor final class CameraModel: NSObject, ObservableObject {
    @Published var frame: CGImage?
    @Published var filter: PhotoFilter = .daisyIris   // the camera's one filter (Fía)
    @Published var denied = false
    @Published var front = false
    private let session = AVCaptureSession()
    private let video = AVCaptureVideoDataOutput()
    private let photo = AVCapturePhotoOutput()
    private let queue = DispatchQueue(label: "choir.camera")
    nonisolated(unsafe) private var liveFilter: PhotoFilter = .daisyIris
    nonisolated static let previewContext = CIContext(options: [.useSoftwareRenderer: false]) // one, shared across frames
    private var onPhoto: ((UIImage) -> Void)?

    func start() async {
        switch AVCaptureDevice.authorizationStatus(for: .video) {
        case .notDetermined: if !(await AVCaptureDevice.requestAccess(for: .video)) { denied = true; return }
        case .denied, .restricted: denied = true; return
        default: break
        }
        configure()
        let s = session
        queue.async { s.startRunning() }
    }
    func stop() { let s = session; queue.async { s.stopRunning() } }

    func setFilter(_ f: PhotoFilter) { filter = f; liveFilter = f }

    private func configure() {
        session.beginConfiguration()
        session.sessionPreset = .photo
        session.inputs.forEach { session.removeInput($0) }
        if let device = AVCaptureDevice.default(.builtInWideAngleCamera, for: .video, position: front ? .front : .back),
           let input = try? AVCaptureDeviceInput(device: device), session.canAddInput(input) { session.addInput(input) }
        if !session.outputs.contains(video), session.canAddOutput(video) {
            video.setSampleBufferDelegate(self, queue: queue)
            video.alwaysDiscardsLateVideoFrames = true
            session.addOutput(video)
        }
        if !session.outputs.contains(photo), session.canAddOutput(photo) { session.addOutput(photo) }
        video.connection(with: .video)?.videoRotationAngle = 90
        video.connection(with: .video)?.isVideoMirrored = front
        session.commitConfiguration()
    }

    func flip() { front.toggle(); configure() }

    func snap(_ done: @escaping (UIImage) -> Void) {
        onPhoto = done
        let settings = AVCapturePhotoSettings()
        photo.connection(with: .video)?.videoRotationAngle = 90
        photo.capturePhoto(with: settings, delegate: self)
    }
}

extension CameraModel: AVCaptureVideoDataOutputSampleBufferDelegate, AVCapturePhotoCaptureDelegate {
    nonisolated func captureOutput(_ output: AVCaptureOutput, didOutput sampleBuffer: CMSampleBuffer, from connection: AVCaptureConnection) {
        guard let buffer = CMSampleBufferGetImageBuffer(sampleBuffer) else { return }
        var image = CIImage(cvPixelBuffer: buffer)
        let scale = 720 / max(image.extent.width, image.extent.height) // a light preview
        image = image.transformed(by: CGAffineTransform(scaleX: scale, y: scale))
        let filtered = (liveFilter == .daisyIris ? PhotoFilter.daisy(image, blurredFrame: true, inked: true) : liveFilter.filtered(image)) ?? image
        guard let cg = Self.previewContext.createCGImage(filtered, from: filtered.extent) else { return }
        Task { @MainActor in self.frame = cg }
    }

    nonisolated func photoOutput(_ output: AVCapturePhotoOutput, didFinishProcessingPhoto photo: AVCapturePhoto, error: Error?) {
        guard let data = photo.fileDataRepresentation(), let image = UIImage(data: data) else { return }
        Task { @MainActor in self.onPhoto?(image); self.onPhoto = nil }
    }
}

struct CameraView: View {
    let onTake: (UIImage, PhotoFilter) -> Void
    @Environment(\.dismiss) private var dismiss
    @StateObject private var camera = CameraModel()
    @State private var flash = false

    var body: some View {
        ZStack {
            Color(red: 0.965, green: 0.945, blue: 0.894).ignoresSafeArea()
            if camera.denied {
                VStack(spacing: 12) {
                    Text("choir needs the camera to take a picture.").foregroundStyle(Theme.ink)
                    Button("Open Settings") { if let u = URL(string: UIApplication.openSettingsURLString) { UIApplication.shared.open(u) } }
                        .buttonStyle(GlassChipStyle(on: true))
                }
            } else if let frame = camera.frame {
                Image(decorative: frame, scale: 1).resizable().scaledToFit().ignoresSafeArea()
            }
            if flash { Color.white.ignoresSafeArea().transition(.opacity) }
            VStack {
                HStack {
                    Button { camera.stop(); dismiss() } label: { Hand("HandX", height: 16) }.buttonStyle(GlassChipStyle())
                        .accessibilityLabel("Close")
                    Spacer()
                    Hand("FlowerO", height: 22).foregroundStyle(.white)
                    Spacer()
                    Button { camera.flip() } label: { Image(systemName: "arrow.triangle.2.circlepath.camera") }.buttonStyle(GlassChipStyle())
                }
                .padding()
                Spacer()
                Button {
                    withAnimation(.easeOut(duration: 0.12)) { flash = true }
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { withAnimation { flash = false } }
                    let f = camera.filter
                    camera.snap { image in
                        Trail.send("camera:snap", ["filter": f.rawValue, "daisy": PhotoFilter.daisyIndex])
                        camera.stop(); onTake(image, f); PhotoFilter.nextDaisy(); dismiss()
                    }
                } label: {
                    Circle().fill(.white).frame(width: 70, height: 70)
                        .overlay(Circle().strokeBorder(.black.opacity(0.2), lineWidth: 3).padding(5))
                }
                .accessibilityLabel("Take picture")
                .padding(.vertical, 18)
            }
        }
        .task { await camera.start() }
        .onDisappear { camera.stop() }
    }
}
