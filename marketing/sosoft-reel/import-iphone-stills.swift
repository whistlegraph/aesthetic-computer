#!/usr/bin/env swift

import Foundation
import ImageCaptureCore

final class CameraImport: NSObject, ICDeviceBrowserDelegate, ICCameraDeviceDelegate, ICCameraDeviceDownloadDelegate {
    private let browser = ICDeviceBrowser()
    private let output: URL?
    private let wanted = try! NSRegularExpression(pattern: #"^IMG_(47(?:5[3-9]|6[0-9]|7[0-9]|8[0-9]|9[0-9])|48[0-9]{2}|49(?:[0-5][0-9]|6[0-6]))\.(HEIC|JPG)$"#, options: [.caseInsensitive])
    private var camera: ICCameraDevice?
    private var pending = 0
    private var finished = false

    init(output: URL?) {
        self.output = output
        super.init()
        browser.delegate = self
        browser.browsedDeviceTypeMask = ICDeviceTypeMask(rawValue: ICDeviceTypeMask.camera.rawValue | ICDeviceLocationTypeMask.local.rawValue)!
    }

    func start() {
        browser.start()
        DispatchQueue.main.asyncAfter(deadline: .now() + 90) { [weak self] in
            guard let self, !self.finished else { return }
            fputs("iphone import timed out\n", stderr)
            exit(2)
        }
    }

    func deviceBrowser(_ browser: ICDeviceBrowser, didAdd device: ICDevice, moreComing: Bool) {
        guard camera == nil, let camera = device as? ICCameraDevice else { return }
        self.camera = camera
        camera.delegate = self
        camera.mediaPresentation = .originalAssets
        camera.requestOpenSession()
    }

    func deviceBrowser(_ browser: ICDeviceBrowser, didRemove device: ICDevice, moreGoing: Bool) {}
    func device(_ device: ICDevice, didCloseSessionWithError error: (any Error)?) {}
    func didRemove(_ device: ICDevice) {}

    func device(_ device: ICDevice, didOpenSessionWithError error: (any Error)?) {
        if let error {
            fputs("could not open iPhone camera roll: \(error)\n", stderr)
            exit(2)
        }
    }

    func deviceDidBecomeReady(withCompleteContentCatalog camera: ICCameraDevice) {
        let files = (camera.mediaFiles ?? []).compactMap { $0 as? ICCameraFile }.filter { file in
            guard let name = file.name else { return false }
            return wanted.firstMatch(in: name, range: NSRange(name.startIndex..., in: name)) != nil
        }.sorted { ($0.name ?? "") < ($1.name ?? "") }

        print("found \(files.count) July 16 iPhone stills")
        guard let output else {
            for file in files {
                print("\(file.name ?? "?")\t\(file.fileSize)\t\(file.creationDate?.description ?? "")")
            }
            finish(camera)
            return
        }

        try? FileManager.default.createDirectory(at: output, withIntermediateDirectories: true)
        pending = files.count
        if pending == 0 {
            finish(camera)
            return
        }
        for file in files {
            camera.requestDownloadFile(
                file,
                options: [
                    .downloadsDirectoryURL: output,
                    .saveAsFilename: file.name ?? UUID().uuidString,
                    .overwrite: true,
                ],
                downloadDelegate: self,
                didDownloadSelector: #selector(didDownloadFile(_:error:options:contextInfo:)),
                contextInfo: nil
            )
        }
    }

    func cameraDevice(_ camera: ICCameraDevice, didAdd items: [ICCameraItem]) {}
    func cameraDevice(_ camera: ICCameraDevice, didRemove items: [ICCameraItem]) {}
    func cameraDevice(_ camera: ICCameraDevice, didRenameItems items: [ICCameraItem]) {}
    func cameraDevice(_ camera: ICCameraDevice, didReceiveThumbnail thumbnail: CGImage?, for item: ICCameraItem, error: (any Error)?) {}
    func cameraDevice(_ camera: ICCameraDevice, didReceiveMetadata metadata: [AnyHashable: Any]?, for item: ICCameraItem, error: (any Error)?) {}
    func cameraDeviceDidChangeCapability(_ camera: ICCameraDevice) {}
    func cameraDevice(_ camera: ICCameraDevice, didReceivePTPEvent eventData: Data) {}
    func cameraDeviceDidRemoveAccessRestriction(_ camera: ICDevice) {}
    func cameraDeviceDidEnableAccessRestriction(_ camera: ICDevice) {}

    @objc func didDownloadFile(_ file: ICCameraFile, error: (any Error)?, options: [String: Any], contextInfo: UnsafeMutableRawPointer?) {
        if let error { fputs("\(file.name ?? "?"): \(error)\n", stderr) }
        else { print("downloaded \(options[ICDownloadOption.savedFilename.rawValue] ?? file.name ?? "?")") }
        pending -= 1
        if pending == 0, let camera { finish(camera) }
    }

    private func finish(_ camera: ICCameraDevice) {
        guard !finished else { return }
        finished = true
        camera.requestCloseSession()
        browser.stop()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) { exit(0) }
    }
}

let args = CommandLine.arguments.dropFirst()
let output = args.first.map { URL(fileURLWithPath: $0, isDirectory: true) }
let importer = CameraImport(output: output)
importer.start()
RunLoop.main.run()
