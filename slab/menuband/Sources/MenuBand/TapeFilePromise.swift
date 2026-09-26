import AppKit

/// Start dragging immediately; fulfill the file only after Finder accepts
/// the drop. The provider's delegate must outlive the dragging session.
final class TapeFilePromise: NSObject, NSFilePromiseProviderDelegate {
    private let name: String
    private let prepare: (@escaping (URL?) -> Void) -> Void
    private static let writingQueue: OperationQueue = {
        let queue = OperationQueue()
        queue.name = "menuband.tape-file-promises"
        queue.qualityOfService = .utility
        queue.maxConcurrentOperationCount = 1
        return queue
    }()

    init(name: String, prepare: @escaping (@escaping (URL?) -> Void) -> Void) {
        self.name = name
        self.prepare = prepare
    }

    func provider(fileType: String) -> NSFilePromiseProvider {
        let provider = NSFilePromiseProvider(fileType: fileType, delegate: self)
        // NSFilePromiseProvider's delegate is weak. Its userInfo travels with
        // the promise, keeping this take alive even after another drag starts.
        provider.userInfo = self
        return provider
    }

    func filePromiseProvider(_ filePromiseProvider: NSFilePromiseProvider,
                             fileNameForType fileType: String) -> String { name }

    func operationQueue(for filePromiseProvider: NSFilePromiseProvider) -> OperationQueue {
        Self.writingQueue
    }

    func filePromiseProvider(_ filePromiseProvider: NSFilePromiseProvider,
                             writePromiseTo url: URL,
                             completionHandler: @escaping (Error?) -> Void) {
        DispatchQueue.main.async {
            self.prepare { source in
                Self.writingQueue.addOperation {
                    guard let source else {
                        completionHandler(NSError(domain: "MenuBandTape", code: 1,
                            userInfo: [NSLocalizedDescriptionKey: "The recording could not be exported."]))
                        return
                    }
                    var coordinationError: NSError?
                    var copyError: Error?
                    NSFileCoordinator().coordinate(writingItemAt: url, options: [], error: &coordinationError) { destination in
                        do { try FileManager.default.copyItem(at: source, to: destination) }
                        catch { copyError = error }
                    }
                    completionHandler(copyError ?? coordinationError)
                }
            }
        }
    }
}
