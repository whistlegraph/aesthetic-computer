import SwiftUI
import ImageIO
import UIKit

/// Pictures from the network, kept: fetched once, decoded small, and never
/// given up on. AsyncImage dropped a photo for good when its load was
/// cancelled mid-scroll, which read as "images missing from chat" (Fía,
/// 10-13). This retries on every appearance and remembers what it has.
enum ImageStore {
    static let memory: NSCache<NSString, UIImage> = {
        let c = NSCache<NSString, UIImage>(); c.totalCostLimit = 120 * 1024 * 1024; return c
    }()
    static let session: URLSession = {
        let config = URLSessionConfiguration.default
        config.urlCache = URLCache(memoryCapacity: 32 * 1024 * 1024, diskCapacity: 256 * 1024 * 1024, diskPath: "choir-images")
        config.requestCachePolicy = .returnCacheDataElseLoad
        return URLSession(configuration: config)
    }()

    /// The picture at `url`, at most `maxPixels` on its long side. Three tries
    /// with a growing pause; a cancelled load is not a failure.
    static func image(_ url: URL, maxPixels: CGFloat) async throws -> UIImage {
        let key = "\(url.absoluteString)@\(Int(maxPixels))" as NSString
        if let hit = memory.object(forKey: key) { return hit }
        var pause: Double = 0.6
        for attempt in 0..<3 {
            do {
                let (data, response) = try await session.data(from: url)
                if let http = response as? HTTPURLResponse, !(200..<300).contains(http.statusCode) { throw URLError(.badServerResponse) }
                guard let image = downsample(data, maxPixels: maxPixels) else { throw URLError(.cannotDecodeContentData) }
                memory.setObject(image, forKey: key, cost: Int(image.size.width * image.size.height * 4))
                return image
            } catch is CancellationError { throw CancellationError() }
            catch let e as URLError where e.code == .cancelled { throw CancellationError() }
            catch {
                if attempt == 2 { throw error }
                try await Task.sleep(for: .seconds(pause)); pause *= 2
            }
        }
        throw URLError(.unknown)
    }

    /// Decode straight to a thumbnail: a 1536 px PNG never becomes a 9 MB bitmap.
    static func downsample(_ data: Data, maxPixels: CGFloat) -> UIImage? {
        guard let source = CGImageSourceCreateWithData(data as CFData, [kCGImageSourceShouldCache: false] as CFDictionary) else { return nil }
        let options: [CFString: Any] = [
            kCGImageSourceCreateThumbnailFromImageAlways: true,
            kCGImageSourceShouldCacheImmediately: true,
            kCGImageSourceCreateThumbnailWithTransform: true,
            kCGImageSourceThumbnailMaxPixelSize: Int(maxPixels),
        ]
        guard let cg = CGImageSourceCreateThumbnailAtIndex(source, 0, options as CFDictionary) else { return nil }
        return UIImage(cgImage: cg)
    }
}

/// A picture that loads itself, in place of AsyncImage. The placeholder is
/// told whether the last try failed, so it can stop spinning.
struct RemoteImage<Content: View, Placeholder: View>: View {
    let url: URL?
    var maxPixels: CGFloat = 1200
    @ViewBuilder let content: (Image) -> Content
    @ViewBuilder let placeholder: (Bool) -> Placeholder
    @State private var image: UIImage?
    @State private var failed = false

    var body: some View {
        Group {
            if let image { content(Image(uiImage: image)) } else { placeholder(failed) }
        }
        .task(id: url) {
            guard let url, image == nil else { return }
            failed = false
            do {
                let loaded = try await ImageStore.image(url, maxPixels: maxPixels)
                withAnimation(.easeOut(duration: 0.2)) { image = loaded }
            } catch is CancellationError {
            } catch { failed = true }
        }
    }
}
