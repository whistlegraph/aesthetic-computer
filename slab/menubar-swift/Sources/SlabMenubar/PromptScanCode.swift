// PROMPT SCAN CODES.
//
// An Aesthetic Code session is holding a live piece, and the way you reach that
// piece from a phone is a QR. Printed into the terminal it costs seventeen rows
// of half-blocks and shoves the actual conversation off the top of the pane;
// carried by the session's prompt rock it costs nothing but the stone's own
// square, and it sits where the eye already looks for that session.
//
// Nothing here is stylised, and that is the point. A QR stops being a QR the
// moment it is lit, tinted, softened, or tumbled: it is solid black on solid
// white, it keeps a real quiet zone, and every module is a whole number of
// device pixels so no edge is ever resampled into grey. The rock's lighting
// model, its translucency and its rotation are all deliberately withheld from
// this surface — the status channel moves around the code, never through it.

import AppKit
import CoreImage

enum PromptScanCode {
    /// Modules of white kept around the code. Four is the spec's minimum for a
    /// reliable read. CIQRCodeGenerator already draws a one-module border of
    /// its own, which lands *inside* this margin rather than counting as it.
    private static let quietModules = 4

    /// The smallest module we are willing to draw, in device pixels. Below
    /// this the code is a texture rather than something a phone camera can
    /// resolve across a desk, and the caller is better off with the stone.
    private static let minimumModulePixels = 3

    /// Points-to-pixels for the rendered bitmap. Fixed at 2 rather than read
    /// from the current screen: a Retina rock gets exact 1:1 pixels, and a
    /// 1x display halves an even module size cleanly instead of landing the
    /// modules on fractional pixel boundaries when a window changes screens.
    static let renderScale: CGFloat = 2

    private static let ciContext = CIContext(options: [.useSoftwareRenderer: true])

    /// The marker carries a bare host+path — `aesthetic.computer/prompt~…` —
    /// because that is what reads well in a terminal. A phone camera wants a
    /// URL it can open, and a schemeless payload is only text to it, so the
    /// scheme is added here rather than asked of the session that minted it.
    static func scannablePayload(_ url: String) -> String {
        let trimmed = url.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return "" }
        return trimmed.contains("://") ? trimmed : "https://" + trimmed
    }

    /// Render `url` as a black-on-white code sized to sit inside a
    /// `surfacePoints` square. Returns nil when there is nothing to encode,
    /// when the payload is too long for a QR at all, or when the code would
    /// only fit at a module size no camera could resolve — in every one of
    /// those cases the caller falls back to the ordinary sigil rock.
    static func render(url: String, surfacePoints: CGFloat) -> CGImage? {
        let payload = scannablePayload(url)
        guard !payload.isEmpty, surfacePoints > 0,
              let filter = CIFilter(name: "CIQRCodeGenerator") else { return nil }
        filter.setValue(Data(payload.utf8), forKey: "inputMessage")
        // "M" recovers ~15% of the symbol. On a screen there is no print
        // damage to recover from, so anything higher only buys modules — and
        // more modules at a fixed surface means smaller ones, which is the
        // opposite of what makes this scan.
        filter.setValue("M", forKey: "inputCorrectionLevel")
        guard let output = filter.outputImage else { return nil }

        let modules = Int(output.extent.width.rounded())
        guard modules > 0,
              let code = ciContext.createCGImage(output, from: output.extent)
        else { return nil }

        // One module is an exact integer square of device pixels, so the
        // nearest-neighbour blit below can only ever produce hard edges.
        let total = modules + quietModules * 2
        let modulePixels = Int(surfacePoints * renderScale) / total
        guard modulePixels >= minimumModulePixels else { return nil }

        let side = total * modulePixels
        guard let ctx = CGContext(
            data: nil, width: side, height: side, bitsPerComponent: 8,
            bytesPerRow: side * 4, space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.noneSkipLast.rawValue)
        else { return nil }
        // The quiet zone is part of the bitmap, not a background colour the
        // compositor is trusted to paint: whatever the rock is sitting on
        // top of, the code arrives with its own white margin already drawn.
        ctx.setFillColor(CGColor(gray: 1, alpha: 1))
        ctx.fill(CGRect(x: 0, y: 0, width: side, height: side))
        ctx.interpolationQuality = .none
        ctx.setShouldAntialias(false)
        let inset = CGFloat(quietModules * modulePixels)
        let span = CGFloat(modules * modulePixels)
        ctx.draw(code, in: CGRect(x: inset, y: inset, width: span, height: span))
        return ctx.makeImage()
    }
}
