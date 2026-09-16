import Foundation
import AppKit
import PDFKit

/// A local session marker may nominate one output, never a directory or remote URL.
/// WebKit reads a private staging copy, so granting it access cannot expose the
/// rest of a project, home directory, or manuscript's private source materials.
struct LocalArtifactPreview: Equatable {
    let path: String
    let mime: String
    let version: Int
    let artifactID: String
    let kind: String

    init?(marker: [String: Any], kind: String) {
        guard ["picture", "sound", "paper"].contains(kind),
              let path = marker["path"] as? String, path.hasPrefix("/"),
              let mime = marker["mime"] as? String,
              let version = marker["version"] as? Int, version > 0,
              let artifactID = marker["artifactId"] as? String, !artifactID.isEmpty else { return nil }
        let allowed: [String: [String]] = [
            "picture": ["image/png", "image/jpeg", "image/webp"],
            "sound": ["audio/wav", "audio/x-wav", "audio/mpeg"],
            "paper": ["application/pdf", "text/plain", "text/markdown", "text/x-tex", "application/x-tex"],
        ]
        guard allowed[kind]?.contains(mime) == true else { return nil }
        self.path = path; self.mime = mime; self.version = version
        self.artifactID = artifactID; self.kind = kind
    }

    var key: String { "\(kind):\(artifactID):\(version):\(path)" }

    func readValidatedFile() throws -> Data {
        let url = URL(fileURLWithPath: path)
        let info = try url.resourceValues(forKeys: [.isRegularFileKey, .isSymbolicLinkKey, .fileSizeKey])
        guard info.isRegularFile == true, info.isSymbolicLink != true,
              let size = info.fileSize, size > 0, size <= 64 * 1024 * 1024 else {
            throw NSError(domain: "EaselPreview", code: 1,
                          userInfo: [NSLocalizedDescriptionKey: "Preview must be a regular file under 64 MB."])
        }
        return try Data(contentsOf: url, options: .mappedIfSafe)
    }

    /// Pixel dimensions for pictures; PDF points for the first page. UI resizing
    /// scales this surface and never changes the artifact's logical viewport.
    func dimensions(_ data: Data) -> CGSize {
        if kind == "picture", let rep = NSBitmapImageRep(data: data), rep.pixelsWide > 0, rep.pixelsHigh > 0 {
            return CGSize(width: rep.pixelsWide, height: rep.pixelsHigh)
        }
        if mime == "application/pdf", let page = PDFDocument(data: data)?.page(at: 0) {
            let size = page.bounds(for: .cropBox).size
            if size.width > 0 && size.height > 0 { return size }
        }
        return CGSize(width: 768, height: 512)
    }

    static func escaped(_ text: String) -> String {
        text.replacingOccurrences(of: "&", with: "&amp;")
            .replacingOccurrences(of: "<", with: "&lt;")
            .replacingOccurrences(of: ">", with: "&gt;")
            .replacingOccurrences(of: "\"", with: "&quot;")
            .replacingOccurrences(of: "'", with: "&#39;")
    }

    /// No project content is executable. Only the fixed readiness script runs.
    func html(text: String? = nil, waveform: String? = nil, nonce: String) -> String {
        let content: String
        let ready: String
        if kind == "picture" {
            content = "<img id='artifact' src='artifact' alt='Picture preview'>"
            ready = "const a=document.getElementById('artifact'); a.onload=ready; a.onerror=failed; if(a.complete&&a.naturalWidth)ready();"
        } else if kind == "sound" {
            content = (waveform ?? "") + "<audio id='artifact' controls preload='metadata' src='artifact'></audio>"
            ready = "const a=document.getElementById('artifact'); a.onloadedmetadata=ready; a.onerror=failed; if(a.readyState>=1)ready();"
        } else {
            content = "<pre>\(Self.escaped(text ?? ""))</pre>"
            ready = "requestAnimationFrame(ready);"
        }
        return """
        <!doctype html><html><head><meta charset="utf-8">
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src file:; media-src file:; style-src 'unsafe-inline'; script-src 'nonce-\(nonce)'; base-uri 'none'; form-action 'none'">
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <style>html,body{margin:0;width:100%;height:100%;background:#15151a;color:#eee}body{display:flex;flex-direction:column;align-items:center;justify-content:center;gap:12px}img{width:100%;height:100%;object-fit:contain}svg{width:95%;height:65%}audio{width:90%}pre{align-self:stretch;white-space:pre-wrap;overflow:auto;padding:24px;font:16px/1.5 monospace;width:100%;margin:0;box-sizing:border-box}</style></head><body>
        \(content)<script nonce="\(nonce)">const ready=()=>window.webkit.messageHandlers.previewReady.postMessage('ready:\(nonce)');const failed=()=>window.webkit.messageHandlers.previewReady.postMessage('failed:\(nonce)');\(ready)</script></body></html>
        """
    }

    /// Static waveform of the actual Easel PCM WAV; unsupported formats simply
    /// retain their native audio controls. Never draw invented progress.
    static func waveform(_ data: Data) -> String? {
        guard data.count >= 44,
              String(data: data.prefix(4), encoding: .ascii) == "RIFF",
              String(data: data[8..<16], encoding: .ascii) == "WAVEfmt ",
              String(data: data[36..<40], encoding: .ascii) == "data" else { return nil }
        func word(_ offset: Int) -> Int { Int(data[offset]) | (Int(data[offset + 1]) << 8) }
        guard word(20) == 1, word(22) == 1, word(34) == 16 else { return nil }
        let count = (data.count - 44) / 2
        guard count > 0 else { return nil }
        var commands: [String] = []
        for x in 0..<256 {
            let start = x * count / 256, end = min(count, max(start + 1, (x + 1) * count / 256))
            var peak = 0
            for sample in start..<end {
                let raw = word(44 + sample * 2)
                peak = max(peak, abs(raw >= 32768 ? raw - 65536 : raw))
            }
            let height = Double(peak) / 32768 * 47
            commands.append("M\(x) \(50 - height)L\(x) \(50 + height)")
        }
        return "<svg viewBox='0 0 256 100' preserveAspectRatio='xMidYMid meet' role='img' aria-label='Audio waveform'><path d='\(commands.joined(separator: " "))' stroke='#f27cad' stroke-width='1' fill='none'/></svg>"
    }
}
