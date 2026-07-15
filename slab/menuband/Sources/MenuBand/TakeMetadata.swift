import Foundation

/// Track metadata for a finished take: descriptive tags embedded in the WAV
/// (RIFF `LIST`/`INFO`, which most DAWs read and Spotlight indexes as
/// kMDItemTitle/Authors), plus a macOS Finder comment (kMDItemFinderComment).
/// Both ride along inside the DMG — xattrs and RIFF chunks survive the copy +
/// `hdiutil` imaging.
enum TakeMetadata {
    /// Append a `LIST`/`INFO` chunk (INAM/IART/ICRD/ISFT/ICMT) to a finished
    /// WAV and fix up the RIFF size. The `AVAudioFile` MUST already be closed —
    /// a still-open writer rewrites the header on deinit and clobbers this.
    /// Call BEFORE stamping the custom icon: this rewrites the file bytes, which
    /// would otherwise strip a resource-fork icon.
    static func writeWavInfo(url: URL, title: String, artist: String,
                             dateISO: String, software: String, comment: String) {
        guard var data = try? Data(contentsOf: url), data.count >= 12,
              data.prefix(4) == Data("RIFF".utf8),
              data[8..<12] == Data("WAVE".utf8)
        else { return }

        // One INFO sub-chunk: 4-char id + LE uint32 size + NUL-terminated text,
        // padded to an even byte count (pad not counted in the size field).
        func sub(_ id: String, _ value: String) -> Data {
            var payload = Array(value.utf8); payload.append(0)
            var d = Data(id.utf8)
            d.append(le32(UInt32(payload.count)))
            d.append(contentsOf: payload)
            if d.count % 2 != 0 { d.append(0) }
            return d
        }
        var info = Data("INFO".utf8)
        info.append(sub("INAM", title))
        info.append(sub("IART", artist))
        info.append(sub("ICRD", dateISO))
        info.append(sub("ISFT", software))
        info.append(sub("ICMT", comment))

        var list = Data("LIST".utf8)
        list.append(le32(UInt32(info.count)))
        list.append(info)

        data.append(list)
        // RIFF size = total file length − 8 (the "RIFF" id + this size field).
        data.replaceSubrange(4..<8, with: le32(UInt32(data.count - 8)))
        try? data.write(to: url)   // non-atomic: same inode, no xattrs to lose yet
    }

    /// Set the macOS Finder comment (Get Info · Comments; Spotlight
    /// kMDItemFinderComment). Stored as a binary-plist xattr, the same shape
    /// Finder writes.
    static func setFinderComment(url: URL, _ comment: String) {
        guard let plist = try? PropertyListSerialization.data(
            fromPropertyList: comment as NSString, format: .binary, options: 0)
        else { return }
        url.withUnsafeFileSystemRepresentation { path in
            guard let path = path else { return }
            _ = plist.withUnsafeBytes { buf in
                setxattr(path, "com.apple.metadata:kMDItemFinderComment",
                         buf.baseAddress, buf.count, 0, 0)
            }
        }
    }

    private static func le32(_ v: UInt32) -> Data {
        Data([UInt8(v & 0xFF), UInt8((v >> 8) & 0xFF),
              UInt8((v >> 16) & 0xFF), UInt8((v >> 24) & 0xFF)])
    }
}
