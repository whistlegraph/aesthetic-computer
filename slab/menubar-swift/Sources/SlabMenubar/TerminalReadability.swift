import AppKit

/// App-supplied RGB/256-color ink bypasses Terminal's sixteen ANSI swatches.
/// Its Display ANSI Colors switch is the only profile control that also
/// handles those colors and explicit backgrounds. Keep status color in the
/// page and bold ink; retain underline, inverse, and font emphasis.
enum TerminalReadability {
    typealias RGB = (Int, Int, Int)
    static let profileName = "Slab-Readable-v1"

    static func luminance(_ rgb: RGB) -> Double {
        // AppleScript colors use the device space, not sRGB.
        let c = NSColor(deviceRed: CGFloat(rgb.0) / 65535,
                        green: CGFloat(rgb.1) / 65535,
                        blue: CGFloat(rgb.2) / 65535, alpha: 1)
            .usingColorSpace(.sRGB)!
        func linear(_ v: CGFloat) -> Double {
            let x = Double(v)
            return x <= 0.04045 ? x / 12.92 : pow((x + 0.055) / 1.055, 2.4)
        }
        return 0.2126 * linear(c.redComponent) + 0.7152 * linear(c.greenComponent)
            + 0.0722 * linear(c.blueComponent)
    }

    static func contrast(_ a: RGB, _ b: RGB) -> Double {
        let x = luminance(a), y = luminance(b)
        return (max(x, y) + 0.05) / (min(x, y) + 0.05)
    }

    /// Preserve the hue, moving only as far toward black/white as necessary.
    /// A midtone page may not permit 7:1; use its maximum achievable contrast.
    static func ink(_ color: RGB, on background: RGB, minimum: Double = 7) -> RGB {
        let black: RGB = (0, 0, 0), white: RGB = (65535, 65535, 65535)
        let pole = contrast(black, background) >= contrast(white, background) ? black : white
        let target = min(minimum, contrast(pole, background))
        if contrast(color, background) >= target { return color }
        func blend(_ t: Double) -> RGB {
            func v(_ a: Int, _ b: Int) -> Int { Int((Double(a) + Double(b - a) * t).rounded()) }
            return (v(color.0, pole.0), v(color.1, pole.1), v(color.2, pole.2))
        }
        var low = 0.0, high = 1.0
        for _ in 0..<20 {
            let mid = (low + high) / 2
            if contrast(blend(mid), background) >= target { high = mid } else { low = mid }
        }
        return blend(high)
    }

    static func writeProfile(in directory: URL) throws -> URL {
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        let url = directory.appendingPathComponent("\(profileName).terminal")
        let profile: [String: Any] = [
            "name": profileName, "type": "Window Settings", "ProfileCurrentVersion": 2.09,
            "DisableANSIColor": true, "UseBoldFonts": true, "UseBrightBold": false,
            "FontAntialias": true, "CursorType": 2, "warnOnShellCloseAction": 2,
            "ShowActiveProcessInTitle": false, "ShowRepresentedURLInTitle": false,
            "ShowDimensionsInTitle": false, "ShowShellCommandInTitle": false,
            "ShowTTYNameInTitle": false, "ShowWindowSettingsNameInTitle": false,
        ]
        let data = try PropertyListSerialization.data(fromPropertyList: profile, format: .xml, options: 0)
        if (try? Data(contentsOf: url)) != data { try data.write(to: url, options: .atomic) }
        return url
    }

    /// Import once, without quitting Terminal or touching existing sessions.
    /// Terminal opens a shell when importing; close only that owned window.
    /// Each real tab then gets its own color overrides, independent of its peers.
    static func bootstrapScript(profileURL: URL) -> String {
        let path = profileURL.path.replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")
        return """
        if not (exists settings set "\(profileName)") then
          set _slabBeforeImport to id of every window
          open POSIX file "\(path)"
          repeat 50 times
            set _slabImportClosed to false
            repeat with _slabImportWindow in windows
              if id of _slabImportWindow is not in _slabBeforeImport then
                if (count tabs of _slabImportWindow) is 1 then
                  set _slabImportTab to tab 1 of _slabImportWindow
                  if name of current settings of _slabImportTab is "\(profileName)" then
                    close _slabImportWindow
                    set _slabImportClosed to true
                  end if
                end if
              end if
            end repeat
            if _slabImportClosed then exit repeat
            delay 0.1
          end repeat
        end if
        set slabSS to settings set "\(profileName)"
        set font name of slabSS to font name of default settings
        """
    }
}
