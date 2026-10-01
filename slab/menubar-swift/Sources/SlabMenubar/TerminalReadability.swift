import AppKit

/// Keep app colors enabled. Slab owns the page, default ink and ANSI palette;
/// truecolor text remains app-owned (and must use the right light/dark theme).
enum TerminalReadability {
    typealias RGB = (Int, Int, Int)
    static let profileName = "Slab-Rainbow-v2"

    // Conventional ANSI roles, with orange and indigo in the bright bank.
    // Preserve each hue when adjusting it against the actual status page.
    static let ansiNames = ["Black", "Red", "Green", "Yellow", "Blue", "Magenta", "Cyan", "White",
                            "BrightBlack", "BrightRed", "BrightGreen", "BrightYellow",
                            "BrightBlue", "BrightMagenta", "BrightCyan", "BrightWhite"]
    static let rainbow: [RGB] = [
        (48, 42, 58), (220, 40, 67), (21, 156, 69), (191, 147, 0),
        (33, 108, 230), (163, 53, 208), (0, 155, 170), (191, 195, 209),
        (122, 116, 137), (255, 88, 94), (74, 205, 86), (244, 122, 21),
        (108, 90, 239), (223, 73, 201), (26, 193, 177), (246, 243, 255),
    ].map { ($0.0 * 257, $0.1 * 257, $0.2 * 257) }

    static func ansi(on background: RGB) -> [RGB] {
        // Leave headroom for Terminal's ANSI rendering and channel rounding.
        rainbow.map { ink($0, on: background, minimum: 5) }
    }

    /// OSC 4 changes only this tab's ANSI swatches, including already-painted
    /// cells. Leave the extended cube and RGB colors to the application's theme.
    static func paletteEscape(on background: RGB) -> String {
        ansi(on: background).enumerated().map { index, c in
            String(format: "\u{1b}]4;%d;rgb:%04x/%04x/%04x\u{7}", index, c.0, c.1, c.2)
        }.joined()
    }

    static func archive(_ rgb: RGB) throws -> Data {
        try NSKeyedArchiver.archivedData(withRootObject:
            NSColor(calibratedRed: CGFloat(rgb.0) / 65535, green: CGFloat(rgb.1) / 65535,
                    blue: CGFloat(rgb.2) / 65535, alpha: 1), requiringSecureCoding: true)
    }

    static func luminance(_ rgb: RGB) -> Double {
        // Terminal's AppleScript triples are calibrated Generic RGB. Using
        // device/sRGB here overstates contrast on light pages (render-tested).
        let c = NSColor(calibratedRed: CGFloat(rgb.0) / 65535,
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
        var start = color, end = pole
        if pole == white {
            // Raise brightness before adding white. This retains saturated
            // rainbow ink on dark pages instead of prematurely making pastels.
            let peak = max(color.0, color.1, color.2)
            if peak > 0 {
                func boost(_ v: Int) -> Int { Int((Double(v) * 65535 / Double(peak)).rounded()) }
                let vivid = (boost(color.0), boost(color.1), boost(color.2))
                if contrast(vivid, background) >= target { end = vivid } else { start = vivid }
            }
        }
        func blend(_ t: Double) -> RGB {
            func v(_ a: Int, _ b: Int) -> Int { Int((Double(a) + Double(b - a) * t).rounded()) }
            return (v(start.0, end.0), v(start.1, end.1), v(start.2, end.2))
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
        var profile: [String: Any] = [
            "name": profileName, "type": "Window Settings", "ProfileCurrentVersion": 2.09,
            "DisableANSIColor": false, "UseBoldFonts": true, "UseBrightBold": false,
            "FontAntialias": true, "CursorType": 2, "warnOnShellCloseAction": 2,
            "ShowActiveProcessInTitle": false, "ShowRepresentedURLInTitle": false,
            "ShowDimensionsInTitle": false, "ShowShellCommandInTitle": false,
            "ShowTTYNameInTitle": false, "ShowWindowSettingsNameInTitle": false,
        ]
        for (name, color) in zip(ansiNames, ansi(on: (65535, 65535, 65535))) {
            profile["ANSI\(name)Color"] = try archive(color)
        }
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
