// UFOExport — write a UFO3 package from the current skeletons + axes.
// Bakes weight/contrast/width/x-height/slant into the outlines; compile to
// OTF/TTF with fontTools via bin/compile.sh.
import Foundation
import CoreGraphics

enum UFOExport {
    static let order: [(name: String, ch: Character, uni: String)] = [
        ("r", "r", "0072"), ("e", "e", "0065"), ("g", "g", "0067"),
        ("a", "a", "0061"), ("d", "d", "0064"), ("space", " ", "0020"),
    ]

    static func write(_ doc: Doc, to url: URL) throws {
        let fm = FileManager.default
        if fm.fileExists(atPath: url.path) { try fm.removeItem(at: url) }
        try fm.createDirectory(at: url, withIntermediateDirectories: true)
        let glyphsDir = url.appendingPathComponent("glyphs")
        try fm.createDirectory(at: glyphsDir, withIntermediateDirectories: true)

        try plist(metainfo).write(to: url.appendingPathComponent("metainfo.plist"), atomically: true, encoding: .utf8)
        try plist(fontinfo).write(to: url.appendingPathComponent("fontinfo.plist"), atomically: true, encoding: .utf8)
        try plist(layercontents).write(to: url.appendingPathComponent("layercontents.plist"), atomically: true, encoding: .utf8)

        var contents = ""
        for e in order { contents += "  <key>\(e.name)</key>\n  <string>\(e.name).glif</string>\n" }
        try plist("<dict>\n\(contents)</dict>").write(to: glyphsDir.appendingPathComponent("contents.plist"), atomically: true, encoding: .utf8)

        let ax = doc.axes
        let tanS = tan(ax.slant * .pi / 180)
        for e in order {
            guard let g = doc.glyphs[e.ch] else { continue }
            let advance = Int((g.advance * ax.width + ax.tracking).rounded())
            var outline = ""
            for loop in Expander.contours(g, ax) {
                outline += "    <contour>\n"
                for p in loop {
                    let x = p.x * ax.width
                    let y = p.y * ax.xHeight
                    let sx = x + tanS * y
                    outline += "      <point x=\"\(fmt(sx))\" y=\"\(fmt(y))\" type=\"line\"/>\n"
                }
                outline += "    </contour>\n"
            }
            let glif = """
            <?xml version="1.0" encoding="UTF-8"?>
            <glyph name="\(e.name)" format="2">
              <advance width="\(advance)"/>
              <unicode hex="\(e.uni)"/>
              <outline>
            \(outline)  </outline>
            </glyph>
            """
            try glif.write(to: glyphsDir.appendingPathComponent("\(e.name).glif"), atomically: true, encoding: .utf8)
        }
    }

    private static func fmt(_ v: CGFloat) -> String { String(format: "%.1f", Double(v)) }

    private static func plist(_ body: String) -> String {
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n<plist version=\"1.0\">\n\(body)\n</plist>\n"
    }
    private static let metainfo = "<dict>\n  <key>creator</key>\n  <string>computer.aesthetic.glyphwizard</string>\n  <key>formatVersion</key>\n  <integer>3</integer>\n</dict>"
    private static let layercontents = "<array>\n  <array>\n    <string>public.default</string>\n    <string>glyphs</string>\n  </array>\n</array>"
    private static let fontinfo = """
    <dict>
      <key>familyName</key>
      <string>Regarde</string>
      <key>styleName</key>
      <string>Regular</string>
      <key>unitsPerEm</key>
      <integer>1000</integer>
      <key>ascender</key>
      <integer>740</integer>
      <key>descender</key>
      <integer>-210</integer>
      <key>capHeight</key>
      <integer>700</integer>
      <key>xHeight</key>
      <integer>520</integer>
    </dict>
    """
}
