// Markdown.swift — a wiki page as typed text.
//
// The wiki (Outline) keeps its pages as markdown. This turns one into an
// attributed string in the app's one monospace face: headings, lists and
// checkboxes, quotes and notices, code, tables as aligned columns, rules,
// pictures, and the inline marks (bold, italic, strike, code, links).

import AppKit

struct Markdown {
    let font: NSFont
    let size: CGFloat
    let wikiBase: String
    let columns: Int                      // how many characters fit across the sheet
    let image: (String) -> NSImage?       // a picture already fetched, by its address
    let maxImage: NSSize

    private var plain: [NSAttributedString.Key: Any] { [.font: font, .foregroundColor: Theme.ink] }
    private var charWidth: CGFloat { ("0" as NSString).size(withAttributes: [.font: font]).width }

    // The addresses of the pictures on a page, so they can be fetched.
    static func images(in markdown: String) -> [String] {
        guard let re = try? NSRegularExpression(pattern: #"!\[[^\]]*\]\(([^)\s]+)[^)]*\)"#) else { return [] }
        let ns = markdown as NSString
        return re.matches(in: markdown, range: NSRange(location: 0, length: ns.length)).map { ns.substring(with: $0.range(at: 1)) }
    }

    func render(_ markdown: String) -> NSMutableAttributedString {
        let out = NSMutableAttributedString()
        let lines = markdown.components(separatedBy: "\n")
        var i = 0
        var blank = true                  // the page starts as if after an empty line
        var notice = false                // inside a :::info … ::: block

        func gap() {
            if !blank { out.append(NSAttributedString(string: "\n", attributes: [.font: Theme.mono(4)])) }
            blank = true
        }
        func add(_ s: NSAttributedString, _ style: NSParagraphStyle) {
            let line = NSMutableAttributedString(attributedString: s)
            line.append(NSAttributedString(string: "\n", attributes: plain))
            line.addAttribute(.paragraphStyle, value: style, range: NSRange(location: 0, length: line.length))
            out.append(line)
            blank = false
        }
        func indent(_ first: CGFloat, _ rest: CGFloat, before: CGFloat = 0, after: CGFloat = 1) -> NSMutableParagraphStyle {
            let p = NSMutableParagraphStyle()
            p.firstLineHeadIndent = first * charWidth
            p.headIndent = rest * charWidth
            p.paragraphSpacingBefore = before
            p.paragraphSpacing = after
            return p
        }

        while i < lines.count {
            let raw = lines[i]
            let trimmed = raw.trimmingCharacters(in: .whitespaces)
            let lead = CGFloat(raw.prefix { $0 == " " || $0 == "\t" }.reduce(0) { $0 + ($1 == "\t" ? 4 : 1) })
            let base: CGFloat = notice ? 2 : 0      // a notice hangs two characters in
            i += 1

            if trimmed.isEmpty || trimmed == "\\" { gap(); continue }

            // fenced code: kept as typed, on a faint ground
            if trimmed.hasPrefix("```") {
                gap()
                while i < lines.count, !lines[i].trimmingCharacters(in: .whitespaces).hasPrefix("```") {
                    add(NSAttributedString(string: lines[i].isEmpty ? " " : lines[i], attributes: [
                        .font: Theme.mono(size - 1), .foregroundColor: Theme.ink,
                        .backgroundColor: Theme.chip]), indent(base + 1, base + 1, after: 0))
                    i += 1
                }
                i += 1
                gap()
                continue
            }

            // :::info / :::warning / :::tip … :::
            if trimmed.hasPrefix(":::") {
                let kind = trimmed.dropFirst(3).trimmingCharacters(in: .whitespaces)
                gap()
                if kind.isEmpty { notice = false } else {
                    notice = true
                    add(NSAttributedString(string: "▍" + kind.uppercased(), attributes: [
                        .font: Theme.mono(size - 2, bold: true), .foregroundColor: Theme.faint]), indent(0, 0))
                }
                continue
            }

            // a rule
            if trimmed.count >= 3, Set(trimmed).isSubset(of: ["-"]) || Set(trimmed).isSubset(of: ["*"]) || Set(trimmed).isSubset(of: ["_"]) {
                gap()
                add(NSAttributedString(string: String(repeating: "─", count: max(8, min(columns - 2, 60))), attributes: [
                    .font: font, .foregroundColor: Theme.faint]), indent(0, 0))
                gap()
                continue
            }

            // headings
            if trimmed.hasPrefix("#") {
                let level = trimmed.prefix { $0 == "#" }.count
                let words = trimmed.drop { $0 == "#" || $0 == " " }
                let attrs: [NSAttributedString.Key: Any] = [
                    .font: Theme.mono(level == 1 ? size + 1 : size, bold: true),
                    .foregroundColor: level <= 2 ? Theme.teal : Theme.ink]
                let head = NSMutableAttributedString(string: level <= 2 ? "▍" : "", attributes: attrs)
                head.append(inline(level <= 2 ? words.uppercased() : String(words), attrs))
                add(head, indent(0, 2, before: blank && out.length > 0 ? 5 : 9, after: 2))
                continue
            }

            // a table: every row that starts with a bar
            if trimmed.hasPrefix("|") {
                var rows: [[String]] = []
                i -= 1
                while i < lines.count, lines[i].trimmingCharacters(in: .whitespaces).hasPrefix("|") {
                    var cells = lines[i].trimmingCharacters(in: .whitespaces).components(separatedBy: "|").map {
                        $0.trimmingCharacters(in: .whitespaces)
                    }
                    cells.removeFirst()
                    if cells.last == "" { cells.removeLast() }
                    let divider = !cells.isEmpty && cells.allSatisfy { !$0.isEmpty && Set($0).isSubset(of: ["-", ":", " "]) }
                    if !divider { rows.append(cells) }
                    i += 1
                }
                table(rows, base: base).forEach { add($0, indent(base, base, after: 0)) }
                continue
            }

            // a quote
            if trimmed.hasPrefix(">") {
                let words = String(trimmed.dropFirst()).trimmingCharacters(in: .whitespaces)
                let line = NSMutableAttributedString(string: "▏ ", attributes: [.font: font, .foregroundColor: Theme.teal])
                line.append(inline(words, [.font: font, .foregroundColor: Theme.faint]))
                add(line, indent(base, base + 2))
                continue
            }

            // lists: "- ", "* ", "+ ", "1. ", and the checkbox forms
            if let m = Self.listRe.firstMatch(in: trimmed, range: NSRange(location: 0, length: (trimmed as NSString).length)) {
                let ns = trimmed as NSString
                let marker = ns.substring(with: m.range(at: 1))
                let box = m.range(at: 2).location == NSNotFound ? nil : ns.substring(with: m.range(at: 2))
                let words = ns.substring(from: m.range.length)
                let done = box?.lowercased().contains("x") == true
                let bullet = box != nil ? (done ? "☑ " : "☐ ") : (marker.first!.isNumber ? marker + " " : "· ")
                let depth = base + (lead / 2).rounded(.down)
                let line = NSMutableAttributedString(string: bullet, attributes: [
                    .font: font, .foregroundColor: box != nil ? Theme.teal : Theme.faint])
                var attrs = plain
                if done { attrs[.foregroundColor] = Theme.faint; attrs[.strikethroughStyle] = NSUnderlineStyle.single.rawValue }
                line.append(inline(words, attrs))
                add(line, indent(depth, depth + CGFloat(bullet.count)))
                continue
            }

            // a picture alone on its line sits at the start of the column
            add(inline(trimmed.hasSuffix("\\") ? String(trimmed.dropLast()) : trimmed, plain), indent(base, base))
        }
        return out
    }

    private static let listRe = try! NSRegularExpression(pattern: #"^([-*+]|\d+[.)])\s+(\[[ xX]\]\s+)?"#)

    // Rows as aligned columns. A column is as wide as its longest cell, and the
    // widest give way until the row fits the sheet; a cell too long for its
    // column wraps inside it, so a row may take several lines.
    private func table(_ rows: [[String]], base: CGFloat) -> [NSAttributedString] {
        let count = rows.map { $0.count }.max() ?? 0
        guard count > 0 else { return [] }
        let cells = rows.map { r in (0..<count).map { r.indices.contains($0) ? Self.bare(r[$0]) : "" } }
        var widths = (0..<count).map { c in max(1, cells.map { $0[c].count }.max() ?? 1) }
        let room = max(count * 6, columns - Int(base) - 2 * (count - 1) - 1)
        while widths.reduce(0, +) > room, let widest = widths.indices.max(by: { widths[$0] < widths[$1] }), widths[widest] > 6 {
            widths[widest] -= 1
        }
        var out: [NSAttributedString] = []
        for (n, row) in cells.enumerated() {
            let wrapped = row.enumerated().map { c, cell in Self.wrap(cell, widths[c]) }
            let tall = wrapped.map { $0.count }.max() ?? 1
            for k in 0..<tall {
                let text = wrapped.enumerated().map { c, lines in
                    (lines.indices.contains(k) ? lines[k] : "").padding(toLength: widths[c], withPad: " ", startingAt: 0)
                }.joined(separator: "  ")
                let line = NSMutableAttributedString(string: text, attributes: [
                    .font: n == 0 ? Theme.mono(size, bold: true) : font, .foregroundColor: Theme.ink])
                // the heading row is underlined; a row of several lines is ruled off from the next
                if (n == 0 || tall > 1) && k == tall - 1 && n < cells.count - 1 {
                    line.addAttributes([.underlineStyle: NSUnderlineStyle.single.rawValue,
                                        .underlineColor: n == 0 ? Theme.ink : Theme.faint],
                                       range: NSRange(location: 0, length: line.length))
                }
                out.append(line)
            }
        }
        return out
    }

    // Words broken into lines no wider than `width` characters.
    private static func wrap(_ s: String, _ width: Int) -> [String] {
        var lines: [String] = [], line = ""
        for var word in s.split(separator: " ").map(String.init) {
            while word.count > width {                      // a word longer than the column is cut
                if !line.isEmpty { lines.append(line); line = "" }
                lines.append(String(word.prefix(width)))
                word = String(word.dropFirst(width))
            }
            if line.isEmpty { line = word }
            else if line.count + 1 + word.count <= width { line += " " + word }
            else { lines.append(line); line = word }
        }
        if !line.isEmpty || lines.isEmpty { lines.append(line) }
        return lines
    }

    // A table cell without its marks: a link keeps its words.
    private static func bare(_ s: String) -> String {
        var t = s.replacingOccurrences(of: #"!?\[([^\]]*)\]\([^)]*\)"#, with: "$1", options: .regularExpression)
        t = t.replacingOccurrences(of: #"(\*\*|__|~~|`)"#, with: "", options: .regularExpression)
        return t.replacingOccurrences(of: "<br>", with: " ").replacingOccurrences(of: "\\", with: "")
    }

    private static let inlineRe = try! NSRegularExpression(pattern: [
        #"!\[([^\]]*)\]\(([^)\s]+)[^)]*\)"#,                 // 1 2   picture
        #"@?\[([^\]]+)\]\(([^)\s]+)[^)]*\)"#,                // 3 4   link, or a mention
        #"<(https?://[^>\s]+)>"#,                            // 5     bare address
        #"`([^`]+)`"#,                                       // 6     code
        #"\*\*(.+?)\*\*"#, #"__(.+?)__"#,                    // 7 8   bold
        #"~~(.+?)~~"#,                                       // 9     struck
        #"(?<![\w*])\*([^*\s][^*]*?)\*(?![\w*])"#,           // 10    italic
        #"(?<![\w_])_([^_\s][^_]*?)_(?![\w_])"#,             // 11    italic
        #"\\([\\`*_{}\[\]()#+\-.!|>~])"#,                    // 12    an escaped mark
    ].joined(separator: "|"))

    func inline(_ s: String, _ attrs: [NSAttributedString.Key: Any]) -> NSMutableAttributedString {
        let out = NSMutableAttributedString()
        let ns = s as NSString
        var at = 0
        func group(_ m: NSTextCheckingResult, _ n: Int) -> String? {
            m.range(at: n).location == NSNotFound ? nil : ns.substring(with: m.range(at: n))
        }
        for m in Self.inlineRe.matches(in: s, range: NSRange(location: 0, length: ns.length)) {
            out.append(NSAttributedString(string: ns.substring(with: NSRange(location: at, length: m.range.location - at)), attributes: attrs))
            at = m.range.location + m.range.length
            if let address = group(m, 2) {
                let alt = group(m, 1) ?? ""
                let link = target(address)
                if let img = image(address) {
                    let att = NSTextAttachment()
                    att.image = img
                    att.bounds = NSRect(origin: .zero, size: Media.fit(img.size, maxWidth: maxImage.width, maxHeight: maxImage.height))
                    let pic = NSMutableAttributedString(attachment: att)
                    if let link { pic.addAttribute(.link, value: link, range: NSRange(location: 0, length: pic.length)) }
                    out.append(pic)
                } else {
                    var a = attrs
                    a[.foregroundColor] = Theme.faint
                    if let link { a[.link] = link }
                    out.append(NSAttributedString(string: "[picture" + (alt.isEmpty ? "" : ": " + alt) + "]", attributes: a))
                }
            } else if let words = group(m, 3), let address = group(m, 4) {
                var a = attrs
                if address.hasPrefix("mention://") {
                    a[.font] = Theme.mono(size, bold: true)
                    out.append(NSAttributedString(string: "@" + words, attributes: a))
                } else {
                    if let link = target(address) { a[.link] = link }
                    out.append(inline(words, a))
                }
            } else if let address = group(m, 5) {
                var a = attrs
                if let link = URL(string: address) { a[.link] = link }
                out.append(NSAttributedString(string: address, attributes: a))
            } else if let code = group(m, 6) {
                var a = attrs
                a[.backgroundColor] = Theme.chip
                out.append(NSAttributedString(string: code, attributes: a))
            } else if let words = group(m, 7) ?? group(m, 8) {
                var a = attrs
                a[.font] = Theme.mono((attrs[.font] as? NSFont)?.pointSize ?? size, bold: true)
                out.append(inline(words, a))
            } else if let words = group(m, 9) {
                var a = attrs
                a[.strikethroughStyle] = NSUnderlineStyle.single.rawValue
                out.append(inline(words, a))
            } else if let words = group(m, 10) ?? group(m, 11) {
                var a = attrs
                a[.obliqueness] = 0.18
                out.append(inline(words, a))
            } else if let mark = group(m, 12) {
                out.append(NSAttributedString(string: mark, attributes: attrs))
            }
        }
        out.append(NSAttributedString(string: ns.substring(from: at), attributes: attrs))
        return out
    }

    // Where a link goes: another wiki page opens in the sheet, the rest in the browser.
    private func target(_ address: String) -> URL? {
        // a page address may carry "#heading" or "?query"; the page is what comes before
        let page = String(address.prefix { $0 != "#" && $0 != "?" })
        if page.hasPrefix("/doc/") || page.hasPrefix(wikiBase + "/doc/") {
            return URL(string: "ncc://doc/" + (page as NSString).lastPathComponent)
        }
        if address.hasPrefix("/") { return URL(string: wikiBase + address) }
        return URL(string: address)
    }
}
