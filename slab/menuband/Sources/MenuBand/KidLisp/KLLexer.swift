// Tokenizer for KidLisp.
//
// Handles bare-command syntax: top-level forms can be paren-wrapped
// `(wipe blue)` or written as comma/newline-separated commands
// `purple, ink, line, blur 5`. The lexer surfaces both LPAREN/RPAREN
// and COMMA/NEWLINE; the parser decides what they mean.

import Foundation

enum KLTokenKind: Equatable {
    case lparen
    case rparen
    case comma
    case newline
    case number(Double)
    case ident(String)
    case eof
}

struct KLToken: Equatable {
    let kind: KLTokenKind
    let pos: Int   // byte offset into source
}

final class KLLexer {
    private let chars: [Character]
    private var i: Int = 0
    private let count: Int

    init(_ source: String) {
        self.chars = Array(source)
        self.count = self.chars.count
    }

    func tokenize() -> [KLToken] {
        var out: [KLToken] = []
        while i < count {
            let c = chars[i]
            if c == ";" {
                while i < count && chars[i] != "\n" { i += 1 }
                continue
            }
            if c == "\n" {
                out.append(KLToken(kind: .newline, pos: i))
                i += 1
                continue
            }
            if c.isWhitespace { i += 1; continue }
            if c == "(" { out.append(KLToken(kind: .lparen, pos: i)); i += 1; continue }
            if c == ")" { out.append(KLToken(kind: .rparen, pos: i)); i += 1; continue }
            if c == "," { out.append(KLToken(kind: .comma, pos: i)); i += 1; continue }
            if c == "-" || c == "+" || c.isNumber || c == "." {
                if let tok = readNumber() { out.append(tok); continue }
            }
            if let tok = readIdent() { out.append(tok); continue }
            // unknown — skip
            i += 1
        }
        out.append(KLToken(kind: .eof, pos: i))
        return out
    }

    private func readNumber() -> KLToken? {
        let start = i
        var s = ""
        if chars[i] == "+" || chars[i] == "-" {
            // Only treat as number sign if followed by digit/dot AND prior token wasn't an ident.
            let next = i + 1
            guard next < count, chars[next].isNumber || chars[next] == "." else {
                // Treat as ident-y operator instead.
                s.append(chars[i]); i += 1
                return KLToken(kind: .ident(s), pos: start)
            }
            s.append(chars[i]); i += 1
        }
        var sawDot = false
        var sawDigit = false
        while i < count {
            let c = chars[i]
            if c.isNumber { sawDigit = true; s.append(c); i += 1; continue }
            if c == "." && !sawDot { sawDot = true; s.append(c); i += 1; continue }
            break
        }
        guard sawDigit, Double(s) != nil else {
            i = start
            return nil
        }
        // Timing token check: a number immediately followed by `s` (possibly
        // `s...` or `s!`) is a single identifier like `2s...`, `0.5s`, `1s!`
        // — `readNumber` shouldn't bite off just the number and leave the
        // `s` for the next ident pass.
        if i < count, chars[i] == "s" {
            s.append(chars[i]); i += 1
            // `...` iterating suffix
            while i < count, chars[i] == "." {
                s.append(chars[i]); i += 1
            }
            // `!` bang suffix
            if i < count, chars[i] == "!" {
                s.append(chars[i]); i += 1
            }
            return KLToken(kind: .ident(s), pos: start)
        }
        guard let d = Double(s) else {
            i = start
            return nil
        }
        return KLToken(kind: .number(d), pos: start)
    }

    private func readIdent() -> KLToken? {
        let start = i
        var s = ""
        // Idents may contain almost anything that isn't a structural char.
        while i < count {
            let c = chars[i]
            if c.isWhitespace || c == "(" || c == ")" || c == "," || c == ";" { break }
            s.append(c); i += 1
        }
        if s.isEmpty { return nil }
        return KLToken(kind: .ident(s), pos: start)
    }
}
