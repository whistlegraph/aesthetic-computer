// Parser: tokens → KLExpr tree.
//
// Top level is a sequence of statements separated by commas or newlines.
// A statement is either:
//   * a paren form         (wipe blue)
//   * a bare-command form  ink red    /  line  /  blur 5
//   * a lone atom          blue        — wraps as implicit wipe later
//
// Inside parens, the standard Lisp rules apply.

import Foundation

final class KLParser {
    private let tokens: [KLToken]
    private var i: Int = 0

    init(_ tokens: [KLToken]) {
        self.tokens = tokens
    }

    func parseProgram() -> KLExpr {
        var stmts: [KLExpr] = []
        while !atEnd() {
            skipSeparators()
            if atEnd() { break }
            if let stmt = parseStatement() {
                stmts.append(stmt)
            }
        }
        return .program(stmts)
    }

    private func parseStatement() -> KLExpr? {
        if case .lparen = peek().kind {
            return parseList()
        }
        // Bare-command form: collect atoms until comma/newline/EOF.
        var atoms: [KLExpr] = []
        while !atEnd() {
            let t = peek()
            if case .comma = t.kind { break }
            if case .newline = t.kind { break }
            if case .lparen = t.kind {
                if let l = parseList() { atoms.append(l) }
                continue
            }
            if case .rparen = t.kind { break }  // shouldn't happen at top level
            if let a = parseAtom() { atoms.append(a) }
        }
        guard !atoms.isEmpty else { return nil }
        // Symbol-headed statements always dispatch as bare commands —
        // a lone `ink` or `line` is a zero-arg invocation, not an atom.
        // The evaluator falls back to "implicit wipe" if the head turns
        // out to be a color name with no args.
        if case .symbol(let head) = atoms[0] {
            return .bareForm(head, Array(atoms.dropFirst()))
        }
        if atoms.count == 1 {
            return .atom(atoms[0])
        }
        return .list(atoms)
    }

    private func parseList() -> KLExpr? {
        guard case .lparen = peek().kind else { return nil }
        advance()
        var items: [KLExpr] = []
        while !atEnd() {
            let t = peek()
            if case .rparen = t.kind { advance(); break }
            if case .newline = t.kind { advance(); continue }  // newlines inside parens are whitespace
            if case .comma = t.kind { advance(); continue }
            if case .lparen = t.kind {
                if let inner = parseList() { items.append(inner) }
                continue
            }
            if let a = parseAtom() { items.append(a) }
        }
        return .list(items)
    }

    private func parseAtom() -> KLExpr? {
        let t = peek()
        switch t.kind {
        case .number(let d):
            advance(); return .number(d)
        case .ident(let s):
            advance(); return .symbol(s)
        default:
            advance()
            return nil
        }
    }

    private func skipSeparators() {
        while !atEnd() {
            let t = peek()
            if case .comma = t.kind { advance(); continue }
            if case .newline = t.kind { advance(); continue }
            break
        }
    }

    private func peek() -> KLToken { tokens[i] }
    @discardableResult private func advance() -> KLToken { defer { i += 1 }; return tokens[i] }
    private func atEnd() -> Bool {
        if case .eof = tokens[i].kind { return true }
        return false
    }
}
