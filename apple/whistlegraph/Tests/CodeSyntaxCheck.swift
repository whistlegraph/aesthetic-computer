import Foundation

@main enum CodeSyntaxCheck {
    static func main() {
        let source = "// export 42\nconst bee = \"🐝\"; paint(12); /* still typing"
        let tokens = CodeSyntax.tokens(source)
        let values = tokens.map { (source as NSString).substring(with: $0.range) }
        precondition(values == ["// export 42", "const", "\"🐝\"", "paint", "12", "/* still typing"])
        precondition(tokens.map(\.kind) == ["comment", "keyword", "string", "call", "number", "comment"])
        precondition(CodeSyntax.tokens("const color = \"pi").last?.kind == "string")
        precondition(CodeSyntax.tokens("const s = `hello ${name}").last?.kind == "string")
        precondition(CodeSyntax.tokens("").isEmpty)
        print("PASS source highlighting: Unicode, comments, calls, numbers, and unfinished strings.")
    }
}
