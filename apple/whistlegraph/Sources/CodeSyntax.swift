import Foundation

/// A tolerant display lexer: unfinished strings/comments remain colored while
/// source arrives. It does not determine whether code is safe to compile.
enum CodeSyntax {
    struct Token { let range: NSRange; let kind: String }
    private static let expression = try! NSRegularExpression(pattern: #"(?<comment>//[^\n\r]*|/\*[\s\S]*?(?:\*/|$))|(?<string>"(?:\\[\s\S]|[^"\\])*(?:"|$)|'(?:\\[\s\S]|[^'\\])*(?:'|$)|`(?:\\[\s\S]|[^`\\])*(?:`|$))|(?<number>\b(?:0[xX][\da-fA-F]+|\d+(?:\.\d*)?(?:[eE][+-]?\d+)?)\b)|(?<keyword>\b(?:export|function|const|let|var|if|else|return|for|while|do|switch|case|break|continue|new|class|extends|async|await|import|from|default|try|catch|throw|finally|true|false|null|undefined|typeof|of|in)\b)|(?<call>\b[A-Za-z_$][\w$]*(?=\s*\())"#)
    static func tokens(_ source: String) -> [Token] {
        expression.matches(in: source, range: NSRange(source.startIndex..., in: source)).compactMap { match in
            for kind in ["comment", "string", "number", "keyword", "call"] {
                let range = match.range(withName: kind)
                if range.location != NSNotFound { return Token(range: range, kind: kind) }
            }
            return nil
        }
    }
}
