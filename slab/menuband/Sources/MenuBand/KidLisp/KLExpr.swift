// KidLisp AST.

import Foundation

indirect enum KLExpr {
    case number(Double)
    case symbol(String)
    case list([KLExpr])              // paren form: (head arg ...) — first elem is the head
    case bareForm(String, [KLExpr])  // top-level non-paren form: `ink red` or `blur 5`
    case atom(KLExpr)                // top-level lone atom: `blue` — implicit wipe
    case program([KLExpr])           // top-level sequence

    var asDouble: Double? {
        if case .number(let d) = self { return d }
        return nil
    }

    var asSymbol: String? {
        if case .symbol(let s) = self { return s }
        return nil
    }
}
