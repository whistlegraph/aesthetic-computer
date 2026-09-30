#if canImport(SwiftUI)
import SwiftUI

/// SwiftUI placement for a strip whose web view is owned elsewhere.
@available(macOS 11, iOS 15, *)
public struct ACWaveformStrip {
    let view: ACWaveformView
    let span: TimeInterval
    let suppressed: Bool

    public init(_ view: ACWaveformView, span: TimeInterval = 4, suppressed: Bool = false) {
        self.view = view
        self.span = span
        self.suppressed = suppressed
    }

    func update(_ view: ACWaveformView) {
        view.span = span
        if view.isSuppressed != suppressed { view.isSuppressed = suppressed }
    }
}

#if canImport(AppKit)
@available(macOS 11, *)
extension ACWaveformStrip: NSViewRepresentable {
    public func makeNSView(context: Context) -> ACWaveformView { view }
    public func updateNSView(_ view: ACWaveformView, context: Context) { update(view) }
}
#else
@available(iOS 15, *)
extension ACWaveformStrip: UIViewRepresentable {
    public func makeUIView(context: Context) -> ACWaveformView { view }
    public func updateUIView(_ view: ACWaveformView, context: Context) { update(view) }
}
#endif
#endif
