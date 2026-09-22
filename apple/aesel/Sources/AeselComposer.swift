import SwiftUI

/// Native editing with the same 24-point baseline grid as the transcript.
private enum ComposerStyle {
    static let row: CGFloat = 24
    static var attributes: [NSAttributedString.Key: Any] {
        let paragraph = NSMutableParagraphStyle()
        paragraph.minimumLineHeight = row
        paragraph.maximumLineHeight = row
        return [.font: AeselFont(name: "Helvetica", size: 16) ?? AeselFont.systemFont(ofSize: 16),
                .paragraphStyle: paragraph]
    }
}

#if os(macOS)
import AppKit
struct AeselComposer: NSViewRepresentable {
    @Binding var text: String
    @Binding var height: CGFloat
    @Binding var focused: Bool
    var color: Color
    var submit: () -> Void

    final class Editor: NSTextView {
        var focusChanged: (Bool) -> Void = { _ in }
        var resized: () -> Void = {}
        var wantsFocus = false
        override func viewDidMoveToWindow() {
            super.viewDidMoveToWindow()
            DispatchQueue.main.async { [weak self] in
                guard let self, self.wantsFocus else { return }
                self.window?.makeFirstResponder(self)
            }
        }
        override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
        override func becomeFirstResponder() -> Bool {
            let result = super.becomeFirstResponder()
            if result { focusChanged(true) }; return result
        }
        override func resignFirstResponder() -> Bool {
            let result = super.resignFirstResponder()
            if result { focusChanged(false) }; return result
        }
        override func setFrameSize(_ size: NSSize) { super.setFrameSize(size); resized() }
    }
    final class Coordinator: NSObject, NSTextViewDelegate {
        var owner: AeselComposer
        var requestedFocus: Bool?
        init(_ owner: AeselComposer) { self.owner = owner }
        func measure(_ view: NSTextView) {
            guard let layout = view.layoutManager, let container = view.textContainer else { return }
            layout.ensureLayout(for: container)
            let used = max(24, layout.usedRect(for: container).height)
            let height = ceil((used + 8) / 24) * 24
            DispatchQueue.main.async { if self.owner.height != height { self.owner.height = height } }
        }
        func textDidChange(_ notification: Notification) {
            guard let view = notification.object as? NSTextView else { return }
            owner.text = view.string; measure(view)
        }
        func textView(_ textView: NSTextView, doCommandBy commandSelector: Selector) -> Bool {
            if commandSelector == #selector(NSResponder.insertNewline(_:)), !textView.hasMarkedText(),
               !NSEvent.modifierFlags.contains(.shift) {
                owner.submit(); return true
            }
            return false
        }
    }
    func makeCoordinator() -> Coordinator { Coordinator(self) }
    func makeNSView(context: Context) -> Editor {
        let view = Editor(frame: .zero)
        view.isRichText = false; view.allowsUndo = true; view.drawsBackground = false; view.isHorizontallyResizable = false
        view.isEditable = true; view.isSelectable = true
        view.minSize = .zero
        view.maxSize = NSSize(width: CGFloat.greatestFiniteMagnitude, height: CGFloat.greatestFiniteMagnitude)
        view.isVerticallyResizable = false; view.autoresizingMask = [.width]
        view.textContainer?.widthTracksTextView = true
        view.textContainer?.lineFragmentPadding = 0
        view.textContainerInset = NSSize(width: 0, height: 4)
        view.isAutomaticQuoteSubstitutionEnabled = false
        view.isAutomaticDashSubstitutionEnabled = false
        view.isAutomaticTextReplacementEnabled = false
        view.delegate = context.coordinator
        view.setAccessibilityLabel("Message draft")
        view.focusChanged = { value in DispatchQueue.main.async { context.coordinator.owner.focused = value } }
        view.resized = { [weak view] in if let view { context.coordinator.measure(view) } }
        return view
    }
    func updateNSView(_ view: Editor, context: Context) {
        context.coordinator.owner = self
        var attributes = ComposerStyle.attributes
        attributes[.foregroundColor] = NSColor(color)
        if view.string != text {
            view.textStorage?.setAttributedString(NSAttributedString(string: text, attributes: attributes))
        } else if let storage = view.textStorage {
            storage.addAttributes(attributes, range: NSRange(location: 0, length: storage.length))
        }
        view.typingAttributes = attributes; view.insertionPointColor = NSColor(color)
        // A click reports focus asynchronously. Unrelated SwiftUI updates must
        // not revoke that click before its binding update arrives.
        view.wantsFocus = focused
        if context.coordinator.requestedFocus != focused {
            context.coordinator.requestedFocus = focused
            if focused && view.window?.firstResponder !== view { view.window?.makeFirstResponder(view) }
            else if !focused && view.window?.firstResponder === view { view.window?.makeFirstResponder(nil) }
        }
        context.coordinator.measure(view)
    }
}
#else
import UIKit
struct AeselComposer: UIViewRepresentable {
    @Binding var text: String
    @Binding var height: CGFloat
    @Binding var focused: Bool
    var color: Color
    var submit: () -> Void
    final class Coordinator: NSObject, UITextViewDelegate {
        var owner: AeselComposer
        init(_ owner: AeselComposer) { self.owner = owner }
        func measure(_ view: UITextView) {
            guard view.bounds.width > 0 else { return }
            let height = ceil(view.sizeThatFits(CGSize(width: view.bounds.width, height: .greatestFiniteMagnitude)).height / 24) * 24
            DispatchQueue.main.async { if self.owner.height != height { self.owner.height = max(48, height) } }
        }
        func textViewDidChange(_ view: UITextView) { owner.text = view.text; measure(view) }
        func textViewDidBeginEditing(_ view: UITextView) { owner.focused = true }
        func textViewDidEndEditing(_ view: UITextView) { owner.focused = false }
        func textView(_ view: UITextView, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool {
            if text == "\n", view.markedTextRange == nil { owner.submit(); return false }; return true
        }
    }
    func makeCoordinator() -> Coordinator { Coordinator(self) }
    func makeUIView(context: Context) -> UITextView {
        let view = UITextView()
        view.backgroundColor = .clear; view.isScrollEnabled = false
        view.textContainer.lineFragmentPadding = 0
        view.textContainerInset = UIEdgeInsets(top: 4, left: 0, bottom: 4, right: 0)
        view.autocorrectionType = .no; view.returnKeyType = .send
        view.delegate = context.coordinator; view.accessibilityLabel = "Message draft"
        return view
    }
    func updateUIView(_ view: UITextView, context: Context) {
        context.coordinator.owner = self
        var attributes = ComposerStyle.attributes; attributes[.foregroundColor] = UIColor(color)
        if view.text != text { view.attributedText = NSAttributedString(string: text, attributes: attributes) }
        else { view.textStorage.addAttributes(attributes, range: NSRange(location: 0, length: view.textStorage.length)) }
        view.typingAttributes = attributes; view.tintColor = UIColor(color)
        if focused && !view.isFirstResponder { view.becomeFirstResponder() }
        else if !focused && view.isFirstResponder { view.resignFirstResponder() }
        context.coordinator.measure(view)
    }
    func sizeThatFits(_ proposal: ProposedViewSize, uiView: UITextView, context: Context) -> CGSize? {
        if let width = proposal.width { uiView.bounds.size.width = width; context.coordinator.measure(uiView) }
        return nil
    }
}
#endif
