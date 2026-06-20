// AppDelegate — window, TypeWizard control panel (axis sliders), live canvas.
import AppKit

final class SliderRow: NSView {
    let slider = NSSlider()
    let valLabel = NSTextField(labelWithString: "")
    var onChange: (Double) -> Void = { _ in }
    var fmt: (Double) -> String = { String(format: "%.0f", $0) }

    init(title: String, min: Double, max: Double, value: Double, fmt: @escaping (Double) -> String) {
        super.init(frame: .zero)
        self.fmt = fmt
        let name = NSTextField(labelWithString: title)
        name.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
        name.textColor = .secondaryLabelColor
        slider.minValue = min; slider.maxValue = max; slider.doubleValue = value
        slider.target = self; slider.action = #selector(changed)
        slider.isContinuous = true
        valLabel.font = .monospacedSystemFont(ofSize: 10, weight: .regular)
        valLabel.alignment = .right; valLabel.stringValue = fmt(value)
        let row = NSStackView(views: [name, slider, valLabel])
        row.orientation = .horizontal; row.spacing = 8
        name.setContentHuggingPriority(.defaultLow, for: .horizontal)
        valLabel.widthAnchor.constraint(equalToConstant: 46).isActive = true
        name.widthAnchor.constraint(equalToConstant: 64).isActive = true
        row.translatesAutoresizingMaskIntoConstraints = false
        addSubview(row)
        NSLayoutConstraint.activate([
            row.leadingAnchor.constraint(equalTo: leadingAnchor),
            row.trailingAnchor.constraint(equalTo: trailingAnchor),
            row.topAnchor.constraint(equalTo: topAnchor),
            row.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])
    }
    required init?(coder: NSCoder) { fatalError() }
    @objc func changed() { valLabel.stringValue = fmt(slider.doubleValue); onChange(slider.doubleValue) }
}

final class AppDelegate: NSObject, NSApplicationDelegate, NSTextFieldDelegate {
    let canvas = CanvasView()
    var window: NSWindow!

    func applicationDidFinishLaunching(_ note: Notification) {
        let panel = makePanel()
        canvas.translatesAutoresizingMaskIntoConstraints = false

        let split = NSStackView(views: [panel, canvas])
        split.orientation = .horizontal; split.spacing = 0; split.distribution = .fill
        panel.widthAnchor.constraint(equalToConstant: 320).isActive = true

        window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1120, height: 680),
                          styleMask: [.titled, .closable, .resizable, .miniaturizable],
                          backing: .buffered, defer: false)
        window.title = "GlyphWizard — Aesthetic Inc"
        window.contentView = split
        window.center()
        window.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }

    private func head(_ s: String) -> NSTextField {
        let t = NSTextField(labelWithString: s)
        t.font = .monospacedSystemFont(ofSize: 10, weight: .bold)
        t.textColor = .systemGreen
        return t
    }

    private func makePanel() -> NSView {
        let stack = NSStackView()
        stack.orientation = .vertical; stack.alignment = .leading; stack.spacing = 9
        stack.edgeInsets = NSEdgeInsets(top: 18, left: 18, bottom: 18, right: 18)
        stack.translatesAutoresizingMaskIntoConstraints = false

        let title = NSTextField(labelWithString: "GLYPHWIZARD")
        title.font = .monospacedSystemFont(ofSize: 13, weight: .bold)
        stack.addArrangedSubview(title)

        // word field
        stack.addArrangedSubview(head("WORDMARK"))
        let word = NSTextField(string: canvas.doc.word)
        word.delegate = self; word.font = .monospacedSystemFont(ofSize: 13, weight: .regular)
        word.widthAnchor.constraint(equalToConstant: 200).isActive = true
        word.tag = 99
        stack.addArrangedSubview(word)

        // axes
        stack.addArrangedSubview(head("TYPEWIZARD · AXES"))
        func add(_ r: SliderRow) { r.widthAnchor.constraint(equalToConstant: 284).isActive = true; stack.addArrangedSubview(r) }
        let a = canvas.doc.axes
        let w = SliderRow(title: "weight", min: 10, max: 240, value: Double(a.weight)) { String(format: "%.0f", $0) }
        w.onChange = { [weak self] v in self?.canvas.doc.axes.weight = CGFloat(v); self?.canvas.needsDisplay = true }; add(w)
        let c = SliderRow(title: "contrast", min: 0, max: 0.85, value: Double(a.contrast)) { String(format: "%.2f", $0) }
        c.onChange = { [weak self] v in self?.canvas.doc.axes.contrast = CGFloat(v); self?.canvas.needsDisplay = true }; add(c)
        let wd = SliderRow(title: "width", min: 0.6, max: 1.5, value: Double(a.width)) { String(format: "%.2f", $0) }
        wd.onChange = { [weak self] v in self?.canvas.doc.axes.width = CGFloat(v); self?.canvas.needsDisplay = true }; add(wd)
        let sl = SliderRow(title: "slant°", min: -6, max: 24, value: Double(a.slant)) { String(format: "%.1f", $0) }
        sl.onChange = { [weak self] v in self?.canvas.doc.axes.slant = CGFloat(v); self?.canvas.needsDisplay = true }; add(sl)
        let xh = SliderRow(title: "x-height", min: 0.8, max: 1.2, value: Double(a.xHeight)) { String(format: "%.2f", $0) }
        xh.onChange = { [weak self] v in self?.canvas.doc.axes.xHeight = CGFloat(v); self?.canvas.needsDisplay = true }; add(xh)
        let tr = SliderRow(title: "tracking", min: -40, max: 160, value: Double(a.tracking)) { String(format: "%.0f", $0) }
        tr.onChange = { [weak self] v in self?.canvas.doc.axes.tracking = CGFloat(v); self?.canvas.needsDisplay = true }; add(tr)

        // toggles
        let ends = NSButton(checkboxWithTitle: "round terminals", target: self, action: #selector(toggleEnds(_:)))
        ends.state = .on
        let sk = NSButton(checkboxWithTitle: "show skeleton (drag points)", target: self, action: #selector(toggleSkel(_:)))
        sk.state = .on
        stack.addArrangedSubview(ends); stack.addArrangedSubview(sk)

        // export
        stack.addArrangedSubview(head("OUTPUT"))
        let ufo = NSButton(title: "Export UFO →", target: self, action: #selector(exportUFO))
        stack.addArrangedSubview(ufo)
        let hint = NSTextField(wrappingLabelWithString: "Then: bin/compile.sh <out.ufo> → OTF/TTF/woff2 via fontTools.")
        hint.font = .monospacedSystemFont(ofSize: 10, weight: .regular); hint.textColor = .tertiaryLabelColor
        hint.widthAnchor.constraint(equalToConstant: 284).isActive = true
        stack.addArrangedSubview(hint)

        let container = NSView()
        container.wantsLayer = true
        container.layer?.backgroundColor = NSColor(white: 0.10, alpha: 1).cgColor
        container.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: container.trailingAnchor),
            stack.topAnchor.constraint(equalTo: container.topAnchor),
        ])
        return container
    }

    @objc func toggleEnds(_ s: NSButton) { canvas.doc.axes.roundEnds = (s.state == .on); canvas.needsDisplay = true }
    @objc func toggleSkel(_ s: NSButton) { canvas.doc.showSkeleton = (s.state == .on); canvas.needsDisplay = true }

    func controlTextDidChange(_ obj: Notification) {
        guard let f = obj.object as? NSTextField, f.tag == 99 else { return }
        canvas.doc.word = f.stringValue.isEmpty ? "regarde" : f.stringValue
        canvas.needsDisplay = true
    }

    @objc func exportUFO() {
        let p = NSSavePanel()
        p.nameFieldStringValue = "Regarde.ufo"
        p.begin { [weak self] r in
            guard r == .OK, let url = p.url, let self = self else { return }
            do { try UFOExport.write(self.canvas.doc, to: url) }
            catch { NSAlert(error: error).runModal() }
        }
    }
}
