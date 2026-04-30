import AppKit

/// Family-grouped scrollable list of GarageBand sampler patches. Mirrors
/// the API of `InstrumentListView` — `onCommit`, `onHover`, and the
/// `selectedPatchURL` highlight — so the popover can swap between the
/// two views in the same physical rectangle without re-architecting the
/// surrounding UI.
///
/// Layout: a vertical `NSStackView` of section blocks. Each block is a
/// "FAMILY NAME" header label followed by one row per patch. Rows are
/// click + hover targets; the active patch is rendered with the system
/// accent fill, hovered rows tint lightly.
final class GarageBandPatchView: NSView {
    /// Click commit. Receives the URL of the picked patch.
    var onCommit: ((URL) -> Void)?
    /// Hover preview. Same press-gated semantics as `InstrumentListView`
    /// — only fires while the user is dragging with the mouse held.
    var onHover: ((URL?) -> Void)?

    /// Highlight the row matching this URL. Set by the controller after
    /// `onCommit` fires (and on initial show from saved state). nil =
    /// nothing selected.
    var selectedPatchURL: URL? {
        didSet { needsDisplay = true; refreshRowHighlights() }
    }

    /// Match the GM grid's footprint so the popover doesn't reflow when
    /// switching backends.
    static let preferredWidth: CGFloat = InstrumentListView.preferredWidth
    static let preferredHeight: CGFloat = InstrumentListView.preferredHeight

    private let scrollView = NSScrollView()
    private let documentView = NSView()
    private var rows: [PatchRow] = []
    private var dragging = false

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.cornerRadius = 4
        layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
        layer?.borderColor = NSColor.separatorColor.cgColor
        layer?.borderWidth = 0.5

        scrollView.translatesAutoresizingMaskIntoConstraints = false
        scrollView.hasVerticalScroller = true
        scrollView.scrollerStyle = .overlay
        scrollView.drawsBackground = false
        scrollView.contentView.drawsBackground = false
        scrollView.documentView = documentView
        addSubview(scrollView)
        NSLayoutConstraint.activate([
            scrollView.leadingAnchor.constraint(equalTo: leadingAnchor),
            scrollView.trailingAnchor.constraint(equalTo: trailingAnchor),
            scrollView.topAnchor.constraint(equalTo: topAnchor),
            scrollView.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])

        rebuild()
    }
    required init?(coder: NSCoder) { fatalError() }

    override var intrinsicContentSize: NSSize {
        NSSize(width: Self.preferredWidth, height: Self.preferredHeight)
    }

    /// Rebuild the row list from `GarageBandLibrary.cache`. Called once
    /// on init; can be re-called if we ever support live re-scanning.
    private func rebuild() {
        documentView.subviews.forEach { $0.removeFromSuperview() }
        rows.removeAll()
        let groups = GarageBandLibrary.groupedByFamily
        let stack = NSStackView()
        stack.translatesAutoresizingMaskIntoConstraints = false
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 0
        stack.edgeInsets = NSEdgeInsets(top: 4, left: 0, bottom: 6, right: 0)
        for group in groups {
            let header = NSTextField(labelWithString: group.family.uppercased())
            header.font = NSFont.systemFont(ofSize: 9, weight: .bold)
            header.textColor = .secondaryLabelColor
            let headerWrap = NSView()
            headerWrap.translatesAutoresizingMaskIntoConstraints = false
            headerWrap.addSubview(header)
            header.translatesAutoresizingMaskIntoConstraints = false
            NSLayoutConstraint.activate([
                header.leadingAnchor.constraint(equalTo: headerWrap.leadingAnchor, constant: 8),
                header.topAnchor.constraint(equalTo: headerWrap.topAnchor, constant: 6),
                header.bottomAnchor.constraint(equalTo: headerWrap.bottomAnchor, constant: -2),
                headerWrap.widthAnchor.constraint(equalToConstant: Self.preferredWidth),
            ])
            stack.addArrangedSubview(headerWrap)

            for patch in group.patches {
                let row = PatchRow(patch: patch, parent: self)
                rows.append(row)
                stack.addArrangedSubview(row)
                row.widthAnchor.constraint(equalToConstant: Self.preferredWidth).isActive = true
            }
        }
        documentView.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.topAnchor.constraint(equalTo: documentView.topAnchor),
            stack.leadingAnchor.constraint(equalTo: documentView.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: documentView.trailingAnchor),
            stack.bottomAnchor.constraint(equalTo: documentView.bottomAnchor),
            documentView.widthAnchor.constraint(equalToConstant: Self.preferredWidth),
        ])
    }

    fileprivate func refreshRowHighlights() {
        for row in rows { row.needsDisplay = true }
    }

    // MARK: - Press-gated mouse handling
    //
    // Same model as InstrumentMapView: passive hover does nothing;
    // mouseDown arms drag-browse, mouseDragged updates hover/preview,
    // mouseUp commits. The PatchRow forwards events up to here so the
    // gesture state lives in one place.

    fileprivate func didPressDown(at point: NSPoint) {
        dragging = true
        forwardHover(at: point)
    }

    fileprivate func didDrag(to point: NSPoint) {
        guard dragging else { return }
        forwardHover(at: point)
    }

    fileprivate func didMouseUp(at point: NSPoint) {
        guard dragging else { return }
        dragging = false
        onHover?(nil)
        if let row = rowAt(point: point) {
            onCommit?(row.patch.url)
            selectedPatchURL = row.patch.url
        } else {
            // Unhighlight all
            for r in rows { r.isHovered = false }
        }
    }

    private func forwardHover(at point: NSPoint) {
        let hit = rowAt(point: point)
        for r in rows { r.isHovered = (r === hit) }
        onHover?(hit?.patch.url)
    }

    private func rowAt(point: NSPoint) -> PatchRow? {
        // `point` is in our coordinate space; rows live inside the
        // scroll's documentView, so convert through.
        let docPt = documentView.convert(point, from: self)
        return rows.first { $0.frame.contains(docPt) }
    }
}

/// A single row in the patch list. Draws its own background so we don't
/// have to rebuild the whole stack just to update one highlight.
private final class PatchRow: NSView {
    let patch: GarageBandLibrary.Patch
    weak var parent: GarageBandPatchView?
    var isHovered: Bool = false {
        didSet { if oldValue != isHovered { needsDisplay = true } }
    }

    private let nameLabel = NSTextField(labelWithString: "")

    init(patch: GarageBandLibrary.Patch, parent: GarageBandPatchView) {
        self.patch = patch
        self.parent = parent
        super.init(frame: NSRect(x: 0, y: 0, width: 224, height: 22))
        translatesAutoresizingMaskIntoConstraints = false
        wantsLayer = true
        nameLabel.translatesAutoresizingMaskIntoConstraints = false
        nameLabel.stringValue = patch.displayName
        nameLabel.font = NSFont.systemFont(ofSize: 11)
        nameLabel.textColor = .labelColor
        nameLabel.lineBreakMode = .byTruncatingTail
        addSubview(nameLabel)
        NSLayoutConstraint.activate([
            heightAnchor.constraint(equalToConstant: 22),
            nameLabel.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 10),
            nameLabel.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -8),
            nameLabel.centerYAnchor.constraint(equalTo: centerYAnchor),
        ])
    }
    required init?(coder: NSCoder) { fatalError() }

    override func draw(_ dirtyRect: NSRect) {
        let isSelected = parent?.selectedPatchURL == patch.url
        if isSelected {
            NSColor.controlAccentColor.withAlphaComponent(0.85).setFill()
            bounds.fill()
            nameLabel.textColor = .white
        } else if isHovered {
            NSColor.controlAccentColor.withAlphaComponent(0.20).setFill()
            bounds.fill()
            nameLabel.textColor = .labelColor
        } else {
            nameLabel.textColor = .labelColor
        }
    }

    override func mouseDown(with event: NSEvent) {
        let pt = (parent ?? self).convert(event.locationInWindow, from: nil)
        parent?.didPressDown(at: parent?.convert(pt, from: parent) ?? pt)
        // Translate the press into a hit on this row directly — that's
        // the simpler path; parent.didPressDown rehits via geometry.
    }
    override func mouseDragged(with event: NSEvent) {
        guard let parent = parent else { return }
        let pt = parent.convert(event.locationInWindow, from: nil)
        parent.didDrag(to: pt)
    }
    override func mouseUp(with event: NSEvent) {
        guard let parent = parent else { return }
        let pt = parent.convert(event.locationInWindow, from: nil)
        parent.didMouseUp(at: pt)
    }
}
