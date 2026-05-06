import AppKit
import AVFoundation
import AudioToolbox
import CoreAudioKit

/// Test-only picker for Liam Hall's Audio Unit instruments. Opens from
/// About → Plugins. Filters the installed AU set down to manufacturer
/// `Lhll` (Liam's four-char code), instantiates the chosen one, and
/// routes Menu Band's melodic notes through it via
/// `MenuBandSynth.setPluginInstrument`. A second window hosts the
/// plug-in's native UI when one is available.
///
/// Intentionally bare and Liam-scoped — this is the "drop angelsaw in
/// and see if it makes sound" surface, not a polished settings panel.
/// If we ever open this up to other manufacturers, drop the filter in
/// `refreshComponents` and re-add the Manufacturer column.
final class AUPluginPickerController: NSWindowController, NSWindowDelegate {
    /// Manufacturer four-char code we filter to. Same value the
    /// angelsaw `Info.plist` declares (`Lhll` = Liam Hall).
    private static let manufacturerFilter: String = "Lhll"
    private let onLoad: (AVAudioUnit) -> Void
    private let onUnload: () -> Void

    private var components: [AVAudioUnitComponent] = []
    private var tableView: NSTableView!
    private var loadButton: NSButton!
    private var unloadButton: NSButton!
    private var showUIButton: NSButton!
    private var statusLabel: NSTextField!

    private var pluginUIWindow: NSWindow?
    private var pluginUIViewController: NSViewController?

    /// Most recently loaded AU — kept so "Show Plugin Window" can reach
    /// its view controller without round-tripping through the synth.
    private var loadedUnit: AVAudioUnit?

    init(currentInstrument: AVAudioUnit?,
         onLoad: @escaping (AVAudioUnit) -> Void,
         onUnload: @escaping () -> Void) {
        self.onLoad = onLoad
        self.onUnload = onUnload
        self.loadedUnit = currentInstrument
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 360, height: 240),
            styleMask: [.titled, .closable, .resizable],
            backing: .buffered,
            defer: false
        )
        window.title = "Liam Hall Pedals"
        window.isReleasedWhenClosed = false
        window.level = NSWindow.Level(rawValue: NSWindow.Level.popUpMenu.rawValue + 1)
        super.init(window: window)
        window.delegate = self
        buildContent()
        refreshComponents()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) is not used")
    }

    func present() {
        guard let window = window else { return }
        window.center()
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    func windowWillClose(_ notification: Notification) {
        // Plugin UI window is independent of the picker — leave it open
        // so the user can keep tweaking parameters with the picker hidden.
    }

    // MARK: - Layout

    private func buildContent() {
        guard let window = window else { return }
        let content = NSView()
        content.translatesAutoresizingMaskIntoConstraints = false
        window.contentView = content

        let scroll = NSScrollView()
        scroll.translatesAutoresizingMaskIntoConstraints = false
        scroll.hasVerticalScroller = true
        scroll.borderType = .bezelBorder
        content.addSubview(scroll)

        let table = NSTableView()
        table.translatesAutoresizingMaskIntoConstraints = false
        table.usesAlternatingRowBackgroundColors = true
        table.headerView = NSTableHeaderView()
        table.allowsMultipleSelection = false
        table.style = .inset
        table.dataSource = self
        table.delegate = self
        table.target = self
        table.doubleAction = #selector(loadSelected)

        let nameCol = NSTableColumn(identifier: NSUserInterfaceItemIdentifier("name"))
        nameCol.title = "Pedal"
        nameCol.width = 220
        nameCol.minWidth = 140
        table.addTableColumn(nameCol)

        let typeCol = NSTableColumn(identifier: NSUserInterfaceItemIdentifier("subtype"))
        typeCol.title = "Subtype"
        typeCol.width = 80
        typeCol.minWidth = 60
        table.addTableColumn(typeCol)

        scroll.documentView = table
        self.tableView = table

        let load = NSButton(title: "Engage",
                            target: self,
                            action: #selector(loadSelected))
        load.bezelStyle = .rounded
        load.keyEquivalent = "\r"
        load.translatesAutoresizingMaskIntoConstraints = false
        loadButton = load

        let unload = NSButton(title: "Disengage",
                              target: self,
                              action: #selector(unloadCurrent))
        unload.bezelStyle = .rounded
        unload.translatesAutoresizingMaskIntoConstraints = false
        unloadButton = unload

        let showUI = NSButton(title: "Show Pedal",
                              target: self,
                              action: #selector(showPluginUI))
        showUI.bezelStyle = .rounded
        showUI.translatesAutoresizingMaskIntoConstraints = false
        showUIButton = showUI

        let rescan = NSButton(title: "Rescan",
                              target: self,
                              action: #selector(rescan))
        rescan.bezelStyle = .rounded
        rescan.translatesAutoresizingMaskIntoConstraints = false

        let status = NSTextField(labelWithString: "")
        status.font = NSFont.systemFont(ofSize: 11)
        status.textColor = .secondaryLabelColor
        status.lineBreakMode = .byTruncatingTail
        status.translatesAutoresizingMaskIntoConstraints = false
        statusLabel = status

        let buttonStack = NSStackView(views: [load, showUI, unload, rescan])
        buttonStack.orientation = .horizontal
        buttonStack.spacing = 8
        buttonStack.alignment = .centerY
        buttonStack.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(buttonStack)
        content.addSubview(status)

        NSLayoutConstraint.activate([
            scroll.topAnchor.constraint(equalTo: content.topAnchor, constant: 16),
            scroll.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            scroll.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),
            scroll.bottomAnchor.constraint(equalTo: buttonStack.topAnchor, constant: -12),

            buttonStack.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            buttonStack.trailingAnchor.constraint(lessThanOrEqualTo: content.trailingAnchor, constant: -16),
            buttonStack.bottomAnchor.constraint(equalTo: status.topAnchor, constant: -8),

            status.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            status.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),
            status.bottomAnchor.constraint(equalTo: content.bottomAnchor, constant: -16),
        ])

        updateButtonsForSelection()
        updateStatusForLoaded()
    }

    // MARK: - Component scan

    @objc private func rescan() {
        refreshComponents()
    }

    private func refreshComponents() {
        // Music device = AU instrument (`aumu`) filtered to Liam Hall's
        // manufacturer code so the picker is just a pedalboard of his
        // builds. Querying by manufacturer up front skips the system's
        // huge GM/AU list and avoids any Apple-AU sort tiebreak.
        let desc = AudioComponentDescription(
            componentType: kAudioUnitType_MusicDevice,
            componentSubType: 0,
            componentManufacturer: fourCharCode(Self.manufacturerFilter),
            componentFlags: 0,
            componentFlagsMask: 0
        )
        let manager = AVAudioUnitComponentManager.shared()
        let found = manager.components(matching: desc)
        components = found.sorted {
            $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending
        }
        tableView.reloadData()
        // Reselect the currently-loaded plugin if we can find it.
        if let loaded = loadedUnit {
            let key = loaded.audioComponentDescription
            if let row = components.firstIndex(where: {
                $0.audioComponentDescription.componentSubType == key.componentSubType &&
                $0.audioComponentDescription.componentManufacturer == key.componentManufacturer &&
                $0.audioComponentDescription.componentType == key.componentType
            }) {
                tableView.selectRowIndexes(IndexSet(integer: row), byExtendingSelection: false)
                tableView.scrollRowToVisible(row)
            }
        }
        updateButtonsForSelection()
        if components.isEmpty {
            statusLabel.stringValue = "No Liam Hall pedals installed."
        } else {
            statusLabel.stringValue = "\(components.count) pedal\(components.count == 1 ? "" : "s") on the board."
        }
    }

    private func updateButtonsForSelection() {
        let row = tableView?.selectedRow ?? -1
        loadButton?.isEnabled = row >= 0 && row < components.count
        unloadButton?.isEnabled = loadedUnit != nil
        showUIButton?.isEnabled = loadedUnit != nil
    }

    private func updateStatusForLoaded() {
        guard let loaded = loadedUnit else { return }
        statusLabel.stringValue = "Engaged: \(loaded.name)"
    }

    // MARK: - Actions

    @objc private func loadSelected() {
        let row = tableView.selectedRow
        guard row >= 0, row < components.count else { return }
        let component = components[row]
        statusLabel.stringValue = "Engaging \(component.name)…"
        loadButton.isEnabled = false
        AVAudioUnit.instantiate(with: component.audioComponentDescription, options: []) { [weak self] avUnit, error in
            DispatchQueue.main.async {
                guard let self = self else { return }
                if let error = error {
                    self.statusLabel.stringValue = "Couldn't engage: \(error.localizedDescription)"
                    self.loadButton.isEnabled = true
                    return
                }
                guard let avUnit = avUnit else {
                    self.statusLabel.stringValue = "Couldn't engage: pedal returned nil."
                    self.loadButton.isEnabled = true
                    return
                }
                // Tear down any open UI window — it was for the previous
                // pedal and the view controller's host AU is gone.
                self.closePluginUIWindow()
                self.loadedUnit = avUnit
                self.onLoad(avUnit)
                self.statusLabel.stringValue = "Engaged \(avUnit.name)."
                self.updateButtonsForSelection()
            }
        }
    }

    @objc private func unloadCurrent() {
        closePluginUIWindow()
        loadedUnit = nil
        onUnload()
        statusLabel.stringValue = "Disengaged — back to GM."
        updateButtonsForSelection()
    }

    @objc private func showPluginUI() {
        guard let unit = loadedUnit else { return }
        if let existing = pluginUIWindow {
            existing.makeKeyAndOrderFront(nil)
            return
        }
        // AUv3 / modern AU path. Some classic AUs return nil here; we
        // surface that as a status message rather than crashing.
        unit.auAudioUnit.requestViewController { [weak self] vc in
            DispatchQueue.main.async {
                guard let self = self else { return }
                guard let vc = vc else {
                    self.statusLabel.stringValue = "\(unit.name): no pedal face available."
                    return
                }
                self.openPluginUI(vc, title: unit.name)
            }
        }
    }

    private func openPluginUI(_ vc: NSViewController, title: String) {
        // Cap the AU's preferred size — angelsaw's view reports a huge
        // canvas that swallows the menubar. The user can still resize
        // up; this just keeps the initial open compact.
        let maxSize = NSSize(width: 520, height: 360)
        let preferred = vc.preferredContentSize
        let size: NSSize
        if preferred == .zero {
            size = NSSize(width: 420, height: 300)
        } else {
            size = NSSize(
                width:  min(preferred.width,  maxSize.width),
                height: min(preferred.height, maxSize.height)
            )
        }
        let win = NSWindow(
            contentRect: NSRect(origin: .zero, size: size),
            styleMask: [.titled, .closable, .resizable],
            backing: .buffered,
            defer: false
        )
        win.title = title
        win.contentViewController = vc
        win.isReleasedWhenClosed = false
        win.center()
        win.makeKeyAndOrderFront(nil)
        pluginUIWindow = win
        pluginUIViewController = vc
    }

    private func closePluginUIWindow() {
        pluginUIWindow?.close()
        pluginUIWindow = nil
        pluginUIViewController = nil
    }

    // MARK: - Helpers

    private func fourCharCode(_ s: String) -> OSType {
        var code: OSType = 0
        for ch in s.unicodeScalars.prefix(4) {
            code = (code << 8) | OSType(ch.value & 0xFF)
        }
        return code
    }

    private func fourCharString(_ code: OSType) -> String {
        let bytes: [UInt8] = [
            UInt8((code >> 24) & 0xFF),
            UInt8((code >> 16) & 0xFF),
            UInt8((code >>  8) & 0xFF),
            UInt8( code        & 0xFF),
        ]
        // Some manufacturers stash non-printables — render those as '?'.
        let chars = bytes.map { (b: UInt8) -> Character in
            (b >= 0x20 && b < 0x7F) ? Character(UnicodeScalar(b)) : "?"
        }
        return String(chars)
    }
}

// MARK: - NSTableView data source / delegate

extension AUPluginPickerController: NSTableViewDataSource, NSTableViewDelegate {
    func numberOfRows(in tableView: NSTableView) -> Int { components.count }

    func tableView(_ tableView: NSTableView,
                   viewFor tableColumn: NSTableColumn?,
                   row: Int) -> NSView? {
        guard row < components.count, let column = tableColumn else { return nil }
        let component = components[row]
        let identifier = NSUserInterfaceItemIdentifier("cell-\(column.identifier.rawValue)")
        let cell = tableView.makeView(withIdentifier: identifier, owner: self) as? NSTableCellView
            ?? makeCell(identifier: identifier)
        switch column.identifier.rawValue {
        case "name":
            cell.textField?.stringValue = component.name
        case "subtype":
            cell.textField?.stringValue = fourCharString(component.audioComponentDescription.componentSubType)
            cell.textField?.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)
        default:
            cell.textField?.stringValue = ""
        }
        return cell
    }

    private func makeCell(identifier: NSUserInterfaceItemIdentifier) -> NSTableCellView {
        let cell = NSTableCellView()
        cell.identifier = identifier
        let tf = NSTextField(labelWithString: "")
        tf.translatesAutoresizingMaskIntoConstraints = false
        tf.lineBreakMode = .byTruncatingTail
        tf.font = NSFont.systemFont(ofSize: 12)
        cell.addSubview(tf)
        cell.textField = tf
        NSLayoutConstraint.activate([
            tf.leadingAnchor.constraint(equalTo: cell.leadingAnchor, constant: 4),
            tf.trailingAnchor.constraint(equalTo: cell.trailingAnchor, constant: -4),
            tf.centerYAnchor.constraint(equalTo: cell.centerYAnchor),
        ])
        return cell
    }

    func tableViewSelectionDidChange(_ notification: Notification) {
        updateButtonsForSelection()
    }
}
