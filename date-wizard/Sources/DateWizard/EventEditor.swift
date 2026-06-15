// EventEditor.swift — a native sheet for creating / editing an aesthetical
// event. Fields: Title, Start, End, Note, and a private/public control,
// with Add / Save / Delete buttons. Used ONLY for aesthetical (editable)
// events — feed events are read-only.
import AppKit

// The result the editor hands back to its owner.
enum EventEditorResult {
    case save(uid: String?, title: String, start: Date, end: Date,
              note: String, visibility: String)
    case delete(uid: String)
    case cancel
}

final class EventEditor: NSWindowController {

    // nil uid → "Add" mode; non-nil → "Edit" mode (Save + Delete shown).
    private let existingUID: String?
    private let onDone: (EventEditorResult) -> Void

    private let titleField = NSTextField()
    private let startPicker = NSDatePicker()
    private let endPicker = NSDatePicker()
    private let noteField = NSTextField()
    private let visControl = NSSegmentedControl(
        labels: ["private", "public"], trackingMode: .selectOne, target: nil, action: nil)

    init(uid: String?, title: String, start: Date, end: Date,
         note: String, visibility: String,
         onDone: @escaping (EventEditorResult) -> Void) {
        self.existingUID = uid
        self.onDone = onDone

        let panel = NSPanel(
            contentRect: NSRect(x: 0, y: 0, width: 360, height: 280),
            styleMask: [.titled],
            backing: .buffered, defer: false)
        panel.title = uid == nil ? "New Event" : "Edit Event"
        super.init(window: panel)

        buildUI(title: title, start: start, end: end, note: note, visibility: visibility)
    }

    required init?(coder: NSCoder) { fatalError() }

    private func buildUI(title: String, start: Date, end: Date,
                         note: String, visibility: String) {
        guard let cv = window?.contentView else { return }
        let pad: CGFloat = 16
        let labelW: CGFloat = 60
        let fieldX = pad + labelW + 8
        let fieldW: CGFloat = 360 - fieldX - pad
        var y: CGFloat = 280 - 40

        func addLabel(_ text: String, at yy: CGFloat) {
            let l = NSTextField(labelWithString: text)
            l.frame = NSRect(x: pad, y: yy, width: labelW, height: 20)
            l.alignment = .right
            l.font = .systemFont(ofSize: 12)
            l.textColor = .secondaryLabelColor
            cv.addSubview(l)
        }

        // Title
        addLabel("Title", at: y)
        titleField.frame = NSRect(x: fieldX, y: y - 2, width: fieldW, height: 24)
        titleField.stringValue = title
        titleField.placeholderString = "Event title"
        cv.addSubview(titleField)
        y -= 38

        // Start
        addLabel("Start", at: y)
        startPicker.frame = NSRect(x: fieldX, y: y - 2, width: fieldW, height: 24)
        startPicker.datePickerStyle = .textFieldAndStepper
        startPicker.datePickerElements = [.yearMonthDay, .hourMinute]
        startPicker.dateValue = start
        cv.addSubview(startPicker)
        y -= 38

        // End
        addLabel("End", at: y)
        endPicker.frame = NSRect(x: fieldX, y: y - 2, width: fieldW, height: 24)
        endPicker.datePickerStyle = .textFieldAndStepper
        endPicker.datePickerElements = [.yearMonthDay, .hourMinute]
        endPicker.dateValue = end
        cv.addSubview(endPicker)
        y -= 38

        // Note
        addLabel("Note", at: y)
        noteField.frame = NSRect(x: fieldX, y: y - 2, width: fieldW, height: 24)
        noteField.stringValue = note
        noteField.placeholderString = "Optional note"
        cv.addSubview(noteField)
        y -= 38

        // Visibility
        addLabel("Privacy", at: y)
        visControl.frame = NSRect(x: fieldX, y: y - 2, width: fieldW, height: 24)
        visControl.selectedSegment = (visibility == "public") ? 1 : 0
        cv.addSubview(visControl)
        y -= 44

        // Buttons row.
        let btnH: CGFloat = 28
        let btnY: CGFloat = 14
        let cancel = NSButton(title: "Cancel", target: self, action: #selector(cancelAction))
        cancel.bezelStyle = .rounded
        cancel.frame = NSRect(x: pad, y: btnY, width: 80, height: btnH)
        cv.addSubview(cancel)

        if existingUID != nil {
            let del = NSButton(title: "Delete", target: self, action: #selector(deleteAction))
            del.bezelStyle = .rounded
            del.frame = NSRect(x: pad + 88, y: btnY, width: 80, height: btnH)
            del.contentTintColor = .systemRed
            cv.addSubview(del)
        }

        let save = NSButton(title: existingUID == nil ? "Add" : "Save",
                            target: self, action: #selector(saveAction))
        save.bezelStyle = .rounded
        save.keyEquivalent = "\r"
        save.frame = NSRect(x: 360 - pad - 90, y: btnY, width: 90, height: btnH)
        cv.addSubview(save)
    }

    // ── actions ──────────────────────────────────────────────────────

    @objc private func saveAction() {
        let title = titleField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !title.isEmpty else {
            NSSound.beep()
            return
        }
        var end = endPicker.dateValue
        // Guard against a non-positive duration.
        if end <= startPicker.dateValue {
            end = startPicker.dateValue.addingTimeInterval(3600)
        }
        let vis = visControl.selectedSegment == 1 ? "public" : "private"
        dismiss()
        onDone(.save(uid: existingUID, title: title,
                     start: startPicker.dateValue, end: end,
                     note: noteField.stringValue, visibility: vis))
    }

    @objc private func deleteAction() {
        guard let uid = existingUID else { return }
        dismiss()
        onDone(.delete(uid: uid))
    }

    @objc private func cancelAction() {
        dismiss()
        onDone(.cancel)
    }

    // The window we're presented on as a sheet (so we can end it cleanly).
    private weak var parentWindow: NSWindow?

    // Present as a sheet on the given window.
    func present(on parent: NSWindow) {
        guard let w = window else { return }
        parentWindow = parent
        parent.beginSheet(w) { _ in }
    }

    private func dismiss() {
        guard let w = window else { return }
        if let parent = parentWindow {
            parent.endSheet(w)
        } else {
            close()
        }
    }
}
