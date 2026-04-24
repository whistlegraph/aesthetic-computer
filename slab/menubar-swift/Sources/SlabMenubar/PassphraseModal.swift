import AppKit

enum PassphraseModal {
    static func prompt(label: String) -> String? {
        assert(Thread.isMainThread, "PassphraseModal must run on the main thread")

        let alert = NSAlert()
        alert.messageText = "Passphrase required"
        alert.informativeText = "slab needs your passphrase for “\(label)”."
        alert.alertStyle = .informational
        alert.addButton(withTitle: "Unlock")
        alert.addButton(withTitle: "Cancel")

        let field = NSSecureTextField(frame: NSRect(x: 0, y: 0, width: 280, height: 24))
        field.placeholderString = "passphrase"
        alert.accessoryView = field

        NSApp.activate(ignoringOtherApps: true)
        alert.window.initialFirstResponder = field

        let response = alert.runModal()
        guard response == .alertFirstButtonReturn else { return nil }
        let value = field.stringValue
        return value.isEmpty ? nil : value
    }
}
