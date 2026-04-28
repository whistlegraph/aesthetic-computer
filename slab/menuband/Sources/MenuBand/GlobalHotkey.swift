import AppKit
import Carbon

// Thin wrapper around Carbon's RegisterEventHotKey so we can map a
// keycode + modifier combination to a closure. Used for the system-wide
// shortcut that toggles MenuBand TYPE mode regardless of which app is
// frontmost.
final class GlobalHotkey {
    private var hotKeyRef: EventHotKeyRef?
    private var eventHandler: EventHandlerRef?
    private let onTrigger: () -> Void

    init(onTrigger: @escaping () -> Void) {
        self.onTrigger = onTrigger
    }

    /// Register the shortcut. `keyCode` uses Carbon `kVK_*` virtual key codes,
    /// `modifiers` uses the Carbon mask (`cmdKey | optionKey | controlKey | shiftKey`).
    @discardableResult
    func register(keyCode: UInt32, modifiers: UInt32) -> Bool {
        unregister()
        let hotKeyID = EventHotKeyID(signature: OSType(0x4E544B59),  // 'NTKY' (MenuBand key)
                                     id: 1)
        var ref: EventHotKeyRef?
        let regOK = RegisterEventHotKey(keyCode, modifiers, hotKeyID,
                                        GetApplicationEventTarget(), 0, &ref)
        guard regOK == noErr, let ref = ref else {
            NSLog("MenuBand hotkey registration failed: \(regOK)")
            return false
        }
        hotKeyRef = ref

        var spec = EventTypeSpec(eventClass: OSType(kEventClassKeyboard),
                                 eventKind: UInt32(kEventHotKeyPressed))
        let opaque = Unmanaged.passUnretained(self).toOpaque()
        let handler: EventHandlerUPP = { _, _, userData -> OSStatus in
            guard let userData = userData else { return noErr }
            let hk = Unmanaged<GlobalHotkey>.fromOpaque(userData).takeUnretainedValue()
            DispatchQueue.main.async { hk.onTrigger() }
            return noErr
        }
        let installOK = InstallEventHandler(GetApplicationEventTarget(), handler,
                                            1, &spec, opaque, &eventHandler)
        if installOK != noErr {
            NSLog("MenuBand hotkey event handler install failed: \(installOK)")
            unregister()
            return false
        }
        return true
    }

    func unregister() {
        if let ref = hotKeyRef { UnregisterEventHotKey(ref) }
        hotKeyRef = nil
        if let h = eventHandler { RemoveEventHandler(h) }
        eventHandler = nil
    }

    deinit { unregister() }
}
