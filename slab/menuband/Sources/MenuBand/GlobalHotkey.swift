import AppKit
import Carbon

// Thin wrapper around Carbon's RegisterEventHotKey so we can map a
// keycode + modifier combination to a closure. Used for the system-wide
// shortcut that toggles MenuBand TYPE mode regardless of which app is
// frontmost.
final class GlobalHotkey {
    private let signature: OSType
    private let id: UInt32
    private var hotKeyRef: EventHotKeyRef?
    private var eventHandler: EventHandlerRef?
    private let onTrigger: () -> Void

    init(signature: OSType = OSType(0x4E544B59), id: UInt32 = 1, onTrigger: @escaping () -> Void) {
        self.signature = signature
        self.id = id
        self.onTrigger = onTrigger
    }

    /// Register the shortcut. `keyCode` uses Carbon `kVK_*` virtual key codes,
    /// `modifiers` uses the Carbon mask (`cmdKey | optionKey | controlKey | shiftKey`).
    @discardableResult
    func register(keyCode: UInt32, modifiers: UInt32) -> Bool {
        unregister()
        let hotKeyID = EventHotKeyID(signature: signature, id: id)
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
        let handler: EventHandlerUPP = { _, event, userData -> OSStatus in
            guard let userData = userData else { return noErr }
            let hk = Unmanaged<GlobalHotkey>.fromOpaque(userData).takeUnretainedValue()
            guard let event = event else { return OSStatus(eventNotHandledErr) }
            var incomingID = EventHotKeyID()
            let status = GetEventParameter(
                event,
                EventParamName(kEventParamDirectObject),
                EventParamType(typeEventHotKeyID),
                nil,
                MemoryLayout<EventHotKeyID>.size,
                nil,
                &incomingID
            )
            guard status == noErr else { return status }
            guard incomingID.signature == hk.signature, incomingID.id == hk.id else {
                return OSStatus(eventNotHandledErr)
            }
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
