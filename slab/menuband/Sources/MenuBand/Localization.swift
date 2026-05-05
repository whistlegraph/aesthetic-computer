import Foundation
import AppKit

// Plug-and-play interface localization for Menu Band.
//
// Strings used in the popover, menus, alerts, and any other user-visible
// surface should pull through `L("key")` rather than hardcoding.  Adding a
// new language is a matter of dropping another dictionary into `tables` —
// any unknown key falls back to English, then to the literal key, so a
// half-translated language never produces a blank string.
enum Localization {

    // MARK: - Languages

    struct Language {
        let code: String
        let label: String   // native-language label for the picker
        let flag: String    // emoji flag for the picker chip
    }

    static let supported: [Language] = [
        Language(code: "en", label: "English", flag: "🇺🇸"),
        Language(code: "es", label: "Español", flag: "🇪🇸"),
    ]

    static let didChange = Notification.Name("MenuBandLanguageDidChange")
    private static let userDefaultsKey = "menuband.language"

    static var current: String {
        get {
            UserDefaults.standard.string(forKey: userDefaultsKey)
                ?? defaultLanguage()
        }
        set {
            guard supported.contains(where: { $0.code == newValue }) else { return }
            UserDefaults.standard.set(newValue, forKey: userDefaultsKey)
            NotificationCenter.default.post(name: didChange, object: nil)
        }
    }

    static func language(for code: String) -> Language {
        supported.first(where: { $0.code == code }) ?? supported[0]
    }

    /// Best-effort match of the OS preferred language to one of our supported
    /// codes. Falls back to English when the user's locale isn't represented.
    private static func defaultLanguage() -> String {
        let pref = (Locale.preferredLanguages.first ?? "en").lowercased()
        for lang in supported where pref.hasPrefix(lang.code) {
            return lang.code
        }
        return "en"
    }

    // MARK: - Lookup

    static func t(_ key: String) -> String {
        if let s = tables[current]?[key] { return s }
        if let s = tables["en"]?[key] { return s }
        return key
    }

    /// Format with positional `%@` substitutions, English-fallback aware.
    static func t(_ key: String, _ args: CVarArg...) -> String {
        let format = tables[current]?[key] ?? tables["en"]?[key] ?? key
        return String(format: format, arguments: args)
    }

    // MARK: - Tables
    //
    // Keep the keys in dotted-namespace form so callers self-document
    // (`popover.layout.label` vs. an ambiguous `layoutLabel`).  When adding
    // a new key, add it to `en` first — the fallback path uses English, so a
    // missing translation in another language won't break the UI.
    static let tables: [String: [String: String]] = [
        "en": en,
        "es": es,
    ]

    private static let en: [String: String] = [
        // Popover — header / banner
        "popover.octave": "Octave",
        "popover.octave.down": "Octave down",
        "popover.octave.up": "Octave up",
        "popover.midi.label": "MIDI",
        "popover.update.available": "Update available: %@",

        // Popover — layout block
        "popover.layout.label": "Keymap",
        "popover.layout.notepat": "Notepat.com",
        "popover.layout.ableton": "Ableton Computer Keyboard",
        "popover.layout.hint": "⌃⌥⌘P toggles last keystrokes mode",
        "popover.layout.why": "Why this Keymap?",
        "popover.layout.why.tooltip": "Open the Keymaps as Social Software paper",

        // Popover — shortcut rows
        "popover.shortcuts.label": "Key Shortcuts",
        "popover.shortcuts.focus": "Focus menu piano",
        "popover.shortcuts.floating": "Floating piano",
        "popover.shortcuts.show": "Show",
        "popover.shortcuts.focusButton": "Focus",
        "popover.shortcuts.press": "Press",
        "popover.shortcuts.pressKeys": "Press keys",
        "popover.shortcuts.use": "Use ⌘, ⌃, or ⌥",
        "popover.shortcuts.saved": "Saved %@",
        "popover.shortcuts.unavailable": "Shortcut unavailable",
        "popover.shortcuts.reserved": "⌃⌥⌘P is reserved",

        // Popover — palette helpers
        "popover.arrows.tooltip": "Arrow keys move the selection.",

        // Popover — about / footer
        "popover.about.lead": "Menu Band",
        "popover.about.body": " makes the built-in macOS MIDI instruments playable right from the menu bar.",
        "popover.about.link": "About",
        "popover.about.quit": "Quit Menu Band",
        "popover.about.crash.send": "Send crash reports",
        "popover.about.crash.sending": "Sending…",
        "popover.about.crash.sentAll": "Sent ✓",
        "popover.about.crash.sentSome": "Sent %@/%@ — retry",
        "popover.about.crash.sendOne": "Send 1 crash",
        "popover.about.crash.sendMany": "Send %@ crashes",

        // Popover — language switcher
        "popover.language.label": "Language",

        // Alerts
        "alert.noMenuBarSpace.title": "Menu Band can't fit in your menu bar",
        "alert.noMenuBarSpace.body":
            "There's no room in your menu bar — even for the compact icon. " +
            "Try quitting an app that puts items in the menu bar (slack, " +
            "dropbox, etc.), or use Bartender / Hidden Bar to manage them." +
            "\n\nMenu Band will keep trying every few seconds.",
        "alert.ok": "OK",
    ]

    private static let es: [String: String] = [
        // Popover — header / banner
        "popover.octave": "Octava",
        "popover.octave.down": "Bajar octava",
        "popover.octave.up": "Subir octava",
        "popover.midi.label": "MIDI",
        "popover.update.available": "Actualización disponible: %@",

        // Popover — layout block
        "popover.layout.label": "Mapa",
        "popover.layout.notepat": "Notepat.com",
        "popover.layout.ableton": "Teclado Ableton",
        "popover.layout.hint": "⌃⌥⌘P alterna el modo de captura",
        "popover.layout.why": "¿Por qué este teclado?",
        "popover.layout.why.tooltip": "Abrir el artículo Keymaps as Social Software",

        // Popover — shortcut rows
        "popover.shortcuts.label": "Atajos de teclado",
        "popover.shortcuts.focus": "Enfocar piano del menú",
        "popover.shortcuts.floating": "Piano flotante",
        "popover.shortcuts.show": "Mostrar",
        "popover.shortcuts.focusButton": "Enfocar",
        "popover.shortcuts.press": "Pulsar",
        "popover.shortcuts.pressKeys": "Pulsa teclas",
        "popover.shortcuts.use": "Usa ⌘, ⌃ o ⌥",
        "popover.shortcuts.saved": "Guardado %@",
        "popover.shortcuts.unavailable": "Atajo no disponible",
        "popover.shortcuts.reserved": "⌃⌥⌘P está reservado",

        // Popover — palette helpers
        "popover.arrows.tooltip": "Las flechas mueven la selección.",

        // Popover — about / footer
        "popover.about.lead": "Menu Band",
        "popover.about.body":
            " hace tocables los instrumentos MIDI integrados de macOS directamente desde la barra de menús.",
        "popover.about.link": "Acerca de",
        "popover.about.quit": "Salir de Menu Band",
        "popover.about.crash.send": "Enviar informes de fallos",
        "popover.about.crash.sending": "Enviando…",
        "popover.about.crash.sentAll": "Enviado ✓",
        "popover.about.crash.sentSome": "Enviados %@/%@ — reintentar",
        "popover.about.crash.sendOne": "Enviar 1 fallo",
        "popover.about.crash.sendMany": "Enviar %@ fallos",

        // Popover — language switcher
        "popover.language.label": "Idioma",

        // Alerts
        "alert.noMenuBarSpace.title":
            "Menu Band no cabe en tu barra de menús",
        "alert.noMenuBarSpace.body":
            "No hay espacio en la barra de menús — ni siquiera para el icono " +
            "compacto. Cierra una app que ocupe la barra (slack, dropbox, " +
            "etc.), o usa Bartender / Hidden Bar para gestionarlas." +
            "\n\nMenu Band seguirá intentándolo cada pocos segundos.",
        "alert.ok": "Aceptar",
    ]
}

/// Shorthand for `Localization.t(key)`.
func L(_ key: String) -> String { Localization.t(key) }

/// Shorthand for `Localization.t(key, args…)` — positional `%@` formatter.
func L(_ key: String, _ args: CVarArg...) -> String {
    let format = Localization.tables[Localization.current]?[key]
        ?? Localization.tables["en"]?[key]
        ?? key
    return String(format: format, arguments: args)
}
