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
        Language(code: "zh", label: "中文", flag: "🇨🇳"),
        Language(code: "ja", label: "日本語", flag: "🇯🇵"),
        Language(code: "ru", label: "Русский", flag: "🇷🇺"),
        // Danish — for the lær klokken community.
        Language(code: "da", label: "Dansk", flag: "🇩🇰"),
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
        "zh": zh,
        "ja": ja,
        "ru": ru,
        "da": da,
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
        "popover.about.lookingForPlayers": "Looking For Players?",
        "popover.about.version": "Version",
        "popover.about.quit": "Quit Menu Band",
        "popover.about.crash.send": "Send crash reports",
        "popover.about.crash.sending": "Sending…",
        "popover.about.crash.sentAll": "Sent ✓",
        "popover.about.crash.sentSome": "Sent %@/%@ — retry",
        "popover.about.crash.sendOne": "Send 1 crash",
        "popover.about.crash.sendMany": "Send %@ crashes",
        "popover.about.crash.viewOne": "View 1 crash",
        "popover.about.crash.viewMany": "View %@ crashes",
        "popover.about.crash.summaryOne": "Menu Band crashed 1 time",
        "popover.about.crash.summaryMany": "Menu Band crashed %@ times",
        "popover.about.crash.viewerTitle": "Crash reports",
        "popover.about.crash.sendToAC": "Send to Aesthetic.Computer",
        "popover.about.nela": "Say hi at NELA Computer Club — Tuesdays in LA",
        "popover.about.startaclub": "Start a computer club of your own",

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
        "popover.about.lookingForPlayers": "¿Buscas músicos?",
        "popover.about.version": "Versión",
        "popover.about.quit": "Salir de Menu Band",
        "popover.about.crash.send": "Enviar informes de fallos",
        "popover.about.crash.sending": "Enviando…",
        "popover.about.crash.sentAll": "Enviado ✓",
        "popover.about.crash.sentSome": "Enviados %@/%@ — reintentar",
        "popover.about.crash.sendOne": "Enviar 1 fallo",
        "popover.about.crash.sendMany": "Enviar %@ fallos",
        "popover.about.crash.viewOne": "Ver 1 fallo",
        "popover.about.crash.viewMany": "Ver %@ fallos",
        "popover.about.crash.summaryOne": "Menu Band falló 1 vez",
        "popover.about.crash.summaryMany": "Menu Band falló %@ veces",
        "popover.about.crash.viewerTitle": "Informes de fallos",
        "popover.about.crash.sendToAC": "Enviar a Aesthetic.Computer",
        "popover.about.nela": "Saluda en NELA Computer Club — los martes en LA",
        "popover.about.startaclub": "Empieza tu propio club de computación",

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

    private static let zh: [String: String] = [
        // Popover — header / banner
        "popover.octave": "八度",
        "popover.octave.down": "降低八度",
        "popover.octave.up": "升高八度",
        "popover.midi.label": "MIDI",
        "popover.update.available": "可用更新:%@",

        // Popover — layout block
        "popover.layout.label": "键位",
        "popover.layout.notepat": "Notepat.com",
        "popover.layout.ableton": "Ableton 计算机键盘",
        "popover.layout.hint": "⌃⌥⌘P 切换最近按键模式",
        "popover.layout.why": "为什么是这个键位?",
        "popover.layout.why.tooltip": "打开《作为社交软件的键位》论文",

        // Popover — shortcut rows
        "popover.shortcuts.label": "键盘快捷键",
        "popover.shortcuts.focus": "聚焦菜单钢琴",
        "popover.shortcuts.floating": "浮动钢琴",
        "popover.shortcuts.show": "显示",
        "popover.shortcuts.focusButton": "聚焦",
        "popover.shortcuts.press": "按下",
        "popover.shortcuts.pressKeys": "按下按键",
        "popover.shortcuts.use": "使用 ⌘、⌃ 或 ⌥",
        "popover.shortcuts.saved": "已保存 %@",
        "popover.shortcuts.unavailable": "快捷键不可用",
        "popover.shortcuts.reserved": "⌃⌥⌘P 已被保留",

        // Popover — palette helpers
        "popover.arrows.tooltip": "方向键移动选择。",

        // Popover — about / footer
        "popover.about.lead": "Menu Band",
        "popover.about.body": " 让 macOS 内置的 MIDI 乐器可以直接从菜单栏弹奏。",
        "popover.about.link": "关于",
        "popover.about.lookingForPlayers": "寻找乐手?",
        "popover.about.version": "版本",
        "popover.about.quit": "退出 Menu Band",
        "popover.about.crash.send": "发送崩溃报告",
        "popover.about.crash.sending": "发送中…",
        "popover.about.crash.sentAll": "已发送 ✓",
        "popover.about.crash.sentSome": "已发送 %@/%@ — 重试",
        "popover.about.crash.sendOne": "发送 1 个崩溃",
        "popover.about.crash.sendMany": "发送 %@ 个崩溃",
        "popover.about.crash.viewOne": "查看 1 个崩溃",
        "popover.about.crash.viewMany": "查看 %@ 个崩溃",
        "popover.about.crash.summaryOne": "Menu Band 崩溃了 1 次",
        "popover.about.crash.summaryMany": "Menu Band 崩溃了 %@ 次",
        "popover.about.crash.viewerTitle": "崩溃报告",
        "popover.about.crash.sendToAC": "发送至 Aesthetic.Computer",
        "popover.about.nela": "周二来 NELA 电脑俱乐部打个招呼 — 洛杉矶",
        "popover.about.startaclub": "组建你自己的电脑俱乐部",

        // Popover — language switcher
        "popover.language.label": "语言",

        // Alerts
        "alert.noMenuBarSpace.title": "Menu Band 无法放入菜单栏",
        "alert.noMenuBarSpace.body":
            "菜单栏没有空间——连紧凑图标也放不下。请退出占用菜单栏的应用 " +
            "(slack、dropbox 等),或使用 Bartender / Hidden Bar 管理它们。" +
            "\n\nMenu Band 会每隔几秒重试。",
        "alert.ok": "好",
    ]

    private static let ja: [String: String] = [
        // Popover — header / banner
        "popover.octave": "オクターブ",
        "popover.octave.down": "オクターブダウン",
        "popover.octave.up": "オクターブアップ",
        "popover.midi.label": "MIDI",
        "popover.update.available": "アップデートあり:%@",

        // Popover — layout block
        "popover.layout.label": "キーマップ",
        "popover.layout.notepat": "Notepat.com",
        "popover.layout.ableton": "Ableton コンピュータキーボード",
        "popover.layout.hint": "⌃⌥⌘P で最後の入力モードを切替",
        "popover.layout.why": "このキーマップの理由は?",
        "popover.layout.why.tooltip": "「Keymaps as Social Software」を開く",

        // Popover — shortcut rows
        "popover.shortcuts.label": "キーボードショートカット",
        "popover.shortcuts.focus": "メニューピアノにフォーカス",
        "popover.shortcuts.floating": "フローティングピアノ",
        "popover.shortcuts.show": "表示",
        "popover.shortcuts.focusButton": "フォーカス",
        "popover.shortcuts.press": "押す",
        "popover.shortcuts.pressKeys": "キーを押す",
        "popover.shortcuts.use": "⌘、⌃、⌥ を使う",
        "popover.shortcuts.saved": "保存しました %@",
        "popover.shortcuts.unavailable": "ショートカット利用不可",
        "popover.shortcuts.reserved": "⌃⌥⌘P は予約済み",

        // Popover — palette helpers
        "popover.arrows.tooltip": "矢印キーで選択を移動。",

        // Popover — about / footer
        "popover.about.lead": "Menu Band",
        "popover.about.body": " は macOS 内蔵の MIDI 楽器をメニューバーから直接演奏できます。",
        "popover.about.link": "概要",
        "popover.about.lookingForPlayers": "仲間を募集中?",
        "popover.about.version": "バージョン",
        "popover.about.quit": "Menu Band を終了",
        "popover.about.crash.send": "クラッシュレポートを送信",
        "popover.about.crash.sending": "送信中…",
        "popover.about.crash.sentAll": "送信済み ✓",
        "popover.about.crash.sentSome": "%@/%@ 件送信 — 再試行",
        "popover.about.crash.sendOne": "1 件のクラッシュを送信",
        "popover.about.crash.sendMany": "%@ 件のクラッシュを送信",
        "popover.about.crash.viewOne": "1 件のクラッシュを表示",
        "popover.about.crash.viewMany": "%@ 件のクラッシュを表示",
        "popover.about.crash.summaryOne": "Menu Band は 1 回クラッシュしました",
        "popover.about.crash.summaryMany": "Menu Band は %@ 回クラッシュしました",
        "popover.about.crash.viewerTitle": "クラッシュレポート",
        "popover.about.crash.sendToAC": "Aesthetic.Computer に送信",
        "popover.about.nela": "火曜は NELA Computer Club へ — ロサンゼルス",
        "popover.about.startaclub": "自分のコンピュータークラブを始めよう",

        // Popover — language switcher
        "popover.language.label": "言語",

        // Alerts
        "alert.noMenuBarSpace.title": "Menu Band がメニューバーに入りません",
        "alert.noMenuBarSpace.body":
            "メニューバーに空きがありません — コンパクトアイコンも入りません。 " +
            "slack や dropbox などメニューバーを占有しているアプリを終了するか、 " +
            "Bartender / Hidden Bar で整理してください。" +
            "\n\nMenu Band は数秒ごとに再試行します。",
        "alert.ok": "OK",
    ]

    private static let ru: [String: String] = [
        // Popover — header / banner
        "popover.octave": "Октава",
        "popover.octave.down": "На октаву ниже",
        "popover.octave.up": "На октаву выше",
        "popover.midi.label": "MIDI",
        "popover.update.available": "Доступно обновление: %@",

        // Popover — layout block
        "popover.layout.label": "Раскладка",
        "popover.layout.notepat": "Notepat.com",
        "popover.layout.ableton": "Клавиатура Ableton",
        "popover.layout.hint": "⌃⌥⌘P переключает режим последних нажатий",
        "popover.layout.why": "Почему такая раскладка?",
        "popover.layout.why.tooltip":
            "Открыть статью «Keymaps as Social Software»",

        // Popover — shortcut rows
        "popover.shortcuts.label": "Сочетания клавиш",
        "popover.shortcuts.focus": "Фокус на пианино меню",
        "popover.shortcuts.floating": "Плавающее пианино",
        "popover.shortcuts.show": "Показать",
        "popover.shortcuts.focusButton": "Фокус",
        "popover.shortcuts.press": "Нажать",
        "popover.shortcuts.pressKeys": "Нажмите клавиши",
        "popover.shortcuts.use": "Используйте ⌘, ⌃ или ⌥",
        "popover.shortcuts.saved": "Сохранено %@",
        "popover.shortcuts.unavailable": "Сочетание недоступно",
        "popover.shortcuts.reserved": "⌃⌥⌘P зарезервировано",

        // Popover — palette helpers
        "popover.arrows.tooltip": "Стрелки перемещают выбор.",

        // Popover — about / footer
        "popover.about.lead": "Menu Band",
        "popover.about.body":
            " даёт играть на встроенных MIDI-инструментах macOS прямо из строки меню.",
        "popover.about.link": "О программе",
        "popover.about.lookingForPlayers": "Ищете напарников?",
        "popover.about.version": "Версия",
        "popover.about.quit": "Выйти из Menu Band",
        "popover.about.crash.send": "Отправить отчёты о сбоях",
        "popover.about.crash.sending": "Отправка…",
        "popover.about.crash.sentAll": "Отправлено ✓",
        "popover.about.crash.sentSome": "Отправлено %@/%@ — повторить",
        "popover.about.crash.sendOne": "Отправить 1 отчёт",
        "popover.about.crash.sendMany": "Отправить %@ отчётов",
        "popover.about.crash.viewOne": "Просмотреть 1 отчёт",
        "popover.about.crash.viewMany": "Просмотреть %@ отчётов",
        "popover.about.crash.summaryOne": "Menu Band падал 1 раз",
        "popover.about.crash.summaryMany": "Menu Band падал %@ раз",
        "popover.about.crash.viewerTitle": "Отчёты о сбоях",
        "popover.about.crash.sendToAC": "Отправить на Aesthetic.Computer",
        "popover.about.nela": "Заходите в NELA Computer Club — по вторникам в Лос-Анджелесе",
        "popover.about.startaclub": "Создайте свой компьютерный клуб",

        // Popover — language switcher
        "popover.language.label": "Язык",

        // Alerts
        "alert.noMenuBarSpace.title":
            "Menu Band не помещается в строке меню",
        "alert.noMenuBarSpace.body":
            "В строке меню нет места — даже для компактного значка. " +
            "Закройте приложение, занимающее строку меню (slack, dropbox " +
            "и т. п.), или используйте Bartender / Hidden Bar для управления." +
            "\n\nMenu Band продолжит попытки каждые несколько секунд.",
        "alert.ok": "ОК",
    ]

    private static let da: [String: String] = [
        // Popover — header / banner
        "popover.octave": "Oktav",
        "popover.octave.down": "Oktav ned",
        "popover.octave.up": "Oktav op",
        "popover.midi.label": "MIDI",
        "popover.update.available": "Opdatering tilgængelig: %@",

        // Popover — layout block
        "popover.layout.label": "Tangentkort",
        "popover.layout.notepat": "Notepat.com",
        "popover.layout.ableton": "Ableton-computertastatur",
        "popover.layout.hint": "⌃⌥⌘P skifter seneste-tast-tilstand",
        "popover.layout.why": "Hvorfor dette tangentkort?",
        "popover.layout.why.tooltip": "Åbn artiklen Keymaps as Social Software",

        // Popover — shortcut rows
        "popover.shortcuts.label": "Tastaturgenveje",
        "popover.shortcuts.focus": "Fokusér menupiano",
        "popover.shortcuts.floating": "Svævende piano",
        "popover.shortcuts.show": "Vis",
        "popover.shortcuts.focusButton": "Fokusér",
        "popover.shortcuts.press": "Tryk",
        "popover.shortcuts.pressKeys": "Tryk på taster",
        "popover.shortcuts.use": "Brug ⌘, ⌃ eller ⌥",
        "popover.shortcuts.saved": "Gemt %@",
        "popover.shortcuts.unavailable": "Genvej utilgængelig",
        "popover.shortcuts.reserved": "⌃⌥⌘P er reserveret",

        // Popover — palette helpers
        "popover.arrows.tooltip": "Piletaster flytter markeringen.",

        // Popover — about / footer
        "popover.about.lead": "Menu Band",
        "popover.about.body":
            " gør macOS' indbyggede MIDI-instrumenter spilbare direkte fra menulinjen.",
        "popover.about.link": "Om",
        "popover.about.lookingForPlayers": "Leder du efter medspillere?",
        "popover.about.version": "Version",
        "popover.about.quit": "Afslut Menu Band",
        "popover.about.crash.send": "Send nedbrudsrapporter",
        "popover.about.crash.sending": "Sender…",
        "popover.about.crash.sentAll": "Sendt ✓",
        "popover.about.crash.sentSome": "Sendt %@/%@ — prøv igen",
        "popover.about.crash.sendOne": "Send 1 nedbrud",
        "popover.about.crash.sendMany": "Send %@ nedbrud",
        "popover.about.crash.viewOne": "Vis 1 nedbrud",
        "popover.about.crash.viewMany": "Vis %@ nedbrud",
        "popover.about.crash.summaryOne": "Menu Band gik ned 1 gang",
        "popover.about.crash.summaryMany": "Menu Band gik ned %@ gange",
        "popover.about.crash.viewerTitle": "Nedbrudsrapporter",
        "popover.about.crash.sendToAC": "Send til Aesthetic.Computer",
        "popover.about.nela": "Sig hej i NELA Computer Club — tirsdage i LA",
        "popover.about.startaclub": "Start din egen computerklub",

        // Popover — language switcher
        "popover.language.label": "Sprog",

        // Alerts
        "alert.noMenuBarSpace.title":
            "Menu Band kan ikke være i din menulinje",
        "alert.noMenuBarSpace.body":
            "Der er ikke plads i menulinjen — ikke engang til det kompakte ikon. " +
            "Afslut en app, der fylder i menulinjen (slack, dropbox osv.), " +
            "eller brug Bartender / Hidden Bar til at styre dem." +
            "\n\nMenu Band prøver igen hvert par sekunder.",
        "alert.ok": "OK",
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
