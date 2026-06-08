import Foundation

// General MIDI program names + family taxonomy. Used by the menubar instrument
// picker. Program order matches MenuBandSynth.setMelodicProgram(_:) (bankMSB
// 0x79 in Apple's gs_instruments.dls).
enum GeneralMIDI {
    static let programNames: [String] = [
        "Acoustic Grand Piano", "Bright Acoustic Piano", "Electric Grand Piano", "Honky-tonk Piano",
        "Electric Piano 1", "Electric Piano 2", "Harpsichord", "Clavinet",
        "Celesta", "Glockenspiel", "Music Box", "Vibraphone",
        "Marimba", "Xylophone", "Tubular Bells", "Dulcimer",
        "Drawbar Organ", "Percussive Organ", "Rock Organ", "Church Organ",
        "Reed Organ", "Accordion", "Harmonica", "Tango Accordion",
        "Acoustic Guitar (nylon)", "Acoustic Guitar (steel)", "Electric Guitar (jazz)", "Electric Guitar (clean)",
        "Electric Guitar (muted)", "Overdriven Guitar", "Distortion Guitar", "Guitar Harmonics",
        "Acoustic Bass", "Electric Bass (finger)", "Electric Bass (pick)", "Fretless Bass",
        "Slap Bass 1", "Slap Bass 2", "Synth Bass 1", "Synth Bass 2",
        "Violin", "Viola", "Cello", "Contrabass",
        "Tremolo Strings", "Pizzicato Strings", "Orchestral Harp", "Timpani",
        "String Ensemble 1", "String Ensemble 2", "Synth Strings 1", "Synth Strings 2",
        "Choir Aahs", "Voice Oohs", "Synth Choir", "Orchestra Hit",
        "Trumpet", "Trombone", "Tuba", "Muted Trumpet",
        "French Horn", "Brass Section", "Synth Brass 1", "Synth Brass 2",
        "Soprano Sax", "Alto Sax", "Tenor Sax", "Baritone Sax",
        "Oboe", "English Horn", "Bassoon", "Clarinet",
        "Piccolo", "Flute", "Recorder", "Pan Flute",
        "Blown Bottle", "Shakuhachi", "Whistle", "Ocarina",
        "Lead 1 (square)", "Lead 2 (sawtooth)", "Lead 3 (calliope)", "Lead 4 (chiff)",
        "Lead 5 (charang)", "Lead 6 (voice)", "Lead 7 (fifths)", "Lead 8 (bass + lead)",
        "Pad 1 (new age)", "Pad 2 (warm)", "Pad 3 (polysynth)", "Pad 4 (choir)",
        "Pad 5 (bowed)", "Pad 6 (metallic)", "Pad 7 (halo)", "Pad 8 (sweep)",
        "FX 1 (rain)", "FX 2 (soundtrack)", "FX 3 (crystal)", "FX 4 (atmosphere)",
        "FX 5 (brightness)", "FX 6 (goblins)", "FX 7 (echoes)", "FX 8 (sci-fi)",
        "Sitar", "Banjo", "Shamisen", "Koto",
        "Kalimba", "Bagpipe", "Fiddle", "Shanai",
        "Tinkle Bell", "Agogo", "Steel Drums", "Woodblock",
        "Taiko Drum", "Melodic Tom", "Synth Drum", "Reverse Cymbal",
        "Guitar Fret Noise", "Breath Noise", "Seashore", "Bird Tweet",
        "Telephone Ring", "Helicopter", "Applause", "Gunshot",
    ]

    /// Per-language GM program names, same 0–127 order as `programNames`.
    /// English IS `programNames`; any language/index not covered here falls
    /// back to English (see `programName`), so a partial table never blanks.
    /// Parenthetical qualifiers are localized; proper-noun instruments
    /// (Marimba, Sitar, Koto, Shamisen, Banjo, Kalimba, Shakuhachi…) keep
    /// their conventional names in each language.
    static let localizedProgramNames: [String: [String]] = [
        "es": [
            "Piano de cola acústico", "Piano acústico brillante", "Piano de cola eléctrico", "Piano honky-tonk",
            "Piano eléctrico 1", "Piano eléctrico 2", "Clavecín", "Clavinet",
            "Celesta", "Glockenspiel", "Caja de música", "Vibráfono",
            "Marimba", "Xilófono", "Campanas tubulares", "Dulcémele",
            "Órgano de barras", "Órgano percusivo", "Órgano de rock", "Órgano de iglesia",
            "Armonio", "Acordeón", "Armónica", "Acordeón de tango",
            "Guitarra acústica (nylon)", "Guitarra acústica (acero)", "Guitarra eléctrica (jazz)", "Guitarra eléctrica (limpia)",
            "Guitarra eléctrica (apagada)", "Guitarra saturada", "Guitarra distorsionada", "Armónicos de guitarra",
            "Bajo acústico", "Bajo eléctrico (dedos)", "Bajo eléctrico (púa)", "Bajo sin trastes",
            "Bajo slap 1", "Bajo slap 2", "Bajo sintetizado 1", "Bajo sintetizado 2",
            "Violín", "Viola", "Violonchelo", "Contrabajo",
            "Cuerdas con trémolo", "Cuerdas en pizzicato", "Arpa orquestal", "Timbales",
            "Conjunto de cuerdas 1", "Conjunto de cuerdas 2", "Cuerdas sintetizadas 1", "Cuerdas sintetizadas 2",
            "Coro aah", "Voces ooh", "Coro sintetizado", "Golpe orquestal",
            "Trompeta", "Trombón", "Tuba", "Trompeta con sordina",
            "Trompa", "Sección de metales", "Metales sintetizados 1", "Metales sintetizados 2",
            "Saxofón soprano", "Saxofón alto", "Saxofón tenor", "Saxofón barítono",
            "Oboe", "Corno inglés", "Fagot", "Clarinete",
            "Flautín", "Flauta", "Flauta dulce", "Flauta de pan",
            "Botella soplada", "Shakuhachi", "Silbido", "Ocarina",
            "Solo 1 (cuadrada)", "Solo 2 (diente de sierra)", "Solo 3 (calíope)", "Solo 4 (chiff)",
            "Solo 5 (charang)", "Solo 6 (voz)", "Solo 7 (quintas)", "Solo 8 (bajo + solo)",
            "Fondo 1 (new age)", "Fondo 2 (cálido)", "Fondo 3 (polisintetizador)", "Fondo 4 (coro)",
            "Fondo 5 (con arco)", "Fondo 6 (metálico)", "Fondo 7 (halo)", "Fondo 8 (barrido)",
            "Efecto 1 (lluvia)", "Efecto 2 (banda sonora)", "Efecto 3 (cristal)", "Efecto 4 (atmósfera)",
            "Efecto 5 (brillo)", "Efecto 6 (duendes)", "Efecto 7 (ecos)", "Efecto 8 (ciencia ficción)",
            "Sitar", "Banjo", "Shamisen", "Koto",
            "Kalimba", "Gaita", "Violín folk", "Shanai",
            "Campanilla", "Agogó", "Tambores de acero", "Caja china",
            "Tambor taiko", "Tom melódico", "Tambor sintetizado", "Platillo invertido",
            "Ruido de trastes", "Ruido de respiración", "Orilla del mar", "Canto de pájaro",
            "Timbre de teléfono", "Helicóptero", "Aplausos", "Disparo",
        ],
        "zh": [
            "原声大钢琴", "明亮钢琴", "电钢琴", "酒吧钢琴",
            "电钢琴1", "电钢琴2", "大键琴", "击弦键琴",
            "钢片琴", "钟琴", "八音盒", "颤音琴",
            "马林巴", "木琴", "管钟", "扬琴",
            "拉杆风琴", "打击式风琴", "摇滚风琴", "教堂管风琴",
            "簧风琴", "手风琴", "口琴", "探戈手风琴",
            "尼龙弦吉他", "钢弦吉他", "爵士电吉他", "清音电吉他",
            "闷音电吉他", "过载吉他", "失真吉他", "吉他泛音",
            "原声贝斯", "指弹电贝斯", "拨片电贝斯", "无品贝斯",
            "击弦贝斯1", "击弦贝斯2", "合成贝斯1", "合成贝斯2",
            "小提琴", "中提琴", "大提琴", "低音提琴",
            "颤弓弦乐", "拨奏弦乐", "管弦竖琴", "定音鼓",
            "弦乐合奏1", "弦乐合奏2", "合成弦乐1", "合成弦乐2",
            "人声合唱（啊）", "人声（喔）", "合成人声", "管弦乐齐奏",
            "小号", "长号", "大号", "弱音小号",
            "圆号", "铜管组", "合成铜管1", "合成铜管2",
            "高音萨克斯", "中音萨克斯", "次中音萨克斯", "上低音萨克斯",
            "双簧管", "英国管", "巴松管", "单簧管",
            "短笛", "长笛", "竖笛", "排箫",
            "吹瓶声", "尺八", "口哨", "陶笛",
            "主音1（方波）", "主音2（锯齿波）", "主音3（汽笛风琴）", "主音4（吹气）",
            "主音5（charang）", "主音6（人声）", "主音7（五度）", "主音8（贝斯+主音）",
            "铺底1（新世纪）", "铺底2（温暖）", "铺底3（复音合成）", "铺底4（合唱）",
            "铺底5（弓奏）", "铺底6（金属）", "铺底7（光晕）", "铺底8（扫频）",
            "音效1（雨声）", "音效2（配乐）", "音效3（水晶）", "音效4（大气）",
            "音效5（明亮）", "音效6（妖精）", "音效7（回声）", "音效8（科幻）",
            "西塔琴", "班卓琴", "三味线", "日本筝",
            "卡林巴", "风笛", "民间小提琴", "唢呐",
            "叮当铃", "阿戈戈铃", "钢鼓", "木鱼",
            "太鼓", "旋律嗵鼓", "合成鼓", "反向镲",
            "吉他品丝杂音", "呼吸声", "海浪声", "鸟鸣",
            "电话铃声", "直升机", "掌声", "枪声",
        ],
        "ja": [
            "アコースティックグランドピアノ", "ブライトピアノ", "エレクトリックグランドピアノ", "ホンキートンクピアノ",
            "エレクトリックピアノ1", "エレクトリックピアノ2", "ハープシコード", "クラビネット",
            "チェレスタ", "グロッケンシュピール", "オルゴール", "ビブラフォン",
            "マリンバ", "シロフォン", "チューブラーベル", "ダルシマー",
            "ドローバーオルガン", "パーカッシブオルガン", "ロックオルガン", "チャーチオルガン",
            "リードオルガン", "アコーディオン", "ハーモニカ", "タンゴアコーディオン",
            "アコースティックギター（ナイロン）", "アコースティックギター（スチール）", "エレクトリックギター（ジャズ）", "エレクトリックギター（クリーン）",
            "エレクトリックギター（ミュート）", "オーバードライブギター", "ディストーションギター", "ギターハーモニクス",
            "アコースティックベース", "エレクトリックベース（指弾き）", "エレクトリックベース（ピック）", "フレットレスベース",
            "スラップベース1", "スラップベース2", "シンセベース1", "シンセベース2",
            "ヴァイオリン", "ヴィオラ", "チェロ", "コントラバス",
            "トレモロストリングス", "ピチカートストリングス", "オーケストラハープ", "ティンパニ",
            "ストリングアンサンブル1", "ストリングアンサンブル2", "シンセストリングス1", "シンセストリングス2",
            "クワイア（アー）", "ボイス（ウー）", "シンセクワイア", "オーケストラヒット",
            "トランペット", "トロンボーン", "チューバ", "ミュートトランペット",
            "フレンチホルン", "ブラスセクション", "シンセブラス1", "シンセブラス2",
            "ソプラノサックス", "アルトサックス", "テナーサックス", "バリトンサックス",
            "オーボエ", "イングリッシュホルン", "バスーン", "クラリネット",
            "ピッコロ", "フルート", "リコーダー", "パンフルート",
            "ブロウンボトル", "尺八", "ホイッスル", "オカリナ",
            "リード1（スクエア）", "リード2（ノコギリ波）", "リード3（カリオペ）", "リード4（チフ）",
            "リード5（チャラング）", "リード6（ボイス）", "リード7（5度）", "リード8（ベース＋リード）",
            "パッド1（ニューエイジ）", "パッド2（ウォーム）", "パッド3（ポリシンセ）", "パッド4（クワイア）",
            "パッド5（ボウド）", "パッド6（メタリック）", "パッド7（ヘイロー）", "パッド8（スウィープ）",
            "FX1（雨）", "FX2（サウンドトラック）", "FX3（クリスタル）", "FX4（アトモスフィア）",
            "FX5（ブライトネス）", "FX6（ゴブリン）", "FX7（エコー）", "FX8（SF）",
            "シタール", "バンジョー", "三味線", "琴",
            "カリンバ", "バグパイプ", "フィドル", "シャナイ",
            "ティンクルベル", "アゴゴ", "スチールドラム", "ウッドブロック",
            "太鼓", "メロディックタム", "シンセドラム", "リバースシンバル",
            "ギターフレットノイズ", "ブレスノイズ", "シーショア", "鳥のさえずり",
            "電話のベル", "ヘリコプター", "拍手", "銃声",
        ],
        "ru": [
            "Акустический рояль", "Яркое акустическое фортепиано", "Электрический рояль", "Хонки-тонк",
            "Электропиано 1", "Электропиано 2", "Клавесин", "Клавинет",
            "Челеста", "Глокеншпиль", "Музыкальная шкатулка", "Вибрафон",
            "Маримба", "Ксилофон", "Трубчатые колокола", "Цимбалы",
            "Орган с регистрами", "Перкуссионный орган", "Рок-орган", "Церковный орган",
            "Язычковый орган", "Аккордеон", "Губная гармоника", "Танго-аккордеон",
            "Акустическая гитара (нейлон)", "Акустическая гитара (сталь)", "Электрогитара (джаз)", "Электрогитара (чистая)",
            "Электрогитара (приглушённая)", "Перегруженная гитара", "Гитара с дисторшном", "Флажолеты гитары",
            "Акустический бас", "Электробас (пальцы)", "Электробас (медиатор)", "Безладовый бас",
            "Слэп-бас 1", "Слэп-бас 2", "Синтезаторный бас 1", "Синтезаторный бас 2",
            "Скрипка", "Альт", "Виолончель", "Контрабас",
            "Струнные тремоло", "Струнные пиццикато", "Оркестровая арфа", "Литавры",
            "Струнный ансамбль 1", "Струнный ансамбль 2", "Синтезаторные струнные 1", "Синтезаторные струнные 2",
            "Хор (А)", "Голоса (О)", "Синтезаторный хор", "Оркестровый удар",
            "Труба", "Тромбон", "Туба", "Труба с сурдиной",
            "Валторна", "Секция духовых", "Синтезаторные духовые 1", "Синтезаторные духовые 2",
            "Сопрано-саксофон", "Альт-саксофон", "Тенор-саксофон", "Баритон-саксофон",
            "Гобой", "Английский рожок", "Фагот", "Кларнет",
            "Пикколо", "Флейта", "Блокфлейта", "Пан-флейта",
            "Звук бутылки", "Сякухати", "Свист", "Окарина",
            "Лид 1 (квадрат)", "Лид 2 (пила)", "Лид 3 (каллиопа)", "Лид 4 (чифф)",
            "Лид 5 (чаранг)", "Лид 6 (голос)", "Лид 7 (квинты)", "Лид 8 (бас + лид)",
            "Пэд 1 (нью-эйдж)", "Пэд 2 (тёплый)", "Пэд 3 (полисинт)", "Пэд 4 (хор)",
            "Пэд 5 (смычковый)", "Пэд 6 (металлический)", "Пэд 7 (ореол)", "Пэд 8 (свип)",
            "Эффект 1 (дождь)", "Эффект 2 (саундтрек)", "Эффект 3 (кристалл)", "Эффект 4 (атмосфера)",
            "Эффект 5 (яркость)", "Эффект 6 (гоблины)", "Эффект 7 (эхо)", "Эффект 8 (фантастика)",
            "Ситар", "Банджо", "Сямисэн", "Кото",
            "Калимба", "Волынка", "Скрипка (фолк)", "Шенай",
            "Колокольчик", "Агого", "Стил-драм", "Деревянная коробочка",
            "Тайко", "Мелодический том", "Синтезаторный барабан", "Обратная тарелка",
            "Шум ладов гитары", "Шум дыхания", "Морской прибой", "Щебет птицы",
            "Телефонный звонок", "Вертолёт", "Аплодисменты", "Выстрел",
        ],
        "da": [
            "Akustisk flygel", "Lyst akustisk klaver", "Elektrisk flygel", "Honky-tonk-klaver",
            "Elektrisk klaver 1", "Elektrisk klaver 2", "Cembalo", "Klavinet",
            "Celesta", "Klokkespil", "Spilledåse", "Vibrafon",
            "Marimba", "Xylofon", "Rørklokker", "Hakkebræt",
            "Drawbar-orgel", "Perkussivt orgel", "Rockorgel", "Kirkeorgel",
            "Harmonium", "Harmonika", "Mundharmonika", "Tangoharmonika",
            "Akustisk guitar (nylon)", "Akustisk guitar (stål)", "Elektrisk guitar (jazz)", "Elektrisk guitar (ren)",
            "Elektrisk guitar (dæmpet)", "Overdrevet guitar", "Forvrænget guitar", "Guitarflageoletter",
            "Akustisk bas", "Elektrisk bas (fingre)", "Elektrisk bas (plekter)", "Båndløs bas",
            "Slap-bas 1", "Slap-bas 2", "Synthbas 1", "Synthbas 2",
            "Violin", "Bratsch", "Cello", "Kontrabas",
            "Tremolo-strygere", "Pizzicato-strygere", "Orkesterharpe", "Pauker",
            "Strygerensemble 1", "Strygerensemble 2", "Synthstrygere 1", "Synthstrygere 2",
            "Kor (aah)", "Stemmer (ooh)", "Synthkor", "Orkesterslag",
            "Trompet", "Basun", "Tuba", "Dæmpet trompet",
            "Valdhorn", "Messingsektion", "Synthmessing 1", "Synthmessing 2",
            "Sopransaxofon", "Altsaxofon", "Tenorsaxofon", "Barytonsaxofon",
            "Obo", "Engelskhorn", "Fagot", "Klarinet",
            "Piccolofløjte", "Tværfløjte", "Blokfløjte", "Panfløjte",
            "Blæst flaske", "Shakuhachi", "Fløjt", "Okarina",
            "Lead 1 (firkant)", "Lead 2 (savtak)", "Lead 3 (calliope)", "Lead 4 (chiff)",
            "Lead 5 (charang)", "Lead 6 (stemme)", "Lead 7 (kvinter)", "Lead 8 (bas + lead)",
            "Pad 1 (new age)", "Pad 2 (varm)", "Pad 3 (polysynth)", "Pad 4 (kor)",
            "Pad 5 (buet)", "Pad 6 (metallisk)", "Pad 7 (halo)", "Pad 8 (sweep)",
            "FX 1 (regn)", "FX 2 (filmmusik)", "FX 3 (krystal)", "FX 4 (atmosfære)",
            "FX 5 (lysstyrke)", "FX 6 (trolde)", "FX 7 (ekko)", "FX 8 (sci-fi)",
            "Sitar", "Banjo", "Shamisen", "Koto",
            "Kalimba", "Sækkepibe", "Spillemandsviolin", "Shanai",
            "Klingeklokke", "Agogo", "Steeldrums", "Træblok",
            "Taiko-tromme", "Melodisk tom", "Synthtromme", "Baglæns bækken",
            "Guitar-båndstøj", "Åndedrætslyd", "Havbrus", "Fuglekvidder",
            "Telefonringning", "Helikopter", "Bifald", "Geværskud",
        ],
    ]

    /// Localized GM program name for `index`, in the given language code
    /// (defaults to the app's current UI language). Falls back to English for
    /// "en", unknown languages, or a short/partial table — never blanks.
    static func programName(_ index: Int,
                            language: String = Localization.current) -> String {
        let i = max(0, min(programNames.count - 1, index))
        if language != "en",
           let table = localizedProgramNames[language], i < table.count {
            return table[i]
        }
        return programNames[i]
    }

    // 16 GM families × 8 programs. Used by the picker submenu hierarchy.
    static let families: [(name: String, range: ClosedRange<Int>)] = [
        ("Piano",       0...7),
        ("Chromatic",   8...15),
        ("Organ",       16...23),
        ("Guitar",      24...31),
        ("Bass",        32...39),
        ("Strings",     40...47),
        ("Ensemble",    48...55),
        ("Brass",       56...63),
        ("Reed",        64...71),
        ("Pipe",        72...79),
        ("Synth Lead",  80...87),
        ("Synth Pad",   88...95),
        ("Synth FX",    96...103),
        ("Ethnic",      104...111),
        ("Percussive",  112...119),
        ("Sound FX",    120...127),
    ]

    /// How a voice should behave when held under shift / linger mode.
    /// Sustained voices ring out via the synth's release envelope when
    /// we skip the noteOff. Staccato voices have ~1-2s natural decay
    /// and would just go silent — those get a doppler-style retrigger
    /// tail (decaying velocity, growing intervals) instead so the
    /// linger has audible texture.
    enum LingerCategory { case sustained, staccato }

    static func lingerCategory(for program: UInt8) -> LingerCategory {
        switch Int(program) {
        case 0...7:    return .sustained   // Pianos: long natural release
        case 8...15:   return .staccato    // Chromatic Percussion (mallets, bells)
        case 16...23:  return .sustained   // Organs hold forever
        case 24...31:  return .staccato    // Guitars (plucked)
        case 32...37:  return .staccato    // Acoustic / Electric / Slap basses
        case 38...39:  return .sustained   // Synth basses
        case 40...44:  return .sustained   // Bowed strings
        case 45...47:  return .staccato    // Pizzicato, Harp, Timpani
        case 48...55:  return .sustained   // Ensembles + choir
        case 56...63:  return .sustained   // Brass
        case 64...71:  return .sustained   // Reeds
        case 72...79:  return .sustained   // Pipes
        case 80...95:  return .sustained   // Synth lead + pad
        case 96...103: return .sustained   // Synth FX (long ambient tails)
        case 104...108: return .staccato   // Sitar, Banjo, Shamisen, Koto, Kalimba
        case 109...111: return .sustained  // Bagpipe, Fiddle, Shanai
        case 112...119: return .staccato   // Percussive family (woodblock, taiko, etc.)
        case 120...127: return .sustained  // Sound FX (mostly long)
        default:        return .sustained
        }
    }

    /// Three-letter family abbreviation for the menubar picker label.
    static func familyAbbrev(for program: UInt8) -> String {
        switch Int(program) / 8 {
        case 0:  return "PNO"
        case 1:  return "CHR"
        case 2:  return "ORG"
        case 3:  return "GTR"
        case 4:  return "BAS"
        case 5:  return "STR"
        case 6:  return "ENS"
        case 7:  return "BRS"
        case 8:  return "REE"
        case 9:  return "PIP"
        case 10: return "LED"
        case 11: return "PAD"
        case 12: return "FX"
        case 13: return "ETH"
        case 14: return "PRC"
        case 15: return "SFX"
        default: return "—"
        }
    }
}
