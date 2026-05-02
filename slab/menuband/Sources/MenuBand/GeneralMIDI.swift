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
