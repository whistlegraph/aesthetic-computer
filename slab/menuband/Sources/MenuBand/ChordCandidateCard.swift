import AppKit
import QuartzCore

/// Shared chord-candidate card builder used by both the floating play
/// palette and the popover. Each card shows the chord name (colored by
/// the root note's chromatic identity) plus large colored chips for
/// the notes still needed to complete the chord. Complete chords drop
/// the chips and grow a colored glow so they read as "active".
enum FloatingChordCandidateCard {
    /// Compact inline pill height. Everything (chord name + missing-
    /// note chips) lives in a single horizontal row so the card reads
    /// as a single tappable suggestion at a glance, no bigger than a
    /// piece of label tape on the visualizer.
    static let baseCardHeight: CGFloat = 24
    static let chipDiameter: CGFloat = 16

    static func build(candidate: MenuBandController.ChordCandidate,
                      isDark: Bool) -> NSView {
        let card = NSView()
        card.wantsLayer = true
        card.layer?.cornerRadius = 5
        card.layer?.masksToBounds = false
        card.translatesAutoresizingMaskIntoConstraints = false

        let rootColor = NoteColors.color(pitchClass: candidate.rootPitchClass)
        let isSharpRoot = NoteColors.isAccidental(pitchClass: candidate.rootPitchClass)
        let cardAccent: NSColor = isSharpRoot
            ? NSColor(white: isDark ? 0.55 : 0.35, alpha: 1.0)
            : rootColor

        let nameLabel = NSTextField(labelWithString: candidate.name)
        nameLabel.font = NSFont.monospacedSystemFont(
            ofSize: candidate.isComplete ? 14 : 12,
            weight: .heavy
        )
        nameLabel.drawsBackground = false
        nameLabel.translatesAutoresizingMaskIntoConstraints = false

        if candidate.isComplete {
            // Active chord: solid root-color fill + colored glow.
            // Glow plus the parent-supplied shake gives the chord-
            // locks-in feel without taking up extra vertical space.
            card.layer?.backgroundColor = cardAccent.withAlphaComponent(0.95).cgColor
            card.layer?.borderColor = cardAccent.shadow(withLevel: 0.30)?.cgColor
                ?? cardAccent.cgColor
            card.layer?.borderWidth = 1.5
            card.layer?.shadowColor = cardAccent.cgColor
            card.layer?.shadowRadius = 9
            card.layer?.shadowOpacity = 0.85
            card.layer?.shadowOffset = .zero
            nameLabel.textColor = NoteColors.textColor(on: cardAccent)
        } else {
            let bgAlpha: CGFloat = isDark ? 0.22 : 0.18
            card.layer?.backgroundColor = cardAccent.withAlphaComponent(bgAlpha).cgColor
            card.layer?.borderColor = cardAccent.withAlphaComponent(0.55).cgColor
            card.layer?.borderWidth = 1
            nameLabel.textColor = isDark
                ? NSColor.white.withAlphaComponent(0.95)
                : NSColor.black.withAlphaComponent(0.90)
        }

        let row = NSStackView()
        row.orientation = .horizontal
        row.alignment = .centerY
        row.spacing = 4
        row.translatesAutoresizingMaskIntoConstraints = false
        row.addArrangedSubview(nameLabel)
        if !candidate.isComplete {
            for pc in candidate.missingPitchClasses {
                row.addArrangedSubview(makeMissingNoteChip(pitchClass: pc))
            }
        }
        card.addSubview(row)
        NSLayoutConstraint.activate([
            row.leadingAnchor.constraint(equalTo: card.leadingAnchor, constant: 7),
            row.trailingAnchor.constraint(equalTo: card.trailingAnchor, constant: -7),
            row.centerYAnchor.constraint(equalTo: card.centerYAnchor),
            card.heightAnchor.constraint(equalToConstant: baseCardHeight),
        ])
        return card
    }

    /// Compact missing-note chip — small rounded square in the note's
    /// chromatic color, lettered with the note name. Sized to sit
    /// comfortably inline next to the chord name. Sharps stay black
    /// with white text so accidental-bearing chords stay legible.
    private static func makeMissingNoteChip(pitchClass: Int) -> NSView {
        let chip = NSView()
        chip.wantsLayer = true
        chip.layer?.cornerRadius = 3
        chip.translatesAutoresizingMaskIntoConstraints = false
        let color = NoteColors.color(pitchClass: pitchClass)
        chip.layer?.backgroundColor = color.cgColor

        let pcs = MenuBandController.pitchClassNames
        let safe = ((pitchClass % 12) + 12) % 12
        let label = NSTextField(labelWithString: pcs[safe])
        label.font = NSFont.monospacedSystemFont(ofSize: 9, weight: .black)
        label.textColor = NoteColors.textColor(on: color)
        label.drawsBackground = false
        label.translatesAutoresizingMaskIntoConstraints = false
        chip.addSubview(label)
        NSLayoutConstraint.activate([
            chip.widthAnchor.constraint(equalToConstant: chipDiameter),
            chip.heightAnchor.constraint(equalToConstant: chipDiameter),
            label.centerXAnchor.constraint(equalTo: chip.centerXAnchor),
            label.centerYAnchor.constraint(equalTo: chip.centerYAnchor),
        ])
        return chip
    }
}
