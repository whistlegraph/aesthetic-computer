import AppKit
import AVFoundation

/// Listening noise on the instrument's existing output engine. Changing Air
/// never opens a device, captures another app, or changes system volume.
final class MenuBandAmbientAir {
    private let lock = NSLock()
    private var requestedLevel: Float = 0
    private var requestedColor: MenuBandAirColor
    private var requestedCutoff: Double
    /// Air is a listening sound for the player at Menu Band. Leaving the app
    /// fades it out; coming back fades it in at the slider's level.
    private var focused = true
    private let defaults: UserDefaults
    private var source: AVAudioSourceNode?
    private var noise = MenuBandAirNoise()
    private var renderLevel: Float = 0
    private var targetLevel: Float = 0
    private var renderColor: MenuBandAirColor = .cabin
    private var renderCutoff = MenuBandAirNoise.defaultCutoff
    // A fixed source format lets AVAudioEngine resample across device changes.
    private let sampleRate: Double = 44_100
    var onChange: (() -> Void)?
    /// Engine lifetime changes only at zero, never on each slider tick.
    var onActiveChange: ((Bool) -> Void)?

    init(defaults: UserDefaults = .standard) {
        self.defaults = defaults
        requestedColor = MenuBandAirColor(rawValue: defaults.string(forKey: "systemAirColor") ?? "") ?? .cabin
        let saved = defaults.double(forKey: "systemAirCutoff")
        requestedCutoff = saved.isFinite && saved >= 60 ? min(6000, saved) : MenuBandAirNoise.defaultCutoff
    }

    var level: Float { lock.lock(); defer { lock.unlock() }; return requestedLevel }
    /// Whether Air is audible (or fading): slider above zero and Menu Band in
    /// front. The engine's idle logic keys off this, not the raw slider.
    var isActive: Bool { lock.lock(); defer { lock.unlock() }; return requestedLevel > 0 && focused }
    var color: MenuBandAirColor { lock.lock(); defer { lock.unlock() }; return requestedColor }
    var cutoff: Double { lock.lock(); defer { lock.unlock() }; return requestedCutoff }

    func setLevel(_ value: Float) {
        guard value.isFinite else { return }
        let value = max(0, min(1, value))
        lock.lock()
        let wasActive = requestedLevel > 0 && focused
        requestedLevel = value
        let nowActive = requestedLevel > 0 && focused
        lock.unlock()
        if wasActive != nowActive { onActiveChange?(nowActive) }
        onChange?()
    }

    /// Menu Band in front (popover open, performance focus, or app active):
    /// Air renders at the slider level. Away: Air ramps to silence over the
    /// same 250 ms as a slider move, keeping the slider where it was.
    func setFocused(_ value: Bool) {
        lock.lock()
        guard focused != value else { lock.unlock(); return }
        let wasActive = requestedLevel > 0 && focused
        focused = value
        let nowActive = requestedLevel > 0 && focused
        lock.unlock()
        if wasActive != nowActive { onActiveChange?(nowActive) }
    }

    func setColor(_ color: MenuBandAirColor) {
        lock.lock(); requestedColor = color; lock.unlock()
        defaults.set(color.rawValue, forKey: "systemAirColor")
        onChange?()
    }

    func setCutoff(_ value: Double) {
        guard value.isFinite else { return }
        let value = max(60, min(6000, value))
        lock.lock(); requestedCutoff = value; lock.unlock()
        defaults.set(value, forKey: "systemAirCutoff")
        onChange?()
    }

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard source == nil else { return }
        let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 2)!
        let node = AVAudioSourceNode(format: format) { [weak self] silent, _, frames, output in
            if let self {
                self.render(frames: Int(frames), output: output, silent: silent)
            } else {
                for buffer in UnsafeMutableAudioBufferListPointer(output) {
                    if let data = buffer.mData { memset(data, 0, Int(buffer.mDataByteSize)) }
                }
                silent.pointee = true
            }
            return noErr
        }
        engine.attach(node)
        engine.connect(node, to: output, format: format)
        source = node
    }

    private func render(frames: Int, output: UnsafeMutablePointer<AudioBufferList>,
                        silent: UnsafeMutablePointer<ObjCBool>) {
        // Never wait for the UI on the audio thread; a concurrent slider move
        // can be picked up on the next buffer.
        if lock.try() {
            targetLevel = focused ? requestedLevel : 0
            renderColor = requestedColor; renderCutoff = requestedCutoff
            lock.unlock()
        }
        let buffers = UnsafeMutableAudioBufferListPointer(output)
        if targetLevel == 0 && renderLevel == 0 {
            for buffer in buffers {
                if let data = buffer.mData { memset(data, 0, Int(buffer.mDataByteSize)) }
            }
            silent.pointee = true
            return
        }
        silent.pointee = false
        let step = Float(1 / (sampleRate * 0.25))
        for frame in 0..<frames {
            renderLevel += max(-step, min(step, targetLevel - renderLevel))
            let sample = renderLevel > 0
                ? noise.next(color: renderColor, sampleRate: sampleRate, cutoff: renderCutoff)
                    * renderLevel * renderLevel * 0.12 : 0
            for buffer in buffers {
                guard let data = buffer.mData else { continue }
                let channels = Int(buffer.mNumberChannels)
                let samples = data.assumingMemoryBound(to: Float.self)
                for channel in 0..<channels { samples[frame * channels + channel] = sample }
            }
        }
    }
}

/// Air in the popover: a wind icon and a level slider. Click the icon to turn
/// Air off or back on at its last level; right-click it (or the slider) for
/// noise color and tone. Values live in tooltips, not readouts.
final class MenuBandAirView: NSStackView, NSMenuDelegate {
    private let icon = NSButton()
    private let slider = NSSlider(value: 0, minValue: 0, maxValue: 1, target: nil, action: nil)
    private let control: MenuBandAmbientAir
    private var lastAudibleLevel: Float = 0.3
    private static let tones: [(title: String, position: Double)] = [
        ("Deep", 0.15), ("Warm", 0.45), ("Bright", 0.8),
    ]

    init(control: MenuBandAmbientAir) {
        self.control = control
        super.init(frame: .zero)
        orientation = .horizontal; alignment = .centerY; spacing = 4
        icon.bezelStyle = .inline; icon.isBordered = false
        icon.imagePosition = .imageOnly; icon.controlSize = .small
        icon.image = NSImage(systemSymbolName: "wind", accessibilityDescription: "Air")
        icon.target = self; icon.action = #selector(toggle)
        icon.setAccessibilityLabel("Air")
        slider.controlSize = .small; slider.isContinuous = true
        slider.target = self; slider.action = #selector(changeLevel)
        slider.setAccessibilityLabel("Air level")
        slider.widthAnchor.constraint(equalToConstant: 84).isActive = true
        let menu = NSMenu(); menu.delegate = self
        icon.menu = menu; slider.menu = menu
        addArrangedSubview(icon); addArrangedSubview(slider)
        control.onChange = { [weak self] in self?.refresh() }
        refresh()
    }
    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    @objc private func toggle() {
        if control.level > 0 {
            lastAudibleLevel = control.level
            control.setLevel(0)
        } else {
            control.setLevel(lastAudibleLevel)
        }
    }
    @objc private func changeLevel() {
        let level = Float(slider.doubleValue)
        if level > 0 { lastAudibleLevel = level }
        control.setLevel(level)
    }
    @objc private func pickColor(_ sender: NSMenuItem) {
        guard let color = sender.representedObject as? MenuBandAirColor else { return }
        control.setColor(color)
    }
    @objc private func pickTone(_ sender: NSMenuItem) {
        guard let position = sender.representedObject as? Double else { return }
        control.setCutoff(MenuBandAirNoise.cutoff(at: position))
    }

    func menuNeedsUpdate(_ menu: NSMenu) {
        menu.removeAllItems()
        let colorHeader = NSMenuItem(title: "Color", action: nil, keyEquivalent: "")
        colorHeader.isEnabled = false
        menu.addItem(colorHeader)
        for color in MenuBandAirColor.allCases {
            let item = NSMenuItem(title: color.title, action: #selector(pickColor(_:)), keyEquivalent: "")
            item.target = self; item.representedObject = color
            item.state = control.color == color ? .on : .off
            menu.addItem(item)
        }
        menu.addItem(.separator())
        let toneHeader = NSMenuItem(title: "Tone", action: nil, keyEquivalent: "")
        toneHeader.isEnabled = false
        menu.addItem(toneHeader)
        let current = MenuBandAirNoise.position(for: control.cutoff)
        let nearest = Self.tones.min { abs($0.position - current) < abs($1.position - current) }
        for tone in Self.tones {
            let item = NSMenuItem(title: tone.title, action: #selector(pickTone(_:)), keyEquivalent: "")
            item.target = self; item.representedObject = tone.position
            item.state = nearest?.title == tone.title ? .on : .off
            menu.addItem(item)
        }
    }

    private func refresh() {
        let level = control.level
        slider.doubleValue = Double(level)
        if level > 0 { lastAudibleLevel = level }
        icon.contentTintColor = level > 0 ? .systemTeal : .secondaryLabelColor
        let percent = "\(Int(level * 100))%"
        let summary = "Air \(percent) · \(control.color.title) · \(Int(control.cutoff.rounded())) Hz"
        icon.toolTip = summary + " — click to turn \(level > 0 ? "off" : "on"), right-click for color and tone"
        slider.toolTip = summary
        slider.setAccessibilityValueDescription(percent)
    }
}
