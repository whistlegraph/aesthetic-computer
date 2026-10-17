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

final class MenuBandAirView: NSStackView {
    private let slider = NSSlider(value: 0, minValue: 0, maxValue: 1, target: nil, action: nil)
    private let color = NSPopUpButton(frame: .zero, pullsDown: false)
    private let readout = NSTextField(labelWithString: "0%")
    private let filterSlider = NSSlider(value: 0, minValue: 0, maxValue: 1, target: nil, action: nil)
    private let filterReadout = NSTextField(labelWithString: "160 Hz")
    private let control: MenuBandAmbientAir

    init(control: MenuBandAmbientAir) {
        self.control = control
        super.init(frame: .zero)
        orientation = .vertical; alignment = .leading; spacing = 5
        let label = NSTextField(labelWithString: "Air")
        label.font = .systemFont(ofSize: 11)
        color.addItems(withTitles: MenuBandAirColor.allCases.map(\.title))
        color.controlSize = .mini; color.target = self; color.action = #selector(changeColor)
        color.setAccessibilityLabel("Air noise color")
        color.widthAnchor.constraint(equalToConstant: 62).isActive = true
        slider.controlSize = .small; slider.isContinuous = true
        slider.target = self; slider.action = #selector(changeLevel)
        slider.setAccessibilityLabel("Air level")
        slider.widthAnchor.constraint(equalToConstant: 92).isActive = true
        slider.toolTip = "Background noise; zero is silent. Plays through Menu Band’s output."
        readout.font = .monospacedDigitSystemFont(ofSize: 10, weight: .regular)
        readout.widthAnchor.constraint(equalToConstant: 32).isActive = true
        let airRow = NSStackView(views: [label, color, slider, readout])
        airRow.orientation = .horizontal; airRow.alignment = .centerY; airRow.spacing = 5
        addArrangedSubview(airRow)
        let filterLabel = NSTextField(labelWithString: "Filter")
        filterLabel.font = .systemFont(ofSize: 11)
        filterLabel.widthAnchor.constraint(equalToConstant: 38).isActive = true
        filterSlider.controlSize = .small; filterSlider.isContinuous = true
        filterSlider.target = self; filterSlider.action = #selector(changeFilter)
        filterSlider.setAccessibilityLabel("Air filter cutoff")
        filterSlider.widthAnchor.constraint(equalToConstant: 119).isActive = true
        filterSlider.toolTip = "Left: deeper rumble. Right: brighter air. Filters Air only."
        filterReadout.font = .monospacedDigitSystemFont(ofSize: 10, weight: .regular)
        filterReadout.widthAnchor.constraint(equalToConstant: 51).isActive = true
        let filterRow = NSStackView(views: [filterLabel, filterSlider, filterReadout])
        filterRow.orientation = .horizontal; filterRow.alignment = .centerY; filterRow.spacing = 5
        addArrangedSubview(filterRow)
        control.onChange = { [weak self] in self?.refresh() }
        refresh()
    }
    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }
    @objc private func changeLevel() { control.setLevel(Float(slider.doubleValue)) }
    @objc private func changeColor() { control.setColor(MenuBandAirColor.allCases[color.indexOfSelectedItem]) }
    @objc private func changeFilter() { control.setCutoff(MenuBandAirNoise.cutoff(at: filterSlider.doubleValue)) }
    private func refresh() {
        slider.doubleValue = Double(control.level)
        readout.stringValue = "\(Int(control.level * 100))%"
        color.selectItem(at: MenuBandAirColor.allCases.firstIndex(of: control.color) ?? 0)
        filterSlider.doubleValue = MenuBandAirNoise.position(for: control.cutoff)
        filterReadout.stringValue = "\(Int(control.cutoff.rounded())) Hz"
    }
}
