// WaveWizard — native macOS wizard for recording labelled audio samples.
//
// Usage:  WaveWizard <spec.json>
//
// The spec lists samples (name + description). For each sample the wizard:
//   1. speaks the description (AVSpeechSynthesizer),
//   2. waits a beat, plays Tink,
//   3. listens for an onset (avgPower > onsetDb),
//   4. records until N ms of silence (avgPower < silenceDb) follow the onset,
//   5. auto-trims (peak-relative threshold) + normalizes (-1 dB) the WAV,
//   6. plays it back and shows Keep / Retry / Skip.
import AppKit

let app = NSApplication.shared
let delegate = WaveWizardAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
