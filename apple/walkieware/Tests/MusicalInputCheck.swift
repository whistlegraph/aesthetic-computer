import Foundation
import AVFoundation
@main struct Check {
 static func main() {
  let rate=8000.0
  for hz in [110.0,220,440,880,1600] {
   let samples=(0..<512).map { sin(2 * Double.pi * hz * Double($0) / rate) * 0.25 }
   let result=MusicalInput.measure(samples,rate:rate)
   guard let pitch=result.pitch, abs(pitch-hz)/hz < 0.025 else { fatalError("Pitch failed: \(hz) \(result)") }
   print("\(Int(hz)) Hz → \(Int(pitch)) Hz")
  }
  precondition(MusicalInput.measure(Array(repeating:0,count:512),rate:rate).pitch == nil)
  var seed: UInt64=42
  let noise=(0..<512).map { _ -> Double in seed=seed &* 6364136223846793005 &+ 1;return (Double(seed >> 32)/Double(UInt32.max)-0.5)*0.4 }
  precondition(MusicalInput.measure(noise,rate:rate).pitch == nil)
  print("Silence and seeded noise rejected as pitch")
  let rhythm=MusicalInput(), format=AVAudioFormat(standardFormatWithSampleRate:8000,channels:1)!
  let pulse=AVAudioPCMBuffer(pcmFormat:format,frameCapacity:16000)!
  pulse.frameLength=16000
  for i in 0..<16000 {
   let t=Double(i)/8000
   let active=(0.2..<0.4).contains(t)||(0.7..<0.9).contains(t)||(1.2..<1.6).contains(t)
   pulse.floatChannelData![0][i]=active ? Float(sin(2 * Double.pi * 440 * t)*0.25) : 0
  }
  rhythm.feed(pulse)
  let rhythmDone=DispatchSemaphore(value:0)
  rhythm.finish { value in
   let onsets=value["onsetsMs"] as! [Double]
   precondition(onsets.count==3,"Expected three separated attacks: \(onsets)")
   for (actual,expected) in zip(onsets,[200.0,700,1200]) { precondition(abs(actual-expected)<64) }
   print("Three attacks detected within one analysis window: \(onsets)")
   rhythmDone.signal()
  }
  precondition(rhythmDone.wait(timeout:.now()+10) == .success)
  for fixture in ["sine-tone","whistle-sweep","mixed-request"] {
   let file = try! AVAudioFile(forReading:URL(fileURLWithPath:"apple/walkieware/Resources/Fixtures/"+fixture+".wav"))
   let analyzer = MusicalInput()
   while file.framePosition < file.length {
    let n = AVAudioFrameCount(min(317,file.length-file.framePosition)) // deliberately odd chunk size
    let b = AVAudioPCMBuffer(pcmFormat:file.processingFormat,frameCapacity:n)!
    try! file.read(into:b,frameCount:n);analyzer.feed(b)
   }
   let done = DispatchSemaphore(value:0)
   analyzer.finish { value in
    let duration = value["durationMs"] as! Double
    precondition(abs(duration-Double(file.length)/file.processingFormat.sampleRate*1000)<1)
    let frames = value["frames"] as! [[String:Double]]
    let pitches = frames.compactMap{$0["pitchHz"]}
    precondition(!pitches.isEmpty && frames.count<=128)
    if fixture == "sine-tone" { precondition(pitches.allSatisfy{abs($0-880)<15}) }
    if fixture == "whistle-sweep" { precondition(pitches.last!>pitches.first!+700) }
    print("\(fixture): \(frames.count) frames; \(Int(duration)) ms; pitch \(Int(pitches.min()!))–\(Int(pitches.max()!)) Hz")
    done.signal()
   }
   precondition(done.wait(timeout:.now()+10) == .success)
  }
 }
}
