import SwiftUI
import AVFoundation

struct BodyFrame { var x, y, z, left, right, pitch: Float }
struct ScoreFrame { var time, lx, ly, lz: Float; var bodies: [BodyFrame] }

@MainActor final class World: ObservableObject {
  @Published var time = 0.0
  @Published var playing = false
  @Published var listeningRadius = 5.0
  @Published var rpmScale = 1.0
  @Published var oscillation = 1.0
  @Published var voices = 12
  var frames: [ScoreFrame] = []
  var audio: AVAudioPlayer?
  private var timer: Timer?
  let fps = 24.0

  init() {
    let args = CommandLine.arguments
    if args.count > 1 { loadScene(args[1]) }
    if args.count > 2, let p = try? AVAudioPlayer(contentsOf: URL(fileURLWithPath: args[2])) { audio = p; p.prepareToPlay() }
    if args.count > 3 { voices = Int(args[3]) ?? 12 }
    timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
      Task { @MainActor in self?.tick() }
    }
  }

  var duration: Double { frames.isEmpty ? 120 : Double(frames.count) / fps }
  var frame: ScoreFrame? { frames.isEmpty ? nil : frames[min(frames.count - 1, max(0, Int(time * fps)))] }
  func toggle() { playing.toggle(); if playing { audio?.currentTime = time; audio?.play() } else { audio?.pause() } }
  func seek(_ t: Double) { time = min(duration, max(0, t)); audio?.currentTime = time }
  private func tick() { guard playing else { return }; time = audio?.isPlaying == true ? audio!.currentTime : time + 1 / 60; if time >= duration { playing = false; time = 0; audio?.stop() } }

  private func loadScene(_ path: String) {
    guard let data = try? Data(contentsOf: URL(fileURLWithPath: path)) else { return }
    func u32(_ o: Int) -> UInt32 { data.withUnsafeBytes { $0.loadUnaligned(fromByteOffset: o, as: UInt32.self) } }
    func f32(_ o: Int) -> Float { Float(bitPattern: u32(o)) }
    guard data.count >= 20, u32(0) == 0x53434e45 else { return }
    let version = Int(u32(4)), count = Int(u32(16)), n = Int(u32(8)), row = version >= 2 ? 6 : 5
    var offset = 20, result: [ScoreFrame] = []; result.reserveCapacity(n)
    for _ in 0..<n {
      guard offset + (4 + count * row) * 4 <= data.count else { break }
      let t = f32(offset), lx = f32(offset + 4), ly = f32(offset + 8), lz = f32(offset + 12); offset += 16
      var bodies: [BodyFrame] = []; bodies.reserveCapacity(count)
      for _ in 0..<count { let p = row > 5 ? f32(offset + 20) : 440; bodies.append(.init(x:f32(offset), y:f32(offset+4), z:f32(offset+8), left:f32(offset+12), right:f32(offset+16), pitch:p)); offset += row * 4 }
      result.append(.init(time:t, lx:lx, ly:ly, lz:lz, bodies:bodies))
    }
    frames = result
  }
}

struct WorldView: View {
  @ObservedObject var world: World
  private func hue(_ hz: Float) -> Double { guard hz > 0 else { return 0.55 }; return min(0.86, max(0.02, 0.04 + 0.78 * log2(Double(hz) / 40) / 8)) }
  var body: some View {
    GeometryReader { geo in
      Canvas { c, size in
        let scale = min(size.width, size.height) / 22, center = CGPoint(x:size.width/2, y:size.height/2)
        func point(_ x: Double, _ y: Double, _ z: Double) -> CGPoint { CGPoint(x:center.x + (x-y)*scale*0.72, y:center.y + (x+y)*scale*0.34-z*scale*0.72) }
        for q in -10...10 { var p=Path(); p.move(to:point(Double(q),-10,0));p.addLine(to:point(Double(q),10,0));c.stroke(p,with:.color(.cyan.opacity(q == 0 ? 0.28:0.08))); var r=Path();r.move(to:point(-10,Double(q),0));r.addLine(to:point(10,Double(q),0));c.stroke(r,with:.color(.cyan.opacity(q == 0 ? 0.28:0.08))) }
        guard let f = world.frame else { return }
        let listener = point(Double(f.lx),Double(f.ly),Double(f.lz)), rr=world.listeningRadius*scale
        c.stroke(Path(ellipseIn:CGRect(x:listener.x-rr,y:listener.y-rr*0.48,width:rr*2,height:rr*0.96)),with:.color(.yellow.opacity(0.42)),lineWidth:2)
        c.fill(Path(ellipseIn:CGRect(x:listener.x-7,y:listener.y-7,width:14,height:14)),with:.color(.yellow))
        let order = world.voices >= 12 ? Array(f.bodies.indices) : Array([4,0,2,3].prefix(world.voices))
        for (i,index) in order.enumerated() { let b = f.bodies[index]
          let energy = hypot(Double(b.left),Double(b.right)), phase=world.time*world.rpmScale*Double.pi*2*76/60 + Double(i)*0.37
          let ox=Double(b.x)+sin(phase*(1+Double(i)*0.023))*world.oscillation*0.16, oy=Double(b.y)+cos(phase*(1+Double(i)*0.031))*world.oscillation*0.16
          let p=point(ox,oy,Double(b.z)), low=b.pitch > 0 ? 1-min(1,max(0,log2(Double(b.pitch)/45)/7.5)):0, radius=4+low*10
          var trail=Path();trail.move(to:p);trail.addLine(to:CGPoint(x:p.x-cos(phase)*24*world.oscillation,y:p.y-sin(phase)*12*world.oscillation));c.stroke(trail,with:.color(Color(hue:hue(b.pitch),saturation:0.8,brightness:1).opacity(0.25+min(0.5,energy*25))),lineWidth:2)
          c.fill(Path(ellipseIn:CGRect(x:p.x-radius,y:p.y-radius,width:radius*2,height:radius*2)),with:.color(Color(hue:hue(b.pitch),saturation:0.72,brightness:0.95)))
        }
      }.background(LinearGradient(colors:[Color(red:0.025,green:0.045,blue:0.07),Color(red:0.015,green:0.02,blue:0.035)],startPoint:.top,endPoint:.bottom))
    }
  }
}

struct ContentView: View {
  @StateObject var world = World()
  var body: some View {
    VStack(spacing:0) {
      WorldView(world:world)
      VStack {
        HStack { Button(world.playing ? "Pause" : "Play") { world.toggle() }; Slider(value:Binding(get:{world.time},set:{world.seek($0)}),in:0...max(1,world.duration)); Text(String(format:"%05.1f / %05.1f",world.time,world.duration)).monospacedDigit() }
        HStack { Text("Range");Slider(value:$world.listeningRadius,in:1...12);Text("RPM");Slider(value:$world.rpmScale,in:0...4);Text("Oscillation");Slider(value:$world.oscillation,in:0...3);Picker("Voices",selection:$world.voices){ForEach([1,3,4,12],id:\.self){Text("\($0)")}}.frame(width:120) }
      }.padding(12).background(.black.opacity(0.88))
    }.frame(minWidth:900,minHeight:700).preferredColorScheme(.dark)
  }
}

@main struct SineabyeLive: App { var body: some Scene { WindowGroup { ContentView() } } }
