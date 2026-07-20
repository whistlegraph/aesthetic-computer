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
        let scale = min(size.width, size.height) / 27, center = CGPoint(x:size.width/2, y:size.height/2)
        // Deliberately top-down: this is a framebuffer painting of the audible
        // plane, not a perspective picture pretending to be acoustic data.
        func point(_ x: Double, _ y: Double, _ z: Double = 0) -> CGPoint { CGPoint(x:center.x + x*scale, y:center.y + y*scale-z*scale*0.08) }
        if let f=world.frame {
          // Coarse pixel-raycast field. Every cell sums distance loss, measured
          // energy, pitch phase, and listening-range falloff from all bodies.
          let cell=8.0, half=12.5
          for gy in stride(from:-half,through:half,by:cell/scale) { for gx in stride(from:-half,through:half,by:cell/scale) {
            var re=0.0,im=0.0,weight=0.0
            for b in f.bodies { let dx=gx-Double(b.x),dy=gy-Double(b.y),d=sqrt(dx*dx+dy*dy+Double(b.z*b.z)*0.12),e=min(1,hypot(Double(b.left),Double(b.right))*42),hz=max(32,Double(b.pitch)),fall=e/(1+0.17*d*d),phase=Double.pi*2*(world.time*hz-d*hz/343);re+=cos(phase)*fall;im+=sin(phase)*fall;weight+=fall }
            let pressure=min(1,sqrt(re*re+im*im)*1.9),density=min(1,weight * 0.9),h=(atan2(im,re)/(.pi*2)+1).truncatingRemainder(dividingBy:1),q=point(gx,gy)
            c.fill(Path(CGRect(x:q.x-cell/2,y:q.y-cell/2,width:cell,height:cell)),with:.color(Color(hue:h,saturation:0.38+pressure * 0.52,brightness:0.11+density * 0.72).opacity(0.16+density * 0.56)))
          }}
        }
        for q in -10...10 { var p=Path(); p.move(to:point(Double(q),-10,0));p.addLine(to:point(Double(q),10,0));c.stroke(p,with:.color(.cyan.opacity(q == 0 ? 0.28:0.08))); var r=Path();r.move(to:point(-10,Double(q),0));r.addLine(to:point(10,Double(q),0));c.stroke(r,with:.color(.cyan.opacity(q == 0 ? 0.28:0.08))) }
        guard let f = world.frame else { return }
        let listener = point(Double(f.lx),Double(f.ly),Double(f.lz)), rr=world.listeningRadius*scale
        c.fill(Path(ellipseIn:CGRect(x:listener.x-rr,y:listener.y-rr,width:rr*2,height:rr*2)),with:.color(.yellow.opacity(0.025)))
        c.stroke(Path(ellipseIn:CGRect(x:listener.x-rr,y:listener.y-rr,width:rr*2,height:rr*2)),with:.color(.yellow.opacity(0.62)),style:StrokeStyle(lineWidth:2,dash:[5,4]))
        c.fill(Path(ellipseIn:CGRect(x:listener.x-7,y:listener.y-7,width:14,height:14)),with:.color(.yellow))
        let selected = world.voices >= 12 ? Array(f.bodies.indices) : Array([4,0,2,3].prefix(world.voices))
        let order = selected.sorted { (f.bodies[$0].x + f.bodies[$0].y - f.bodies[$0].z) < (f.bodies[$1].x + f.bodies[$1].y - f.bodies[$1].z) }
        for (i,index) in order.enumerated() { let b = f.bodies[index]
          let energy = hypot(Double(b.left),Double(b.right)), phase=world.time*world.rpmScale*Double.pi*2*76/60 + Double(i)*0.37
          let ox=Double(b.x)+sin(phase*(1+Double(i)*0.023))*world.oscillation*0.16, oy=Double(b.y)+cos(phase*(1+Double(i)*0.031))*world.oscillation*0.16
          let p=point(ox,oy,Double(b.z)), low=b.pitch > 0 ? 1-min(1,max(0,log2(Double(b.pitch)/45)/7.5)):0, radius=5+low*11
          let color=Color(hue:hue(b.pitch),saturation:0.78,brightness:0.98), dx=listener.x-p.x, dy=listener.y-p.y, screenDistance=max(1,hypot(dx,dy)), ux=dx/screenDistance, uy=dy/screenDistance, nx = -uy, ny = ux
          let worldDistance=hypot(hypot(ox-Double(f.lx),oy-Double(f.ly)),Double(b.z-f.lz)), inRange=worldDistance <= world.listeningRadius, audible=min(1,energy*38), visibility=inRange ? 0.18+audible*0.82 : 0.09
          // Expanding radii are the near-future pressure arrivals. A ring
          // crossing the yellow receiver radius means that voice is imminent.
          let propagation=fmod(world.time*(1.4+min(2.2,Double(max(0,b.pitch))/500))+Double(index)*0.31,max(1,world.listeningRadius*1.65))
          for ring in 0..<3 { let wr=fmod(propagation+Double(ring)*world.listeningRadius * 0.55,max(1,world.listeningRadius*1.65))*scale;c.stroke(Path(ellipseIn:CGRect(x:p.x-wr,y:p.y-wr,width:wr*2,height:wr*2)),with:.color(color.opacity((inRange ? 0.10:0.035)+audible * 0.16)),lineWidth:1+audible*1.5) }
          // The center ray is the actual emitter -> listener propagation path.
          var ray=Path();ray.move(to:p);ray.addLine(to:listener);c.stroke(ray,with:.color(color.opacity(visibility)),style:StrokeStyle(lineWidth:1+audible*5,lineCap:.round,dash:inRange ? []:[5,7]))
          // A ship-facing radiation cone: width is measured stereo imbalance.
          let spread=8+18*abs(Double(b.left-b.right))/(Double(b.left+b.right)+0.00001), reach=min(screenDistance,48+audible*100)
          var cone=Path();cone.move(to:CGPoint(x:p.x+nx*radius * 0.45,y:p.y+ny*radius * 0.45));cone.addLine(to:CGPoint(x:p.x+ux*reach+nx*spread,y:p.y+uy*reach+ny*spread));cone.addLine(to:CGPoint(x:p.x+ux*reach-nx*spread,y:p.y+uy*reach-ny*spread));cone.closeSubpath();c.fill(cone,with:.color(color.opacity(0.025+audible*0.11)))
          // Wave packets move toward the listener; spacing compresses with pitch.
          let packetCount=Int(min(9,max(3,screenDistance/22))), speed=0.24+min(0.55,Double(max(0,b.pitch))/1800)
          for wave in 0..<packetCount { let u=(Double(wave)/Double(packetCount)+world.time*speed).truncatingRemainder(dividingBy:1), x=p.x+dx*u, y=p.y+dy*u, half=(4+14*u)*(0.45+audible);var crest=Path();crest.move(to:CGPoint(x:x+nx*half,y:y+ny*half));crest.addQuadCurve(to:CGPoint(x:x-nx*half,y:y-ny*half),control:CGPoint(x:x+ux*(3+audible*7),y:y+uy*(3+audible*7)));c.stroke(crest,with:.color(color.opacity((inRange ? 0.18:0.04)+audible*0.52)),lineWidth:1+audible*2.5) }
          // The two ears are literal measured post-HRTF output lobes.
          let lr=max(2,radius*(0.25+min(1,Double(b.left)*70))), rr=max(2,radius*(0.25+min(1,Double(b.right)*70)))
          c.fill(Path(ellipseIn:CGRect(x:p.x+nx*(radius+3)-lr,y:p.y+ny*(radius+3)-lr,width:lr*2,height:lr*2)),with:.color(color.opacity(0.22+min(0.7,Double(b.left)*45))))
          c.fill(Path(ellipseIn:CGRect(x:p.x-nx*(radius+3)-rr,y:p.y-ny*(radius+3)-rr,width:rr*2,height:rr*2)),with:.color(color.opacity(0.22+min(0.7,Double(b.right)*45))))
          // Pitch becomes polygon complexity; the pointed face is aimed at ship.
          let sides=max(3,min(9,3+Int(max(0,log2(Double(max(40,b.pitch))/40))))), heading=atan2(dy,dx), facet=Path { path in for k in 0..<sides { let a=heading+Double(k)*Double.pi*2/Double(sides),q=CGPoint(x:p.x+cos(a)*radius,y:p.y+sin(a)*radius);k == 0 ? path.move(to:q):path.addLine(to:q) };path.closeSubpath() }
          c.fill(facet,with:.color(color.opacity(0.56+audible*0.44)));c.stroke(facet,with:.color(.white.opacity(0.18+audible*0.58)),lineWidth:1.5)
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
