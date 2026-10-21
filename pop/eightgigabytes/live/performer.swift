// Same singer + face + caption sources as Menu Band; a fixed 48 kHz pop signal path.
// build.sh concatenates Menu Band's MenuBandSinger, SingerArticulation, SingerFace*,
// SingerGaze and LyricCaption ahead of this file, so this is one Swift file: the
// caption's private panel is reachable from the extension below.
import AppKit
import AVFoundation
import QuartzCore

let app=NSApplication.shared
app.setActivationPolicy(.accessory)
let activity=ProcessInfo.processInfo.beginActivity(options:.userInitiated,reason:"Prepare and perform eight gigabytes")
let args=CommandLine.arguments
func arg(_ key:String)->String? { guard let i=args.firstIndex(of:key),i+1<args.count else{return nil};return args[i+1] }
let configPath=arg("--config")!
let folder=URL(fileURLWithPath:configPath).deletingLastPathComponent().path
let config=try JSONSerialization.jsonObject(with:Data(contentsOf:URL(fileURLWithPath:configPath))) as! [String:Any]
let member=config["member"] as! String,duration=config["duration"] as! Double,offset=config["offset"] as! Double
let peers=config["peers"] as? [String] ?? []
let beatSeconds=config["beat"] as? Double ?? 0.6
let check=args.contains("--check"),preview=arg("--preview")
let epoch=Double(arg("--epoch") ?? "0")!
// The conductor's pid (its own member only): if the conductor dies — Ctrl-C, a killed
// terminal, an interrupted tool — this member cancels and takes the peers down with it,
// so the piece never outlives the hand that cued it.
let conductorPid=Int32(arg("--conductor") ?? "0") ?? 0
let out=arg("--out") ?? folder+"/check-"+member
let statusPath=folder+"/status.json"
let cStatus=strdup(folder+"/audio-status.json")!
var info=config["payload"] as! [String:String]
let pinned=info["singVoice"]!
guard MenuBandSinger.voice(named:pinned) != nil else {fatalError("Missing pinned voice \(pinned)")}
info["singVoice"]=config["speechVoice"] as? String ?? pinned
let bpm=Double(info["bpm"]!)!,sung=SungLine(info:info,bpm:Double(info["bpm"]!)!)!
let notation=config["lines"] as! [[String:Any]]
let voice=MenuBandSinger(),format=AVAudioFormat(commonFormat:.pcmFormatFloat32,sampleRate:48000,channels:1,interleaved:false)!
var renders:[SungRender]=[]
// A line's glow: the lead sings at full gain and lights its screen fully; a backing part
// or a hum (quieter in the mix) lights its screen less.
let lineWeights=notation.map{sqrt(min(1,(($0["gain"] as! Double)/0.9)))}

// ── the screen as a lamp: brightness follows this member's own singing ───────────
// DisplayServices is the one brightness path that works on an Apple Silicon built-in
// panel (slab/bin/acbright.swift). A one-pole follower rises fast on a sung onset and
// dies down slowly, so the face glows while it sings and dims as it rests. Only the
// live performance touches the lamp; the silent check never does. Restored on exit.
let bright=config["brightness"] as? [String:Double] ?? [:]
let brightFloor=bright["floor"] ?? 0.22,brightLit=bright["lit"] ?? 1.0,brightHouse=Float(bright["house"] ?? 1.0)
typealias BrightGet = @convention(c) (UInt32, UnsafeMutablePointer<Float>) -> Int32
typealias BrightSet = @convention(c) (UInt32, Float) -> Int32
let displayServices=dlopen("/System/Library/PrivateFrameworks/DisplayServices.framework/DisplayServices",RTLD_NOW)
let brightGet=displayServices.flatMap{dlsym($0,"DisplayServicesGetBrightness")}.map{unsafeBitCast($0,to:BrightGet.self)}
let brightSet=displayServices.flatMap{dlsym($0,"DisplayServicesSetBrightness")}.map{unsafeBitCast($0,to:BrightSet.self)}
let mainDisplay=CGMainDisplayID()
var baseBrightness:Float?,shownBrightness:Float = -1,glow=1.0,lastGlowTime=0.0
var lampSets=0,lampLow:Float=1,lampHigh:Float=0
func readBrightness()->Float? { guard let g=brightGet else{return nil};var v:Float=0;return g(mainDisplay,&v)==0 ? v : nil }
func setBrightness(_ v:Float) {
 guard let s=brightSet else{return}
 let clamped=max(0,min(1,v));if abs(clamped-shownBrightness)<0.002 {return}
 if s(mainDisplay,clamped)==0 {shownBrightness=clamped;lampSets+=1;lampLow=min(lampLow,clamped);lampHigh=max(lampHigh,clamped)}
}
/// One frame of the lamp: ease the glow toward what is being sung, then light the screen.
func lamp(level:Double,weight:Double) {
 guard baseBrightness != nil else{return}
 let now=CACurrentMediaTime(),dt=lastGlowTime==0 ? 1.0/60 : max(0,min(0.1,now-lastGlowTime));lastGlowTime=now
 let target=min(1,max(0,level))*weight
 glow+=(target-glow)*(1-exp(-dt/(target>glow ? 0.05 : 0.7)))
 setBrightness(Float(brightFloor+(brightLit-brightFloor)*glow))
}
let brightnessPath=folder+"/brightness.json"   // the base, for a stop from outside this process
func restoreBrightness() {
 guard let b=baseBrightness else{return}
 try? FileManager.default.removeItem(atPath:brightnessPath)
 NSLog("💡 lamp: %d brightness sets, %.0f%%…%.0f%% (was %.0f%%), house lights to %.0f%%",lampSets,lampLow*100,lampHigh*100,b*100,brightHouse*100)
 shownBrightness = -1;setBrightness(brightHouse);baseBrightness=nil
}
let accent: NSColor = member=="neo" ? NSColor(srgbRed:143/255,green:209/255,blue:63/255,alpha:1) : member=="blueberry" ? NSColor(srgbRed:90/255,green:87/255,blue:211/255,alpha:1) : NSColor(srgbRed:242/255,green:167/255,blue:185/255,alpha:1)
func receipt(_ phase:String,_ extra:[String:Any]=[:]) {
 var j:[String:Any]=["phase":phase,"member":member,"pid":ProcessInfo.processInfo.processIdentifier,"startEpoch":epoch,"duration":duration,"voice":pinned]
 extra.forEach{j[$0.key]=$0.value}
 try? JSONSerialization.data(withJSONObject:j,options:.sortedKeys).write(to:URL(fileURLWithPath:statusPath),options:.atomic)
}

// ── the words, one at a time, timed from the score ───────────────────────────
// Every sung syllable is one pitched note of the line's notation ("56:1.5 r:.5 …",
// pitch:beats), so a syllable's onset is the line's start plus the beats before it.
struct Word { let token:String;let start:Double,end:Double;let syllableStarts:[Double] }
var words:[Word]=[]
for line in notation {
 let at=line["at"] as! Double,tokens=line["words"] as? [String] ?? [(line["text"] as! String)]
 var onsets:[(start:Double,end:Double)]=[];var cursor=at
 for note in (line["notation"] as! String).split(separator:" ") {
  let parts=note.split(separator:":");guard parts.count==2,let beats=Double(parts[1]) else{continue}
  let dur=beats*beatSeconds
  if parts[0] != "r" {onsets.append((cursor,cursor+dur))}
  cursor+=dur
 }
 var k=0
 for token in tokens {
  let n=token.split(separator:"-").count;guard k+n<=onsets.count else{break}
  let mine=onsets[k..<k+n];k+=n
  words.append(Word(token:token,start:mine.first!.start,end:mine.last!.end,syllableStarts:mine.map{$0.start}))
 }
}

extension LyricCaption {
 /// The word arrives from the side: the caption's whole layer slides in and settles.
 func slide(from dx:CGFloat) {
  guard let panel,let layer=panel.contentView?.layer else{return}
  // Menu Band seats the caption 9 % up the screen; here the progress bar lives there and the
  // descenders of the big lettering reached it, so the caption's own window is moved up
  // (show() re-seats it every word; a layer transform would be reset by the layer-backed view).
  panel.setFrameOrigin(NSPoint(x:panel.frame.origin.x,y:panel.frame.origin.y+captionSize()*0.75))
  let a=CABasicAnimation(keyPath:"transform.translation.x");a.fromValue=dx;a.toValue=0;a.duration=0.26
  a.timingFunction=CAMediaTimingFunction(name:.easeOut);layer.add(a,forKey:"slide")
 }
}
final class ProgressView:NSView {
 var fraction:CGFloat=0
 let track=accent.blended(withFraction:0.5,of:.black) ?? .darkGray
 let fill=accent.blended(withFraction:0.45,of:.white) ?? .white
 override func draw(_ rect:NSRect){
  track.setFill();bounds.fill()
  fill.setFill();NSRect(x:0,y:0,width:bounds.width*fraction,height:bounds.height).fill()
 }
}
final class StagePanel:NSPanel {
 override var canBecomeKey:Bool{true}
 override var canBecomeMain:Bool{true}
 override func keyDown(with e:NSEvent){ if e.keyCode==53 {cancel("escape")} }
 override func cancelOperation(_ sender:Any?){cancel("escape")}
}
var panel:NSPanel?,face:SingerFaceView?,progress:ProgressView?,timer:Timer?
var shownWord = -1, litSyllable = -1, captionUp=false
let caption=LyricCaption.at(nil)
let LEAD=0.12,LINGER=0.45   // the word shows a hair early and stays a beat after
func makeView(_ size:NSSize)->NSView {
 let root=NSView(frame:NSRect(origin:.zero,size:size))
 let f=SingerFaceView(frame:root.bounds);f.member=member;f.accent=accent;face=f
 root.addSubview(f)
 let bar=ProgressView(frame:NSRect(x:0,y:0,width:size.width,height:max(10,size.height*0.014)))
 root.addSubview(bar);progress=bar
 return root
}
// One word at a time, but never so big it crowds the face or clips at the edges (jeffrey 2026-09-26).
func captionSize()->CGFloat { round((NSScreen.screens.first ?? NSScreen.main!).frame.width/15) }
/// The mouth, effort and glow weight at `t` seconds after the downbeat.
func poseAt(_ t:Double)->(pose:SingerMouthPose,effort:Double,weight:Double,resting:Bool) {
 if let i=renders.indices.first(where:{t >= renders[$0].spanOffset-offset && t < renders[$0].spanOffset-offset+renders[$0].duration}) {
  let r=renders[i],local=t-(r.spanOffset-offset)
  return (r.articulation?.pose(at:local) ?? SingerViseme.rest.pose,r.articulation?.level(at:local) ?? 0,lineWeights[i],false)
 }
 return (SingerViseme.rest.pose,0,1,true)
}
/// The face asks for this on every display frame (SingerFaceView.articulationPose), so the
/// mouth, the effort and the lamp move on the display clock, read off the audio clock —
/// not off a 30 Hz timer. The face's own tick owns the beat phase (configure below);
/// writing it from here as well made the sway flicker between two clocks.
func framePose()->SingerMouthPose? {
 guard let f=face else{return nil}
 let p=poseAt(max(0,eg_time()))
 f.resting=p.resting;f.previewEffort=CGFloat(p.effort)
 lamp(level:p.effort,weight:p.weight)
 return p.pose
}
func pose(_ t:Double,captions:Bool=true) {
 progress?.fraction=CGFloat(min(1,max(0,t/duration)));progress?.needsDisplay=true
 guard captions else{return}
 // the sung word: the latest word that has started (a little early), until it has lingered
 if let w=words.indices.last(where:{words[$0].start-LEAD<=t}),t<words[w].end+LINGER {
  if w != shownWord {
   shownWord=w;litSyllable = -1
   caption.show(line:w,tokens:[words[w].token],accent:accent,size:captionSize())
   caption.slide(from:captionSize()*0.5);captionUp=true
  }
  let due=words[w].syllableStarts.lastIndex(where:{$0-0.02<=t}) ?? -1
  if due>litSyllable {for k in (litSyllable+1)...due {caption.highlight(line:w,syllable:k)};litSyllable=due}
 } else if captionUp {caption.hide(line:nil);captionUp=false}
}
func show(){
 let screen=NSScreen.screens.first ?? NSScreen.main!
 let p=StagePanel(contentRect:screen.frame,styleMask:[.borderless],backing:.buffered,defer:false)
 p.level = .screenSaver;p.collectionBehavior=[.canJoinAllSpaces,.stationary,.fullScreenAuxiliary];p.isOpaque=true;p.backgroundColor=accent;p.ignoresMouseEvents=true
 p.contentView=makeView(screen.frame.size);panel=p
 face?.articulationPose=framePose;face?.configure(epoch:epoch-offset,bpm:bpm,expression:0);face?.start()
 app.activate(ignoringOtherApps:true);p.makeKeyAndOrderFront(nil);p.orderFrontRegardless()
}
// ── stopping: Esc on this Mac, or a SIGTERM from a peer's Esc (launchctl remove) ──
var cancelled=false
func cancel(_ reason:String) {
 if cancelled {return};cancelled=true
 eg_stop();timer?.invalidate();panel?.orderOut(nil);caption.hide(line:nil);restoreBrightness()
 receipt("cancelled",["reason":reason])
 var procs:[Process]=[]
 if reason=="escape" || reason=="conductor" {
  for h in peers {
   let p=Process();p.executableURL=URL(fileURLWithPath:"/usr/bin/ssh")
   p.arguments=["-o","BatchMode=yes","-o","ConnectTimeout=3",h,"launchctl remove computer.aesthetic.eightgigabytes"]
   p.standardOutput=FileHandle.nullDevice;p.standardError=FileHandle.nullDevice
   try? p.run();procs.append(p)
  }
 }
 DispatchQueue.global().async { procs.forEach{$0.waitUntilExit()};exit(0) }
 DispatchQueue.main.asyncAfter(deadline:.now()+4){exit(0)}
}
var signalSources:[DispatchSourceSignal]=[]
for sig in [SIGTERM,SIGINT,SIGHUP] {
 signal(sig,SIG_IGN)
 let source=DispatchSource.makeSignalSource(signal:sig,queue:.main)
 source.setEventHandler{cancel("signal")};source.resume();signalSources.append(source)
}

func writePreview(_ path:String) {
 let w=1440,h=900;_ = makeView(NSSize(width:w,height:h));pose(duration*0.45,captions:false)
 let p=poseAt(duration*0.45);face!.previewPose=p.pose;face!.resting=p.resting
 face!.previewExpression(beat:(duration*0.45+offset)*bpm/60,effort:CGFloat(p.effort),breath:0)
 let bitmap=NSBitmapImageRep(bitmapDataPlanes:nil,pixelsWide:w,pixelsHigh:h,bitsPerSample:8,samplesPerPixel:4,hasAlpha:true,isPlanar:false,colorSpaceName:.deviceRGB,bytesPerRow:w*4,bitsPerPixel:32)!
 NSGraphicsContext.saveGraphicsState();NSGraphicsContext.current=NSGraphicsContext(bitmapImageRep:bitmap)
 face!.draw(face!.bounds);progress!.draw(progress!.bounds)
 if let word=words.last(where:{$0.start<=duration*0.45}) {
  let text=NSAttributedString(string:word.token.replacingOccurrences(of:"-",with:""),attributes:[.font:LyricCaption.font(CGFloat(w)/8),.foregroundColor:NSColor.black])
  text.draw(at:NSPoint(x:(CGFloat(w)-text.size().width)/2,y:CGFloat(h)*0.09))
 }
 NSGraphicsContext.restoreGraphicsState();try! bitmap.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:path))
}
receipt("preparing")
DispatchQueue.global(qos:.userInitiated).async {
 guard eg_load(folder+"/"+member+".tsv")==0 else{receipt("error",["reason":"Invalid instrument score"]);exit(1)}
 let start=Date()
 for (i,line) in sung.splitLines().enumerated() {
  guard let r=voice.renderSync(line,into:format),r.notesUsed==r.noteCount else{receipt("error",["reason":"Voice render failed","line":i]);exit(1)}
  let ns=notation[i],gain=ns["gain"] as! Double,pan=ns["pan"] as! Double
  guard eg_voice(r.buffer.floatChannelData![0],Int32(r.buffer.frameLength),r.spanOffset-offset,gain,pan)==0 else{receipt("error",["reason":"Vocal treatment failed"]);exit(1)}
  renders.append(r)
 }
 if check {
  try! FileManager.default.createDirectory(atPath:out,withIntermediateDirectories:true)
  FileManager.default.createFile(atPath:out+"/mix.f32",contents:nil)
  let file=try! FileHandle(forWritingTo:URL(fileURLWithPath:out+"/mix.f32"))
  let total=Int((duration*48000).rounded());var samples=[Float](repeating:0,count:1024),maxMs=0.0;var blockTimes:[Double]=[]
  for at in stride(from:0,to:total,by:512){let n=min(512,total-at),before=eg_cpu_time();eg_render(&samples,Int32(n));let ms=(eg_cpu_time()-before)*1000;maxMs=max(maxMs,ms);blockTimes.append(ms);samples.withUnsafeBytes{file.write(Data($0.prefix(n*8)))}}
  try! file.close()
  let proof:[String:Any]=["member":member,"voice":pinned,"phrases":renders.count,"words":words.count,"frames":total,"sampleRate":48000,"peak":eg_peak(),"maxBlockMs":maxMs,"timingMetric":"thread CPU time","p99BlockMs":blockTimes.sorted()[Int(Double(blockTimes.count)*0.99)],"blockBudgetMs":512.0/48,"prepareSeconds":Date().timeIntervalSince(start),"silent":true]
  try! JSONSerialization.data(withJSONObject:proof,options:.prettyPrinted).write(to:URL(fileURLWithPath:out+"/report.json"))
  DispatchQueue.main.async {if let preview{writePreview(preview)};receipt("ready",proof);exit(0)}
 } else {
  guard epoch-Date().timeIntervalSince1970>3 else{receipt("error",["reason":"Missed preparation deadline"]);exit(1)}
  eg_set_gain(config["outputGain"] as? Double ?? 1);eg_set_drive(config["driveDb"] as? Double ?? 0)
  let result=eg_start(epoch,cStatus);guard result==0 else{receipt("error",["reason":"AudioQueue start","code":result]);exit(1)}
  DispatchQueue.main.async {
   receipt("armed");var shown=false
   // Captions, progress and the clock run here; the face and the lamp run on the display link.
   timer=Timer.scheduledTimer(withTimeInterval:1.0/60,repeats:true){_ in
    if cancelled {return}
    let t=Date().timeIntervalSince1970-epoch
    // House lights: three seconds before the downbeat the screen eases down to the floor.
    if t >= -3 && baseBrightness==nil, let b=readBrightness() {
     baseBrightness=b;glow=1
     try? JSONSerialization.data(withJSONObject:["base":Double(brightHouse),"was":Double(b),"member":member]).write(to:URL(fileURLWithPath:brightnessPath))
    }
    // Esc must always work: the stage owns the keyboard while it is up. If another app
    // took the key window back, take it again (twice a second).
    if shown, let p=panel, !p.isKeyWindow, Int((t*60).rounded())%30==0 {app.activate(ignoringOtherApps:true);p.makeKeyAndOrderFront(nil)}
    if conductorPid>0, kill(conductorPid,0) != 0, errno==ESRCH {cancel("conductor");return}
    if !shown {lamp(level:0,weight:1)}
    if t>=0 && !shown {show();shown=true;receipt("playing")}
    if shown {pose(max(0,eg_time()))}
    if t>=duration+0.15{eg_stop();panel?.orderOut(nil);caption.hide(line:nil);restoreBrightness();receipt("complete");exit(0)}
   }
  }
 }
}
app.run()
