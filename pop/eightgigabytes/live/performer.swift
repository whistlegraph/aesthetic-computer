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
func pose(_ t:Double,captions:Bool=true) {
 guard let f=face else{return}
 progress?.fraction=CGFloat(min(1,max(0,t/duration)));progress?.needsDisplay=true
 if let index=renders.indices.first(where:{t >= renders[$0].spanOffset-offset && t < renders[$0].spanOffset-offset+renders[$0].duration}) {
  let r=renders[index],local=t-(r.spanOffset-offset)
  f.previewPose=r.articulation?.pose(at:local);f.resting=false
  f.previewExpression(beat:(t+offset)*bpm/60,effort:CGFloat(r.articulation?.level(at:local) ?? 0),breath:0)
 } else {
  f.previewPose=SingerViseme.rest.pose;f.resting=true
  f.previewExpression(beat:(t+offset)*bpm/60,effort:0,breath:0)
 }
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
 p.contentView=makeView(screen.frame.size);panel=p;face?.start()
 app.activate(ignoringOtherApps:true);p.makeKeyAndOrderFront(nil);p.orderFrontRegardless()
}
// ── stopping: Esc on this Mac, or a SIGTERM from a peer's Esc (launchctl remove) ──
var cancelled=false
func cancel(_ reason:String) {
 if cancelled {return};cancelled=true
 eg_stop();timer?.invalidate();panel?.orderOut(nil);caption.hide(line:nil)
 receipt("cancelled",["reason":reason])
 var procs:[Process]=[]
 if reason=="escape" {
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
signal(SIGTERM,SIG_IGN)
let termSource=DispatchSource.makeSignalSource(signal:SIGTERM,queue:.main)
termSource.setEventHandler{cancel("signal")};termSource.resume()

func writePreview(_ path:String) {
 let w=1440,h=900;_ = makeView(NSSize(width:w,height:h));pose(duration*0.45,captions:false)
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
   timer=Timer.scheduledTimer(withTimeInterval:1.0/30,repeats:true){_ in
    if cancelled {return}
    let t=Date().timeIntervalSince1970-epoch
    if t>=0 && !shown {show();shown=true;receipt("playing")}
    if shown {pose(max(0,eg_time()))}
    if t>=duration+0.15{eg_stop();panel?.orderOut(nil);caption.hide(line:nil);receipt("complete");exit(0)}
   }
  }
 }
}
app.run()
