#!/usr/bin/env python3
"""Render all production face poses through Metal; benchmark real presentation."""
from pathlib import Path
import subprocess,tempfile,sys
repo=Path(__file__).resolve().parents[4]
source=repo/'slab/menuband/Sources/MenuBand'
out=Path(sys.argv[sys.argv.index('--out')+1] if '--out' in sys.argv else '/Users/jas/Shelf/macneopolitan-doowop/metal');out.mkdir(exist_ok=True,parents=True)
checks=r'''
import AppKit
let app = NSApplication.shared
app.setActivationPolicy(.accessory)
enum LyricCaption { static func font(_ size: CGFloat) -> NSFont { NSFont.systemFont(ofSize:size,weight:.bold) } }
let output = CommandLine.arguments[1]
let bounds = NSRect(x:0,y:0,width:960,height:600)
let surface = SingerFaceMetalView(metalFrame:bounds)!
let face = SingerFaceView(frame:bounds)
face.member = "neo"; face.accent = NSColor(srgbRed:143.0/255,green:209.0/255,blue:63.0/255,alpha:1)
face.previewPose = SingerViseme.rest.pose
func positions(beat:Double,energy:CGFloat) -> [SIMD2<Float>] {
 face.previewExpression(beat:beat,effort:energy,breath:0)
 return face.metalPreview().vertices.map { $0.position }
}
let quietA = positions(beat:2,energy:0), quietB = positions(beat:6,energy:0)
let quietTravel = zip(quietA,quietB).map { simd_length($0-$1) }.max()!
assert(quietTravel > 0 && quietTravel < 4, "Quiet face should breathe gently up close")
assert(positions(beat:2,energy:1) != positions(beat:6,energy:1), "Loud face must move")
print("Intimate breathing and larger musical movement checks passed.")
for beat in stride(from:0.0,to:16.0,by:0.5) {
 face.previewPose = SingerViseme.ah.pose
 face.previewExpression(beat:beat,effort:1,breath:1)
 let ink = face.metalPreview().vertices.filter { $0.color.x == 0 && $0.color.y == 0 && $0.color.z == 0 }
 assert(ink.allSatisfy { $0.position.y < 588 && $0.position.y > 0 }, "Extreme expression needs headroom for live bob")
}
print("Full-intensity camera/breath headroom checked across 16 beats.")
var maxVertices = 0
for (i,shape) in SingerViseme.allCases.enumerated() {
 face.previewPose = shape.pose
 face.previewExpression(beat:Double(i)/2,effort:CGFloat(i%3)/2,breath:i%3 == 0 ? 0.7:0)
 let mesh = face.metalPreview()
 assert(!mesh.vertices.isEmpty && mesh.vertices.count%3 == 0)
 assert(mesh.vertices.allSatisfy { $0.position.x.isFinite && $0.position.y.isFinite })
 maxVertices = max(maxVertices,mesh.vertices.count)
 let bitmap = surface.snapshot(mesh,width:960,height:600)!
 try bitmap.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:output+"/"+shape.rawValue+".png"))
}
for (member,color) in [("blueberry",NSColor(srgbRed:90.0/255,green:87.0/255,blue:211.0/255,alpha:1)),("frisbee",NSColor(srgbRed:242.0/255,green:167.0/255,blue:185.0/255,alpha:1))] {
 face.member = member; face.accent = color
 for shape in [SingerViseme.ah,.oo,.hum] {
  face.previewPose = shape.pose
  face.previewExpression(beat:1,effort:1,breath:0.7)
  let bitmap = surface.snapshot(face.metalPreview(),width:960,height:600)!
  try bitmap.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:output+"/"+member+"-"+shape.rawValue+".png"))
 }
}
for (member,color) in [("neo",NSColor(srgbRed:143.0/255,green:209.0/255,blue:63.0/255,alpha:1)),("blueberry",NSColor(srgbRed:90.0/255,green:87.0/255,blue:211.0/255,alpha:1)),("frisbee",NSColor(srgbRed:242.0/255,green:167.0/255,blue:185.0/255,alpha:1))] {
 face.member=member; face.accent=color
 var levels: [CGFloat] = []
 for (name,energy) in [("still",CGFloat(0)),("soft",CGFloat(0.24)),("alive",CGFloat(1))] {
  face.previewPose = energy == 0 ? SingerViseme.rest.pose : SingerViseme.oh.pose
  face.previewExpression(beat:2,effort:energy,breath:0)
  let bitmap = surface.snapshot(face.metalPreview(),width:960,height:600)!
  let edge = bitmap.colorAt(x:4,y:4)!.usingColorSpace(.sRGB)!
  levels.append(edge.redComponent+edge.greenComponent+edge.blueComponent)
  try bitmap.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:output+"/"+member+"-"+name+".png"))
 }
 assert(levels[2] > levels[0]*1.1 && levels[1] > levels[0])
}
face.member = "neo"; face.accent = NSColor(srgbRed:143.0/255,green:209.0/255,blue:63.0/255,alpha:1)
print("Metal rendered all 15 visemes and member variants; maximum vertices:",maxVertices)
fflush(stdout)
if CommandLine.arguments.contains("--window") {
 let panel = NSPanel(contentRect:NSRect(x:80,y:80,width:960,height:600),styleMask:[.titled],backing:.buffered,defer:false)
 panel.title = "MenuBand Metal rendering check"
 panel.contentView = face
 face.previewPose = nil; face.previewEffort = nil
 face.configure(epoch:Date().timeIntervalSince1970,bpm:88,expression:0.82)
 face.articulationPose = {
  let t = CACurrentMediaTime()
  return SingerViseme.allCases[Int(t*4)%SingerViseme.allCases.count].pose
 }
 panel.orderFrontRegardless(); face.start()
 RunLoop.main.run(until:Date().addingTimeInterval(12))
 face.stop(); panel.orderOut(nil)
}
'''
with tempfile.TemporaryDirectory(prefix='metal-face-') as tmp:
 tmp=Path(tmp); main=tmp/'main.swift';binary=tmp/'check'
 main.write_text('\n'.join((source/n).read_text() for n in ['SingerArticulation.swift','SingerFaceMetal.swift','SingerFace.swift'])+'\n'+checks)
 subprocess.run(['swiftc','-O',str(main),'-o',str(binary)],check=True)
 subprocess.run([str(binary),str(out)]+([] if '--no-window' in sys.argv else ['--window']),check=True)
