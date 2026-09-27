#!/usr/bin/env python3
"""Check the production caption bitmap cache without opening a window."""
from pathlib import Path
import subprocess,tempfile
source=(Path(__file__).resolve().parents[4]/'slab/menuband/Sources/MenuBand/LyricCaption.swift').read_text()
stubs='''
struct SingerFace {
 struct SimSlot { let index:Int; func tile(on screen:NSScreen)->NSRect {screen.frame} }
}
'''
checks='''
_ = NSApplication.shared
let glyph = CaptionGlyphLayer()
glyph.bounds = CGRect(x:0,y:0,width:80,height:90); glyph.contentsScale = 2
glyph.text = "doo"; glyph.font = LyricCaption.font(40); glyph.ink = .white
glyph.display()
let first = glyph.contents as! CGImage
assert(first.width == 160 && first.height == 180)
glyph.ink = .black; glyph.display()
let dark = glyph.contents as! CGImage
assert(first !== dark)
assert((first.dataProvider!.data! as Data) != (dark.dataProvider!.data! as Data))
glyph.ink = .white; glyph.display()
assert((glyph.contents as! CGImage) === first, "Repeated glyph state should reuse its GPU image")
let bitmap = NSBitmapImageRep(cgImage:first)
try bitmap.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:"/Users/jas/Shelf/macneopolitan-doowop/metal/caption-cache.png"))
print("Caption cache: retina dimensions, ink invalidation and bitmap reuse passed.")
'''
with tempfile.TemporaryDirectory() as tmp:
 p=Path(tmp)/'check.swift';p.write_text(source+stubs+checks)
 subprocess.run(['swift',str(p)],check=True)
