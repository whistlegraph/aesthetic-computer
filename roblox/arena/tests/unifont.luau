local font=require('../../shared/Unifont')
for _,cp in {0x41,0xE6,0xF8,0xE5,0x2603,0x4E2D,0xFFFD} do
    local glyph=font.glyph(cp)
    assert(glyph and #glyph.runs>0)
    for _,run in glyph.runs do assert(run[1]>=0 and run[1]+run[3]<=glyph.width and run[2]<16) end
end
assert(font.glyph(0x41).width==8)
assert(font.glyph(0x4E2D).width==16)
assert(font.glyph(0x1F600)==nil)
print('Unifont Latin, Danish, symbol, wide glyphs and missing-glyph checks passed')
