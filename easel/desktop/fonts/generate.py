#!/usr/bin/env python3
"""Rebuild exact 6x10 pixel outlines. Requires fonttools==4.65.0 (build only)."""
import hashlib
import json
from pathlib import Path
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import TTFont, newTable

HERE = Path(__file__).resolve().parent
SOURCE = HERE / '6x10.bdf'
SCALE = 100

def read_bdf():
    glyphs = []
    for block in SOURCE.read_text().split('STARTCHAR ')[1:]:
        lines = block.splitlines()
        props = {line.split(' ', 1)[0]: line.split(' ', 1)[1] for line in lines if ' ' in line}
        code = int(props['ENCODING'])
        if code < 0 or code > 0x10FFFF:
            continue
        width, height, x, y = map(int, props['BBX'].split())
        start = lines.index('BITMAP') + 1
        rows = [int(row, 16) for row in lines[start:start + height]]
        pixels = [(x + col, y + height - row - 1)
                  for row, bits in enumerate(rows)
                  for col in range(width)
                  if bits & (1 << (((width + 7) // 8) * 8 - col - 1))]
        glyphs.append((code, pixels))
    return glyphs

def build():
    data = read_bdf()
    order = ['.notdef'] + [f'uni{code:04X}' for code, _ in data]
    font = FontBuilder(1000, isTTF=True)
    font.setupGlyphOrder(order)
    font.setupCharacterMap({code: f'uni{code:04X}' for code, _ in data})
    glyphs = {}
    for name, pixels in [('.notdef', [])] + [(f'uni{code:04X}', pixels) for code, pixels in data]:
        pen = TTGlyphPen(None)
        for x, y in pixels:
            # Clockwise contours, one exact source pixel per square.
            pen.moveTo((x*SCALE,y*SCALE))
            pen.lineTo((x*SCALE,(y+1)*SCALE))
            pen.lineTo(((x+1)*SCALE,(y+1)*SCALE))
            pen.lineTo(((x+1)*SCALE,y*SCALE))
            pen.closePath()
        glyphs[name] = pen.glyph()
    font.setupGlyf(glyphs)
    font.setupHorizontalMetrics({name: (600, glyphs[name].xMin if hasattr(glyphs[name], 'xMin') else 0) for name in order})
    font.setupHorizontalHeader(ascent=800, descent=-200, lineGap=0)
    font.setupNameTable({'familyName':'AC Easel Pixel','styleName':'Regular',
        'uniqueFontIdentifier':'AC-Easel-Pixel-6x10-v1','fullName':'AC Easel Pixel Regular',
        'psName':'ACEaselPixel-Regular','version':'Version 1.000',
        'copyright':'Public domain terminal emulator font. Share and enjoy.',
        'licenseDescription':'Public domain; exact pixel-outline conversion of Misc Fixed 6x10.',
        'licenseInfoURL':'https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html'})
    font.setupOS2(sTypoAscender=800,sTypoDescender=-200,sTypoLineGap=0,usWinAscent=800,usWinDescent=200,fsType=0,sxHeight=500,sCapHeight=700)
    font.setupPost(isFixedPitch=1)
    font.setupMaxp()
    font.font['head'].created = font.font['head'].modified = 3851280000
    font.font.recalcTimestamp = False
    gasp = newTable('gasp')
    gasp.gaspRange = {65535: 0}  # Preserve hard pixel edges where honored.
    font.font['gasp'] = gasp
    font.save(HERE / 'ac-easel-pixel.ttf')
    font.font.flavor = 'woff'
    font.save(HERE / 'ac-easel-pixel.woff')
    hashes = {name: hashlib.sha256((HERE/name).read_bytes()).hexdigest()
              for name in ['6x10.bdf','ac-easel-pixel.ttf','ac-easel-pixel.woff']}
    (HERE/'provenance.json').write_text(json.dumps({
        'source':'https://www.cl.cam.ac.uk/~mgk25/download/ucs-fonts.tar.gz',
        'upstreamFile':'6x10.bdf','upstreamRevision':'1.35, 2006-01-05',
        'license':'Public domain terminal emulator font. Share and enjoy.',
        'family':'AC Easel Pixel','glyphs':len(data),'sha256':hashes},indent=2)+'\n')

def check():
    font = TTFont(HERE/'ac-easel-pixel.ttf')
    assert font['head'].unitsPerEm == 1000
    assert font['post'].isFixedPitch == 1
    assert all(width == 600 for width,_ in font['hmtx'].metrics.values())
    assert font['hhea'].ascent - font['hhea'].descent == 1000
    cmap = font.getBestCmap()
    for code, pixels in read_bdf():
        glyph = font['glyf'][cmap[code]]
        assert glyph.numberOfContours == len(pixels), hex(code)
        if pixels:
            coords = list(glyph.coordinates)
            expected = [(a*SCALE,b*SCALE) for x,y in pixels
                        for a,b in [(x,y),(x,y+1),(x+1,y+1),(x+1,y)]]
            assert coords == expected, hex(code)
    assert all(code in cmap for code in [ord('E'),ord('@'),0xE9,0x2500,0x2588])
    assert TTFont(HERE/'ac-easel-pixel.woff').getBestCmap() == cmap
    print(f'Checked {len(cmap)} glyphs: exact BDF pixels, fixed cell metrics, Unicode, TTF/WOFF parity.')

if __name__ == '__main__':
    import sys
    if '--check' not in sys.argv:
        build()
    check()
