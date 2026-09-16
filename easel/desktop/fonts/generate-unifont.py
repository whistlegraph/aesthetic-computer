#!/usr/bin/env python3
# Copyright (c) 2026 Aesthetic Computer. This font conversion script and its
# outputs are licensed under SIL OFL 1.1, see UNIFONT-OFL-1.1.txt.
"""Exact GNU Unifont terminal subset; build-only dependency fonttools==4.65.0."""
import gzip, hashlib, json, sys
from pathlib import Path
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import TTFont, newTable
HERE=Path(__file__).resolve().parent
SOURCE=HERE/'unifont-16.0.03.bdf.gz'
STEM='ac-easel-unifont'

def source():
    text=gzip.open(SOURCE,'rt').read()
    copyright=next(line.split('"')[1] for line in text.splitlines() if line.startswith('COPYRIGHT '))
    result=[]
    for block in text.split('STARTCHAR ')[1:]:
        lines=[line.strip() for line in block.splitlines()]
        props={line.split(' ',1)[0]:line.split(' ',1)[1] for line in lines if ' ' in line}
        code=int(props['ENCODING'])
        if not (0x20<=code<=0x52f or 0x2000<=code<=0x2bff): continue
        width,height,x,y=map(int,props['BBX'].split())
        advance=int(props['DWIDTH'].split()[0])
        if width>8 or advance>8: continue
        start=lines.index('BITMAP')+1
        rows=[int(row,16) for row in lines[start:start+height]]
        pixels=[(x+col,y+height-row-1) for row,bits in enumerate(rows) for col in range(width)
                if bits & (1<<(((width+7)//8)*8-col-1))]
        result.append((code,advance,pixels))
    return copyright,result

def build():
    copyright,data=source()
    order=['.notdef']+[f'uni{c:04X}' for c,_,_ in data]
    fb=FontBuilder(1600,isTTF=True);fb.setupGlyphOrder(order)
    fb.setupCharacterMap({c:f'uni{c:04X}' for c,_,_ in data})
    glyphs={};metrics={}
    for name,advance,pixels in [('.notdef',8,[])]+[(f'uni{c:04X}',a,p) for c,a,p in data]:
        pen=TTGlyphPen(None)
        for x,y in pixels:
            pen.moveTo((100*x,100*y));pen.lineTo((100*x,100*(y+1)))
            pen.lineTo((100*(x+1),100*(y+1)));pen.lineTo((100*(x+1),100*y));pen.closePath()
        glyphs[name]=pen.glyph();metrics[name]=(100*advance,0)
    fb.setupGlyf(glyphs)
    for name in metrics:
        metrics[name]=(metrics[name][0],getattr(glyphs[name],'xMin',0))
    fb.setupHorizontalMetrics(metrics);fb.setupHorizontalHeader(ascent=1400,descent=-200,lineGap=0)
    fb.setupNameTable({'familyName':'AC Easel Unifont','styleName':'Regular','uniqueFontIdentifier':'AC-Easel-Unifont-16.0.03-Terminal-v1',
      'fullName':'AC Easel Unifont Regular','psName':'ACEaselUnifont-Regular','version':'Version 16.003',
      'copyright':copyright,'licenseDescription':'SIL Open Font License version 1.1; exact bitmap-outline terminal subset of GNU Unifont 16.0.03.',
      'licenseInfoURL':'https://unifoundry.com/OFL-1.1.txt'})
    fb.setupOS2(sTypoAscender=1400,sTypoDescender=-200,sTypoLineGap=0,usWinAscent=1400,usWinDescent=200,fsType=0,sxHeight=800,sCapHeight=1000)
    fb.setupPost(isFixedPitch=1);fb.setupMaxp()
    fb.font['head'].created=fb.font['head'].modified=3851280000;fb.font.recalcTimestamp=False
    gasp=newTable('gasp');gasp.gaspRange={65535:0};fb.font['gasp']=gasp
    fb.save(HERE/(STEM+'.ttf'));fb.font.flavor='woff';fb.save(HERE/(STEM+'.woff'))
    (HERE/'UNIFONT-COPYRIGHT.txt').write_text(copyright+'\n\nThis terminal subset and its conversion script are distributed under SIL OFL 1.1.\nThe surrounding Easel application has its own separate license.\nModified by Aesthetic Computer, 2026-09-15: terminal subset and exact square outlines.\n')
    files=['unifont-16.0.03.bdf.gz',STEM+'.ttf',STEM+'.woff']
    (HERE/'unifont-provenance.json').write_text(json.dumps({'source':'https://assets.aesthetic.computer/type/unifont-16.0.03.bdf.gz',
      'upstream':'https://unifoundry.com/pub/unifont/unifont-16.0.03/font-builds/unifont-16.0.03.bdf.gz',
      'version':'16.0.03','family':'AC Easel Unifont','glyphs':len(data),'license':'SIL OFL 1.1',
      'selection':'U+0020..052F and U+2000..2BFF; glyph width/advance <= 8; omitted glyphs use system fallback',
      'sha256':{name:hashlib.sha256((HERE/name).read_bytes()).hexdigest() for name in files}},indent=2)+'\n')

def check():
    font=TTFont(HERE/(STEM+'.ttf'));cmap=font.getBestCmap();_,data=source()
    assert font['head'].unitsPerEm==1600
    assert font['hhea'].ascent-font['hhea'].descent==1600
    for code,advance,pixels in data:
        name=cmap[code];glyph=font['glyf'][name]
        assert font['hmtx'][name][0]==advance*100
        assert glyph.numberOfContours==len(pixels)
        if pixels:
            expected=[(100*a,100*b) for x,y in pixels for a,b in [(x,y),(x,y+1),(x+1,y+1),(x+1,y)]]
            assert list(glyph.coordinates)==expected,hex(code)
    assert all(c in cmap for c in [0x40,0xE9,0x2500,0x2588,0x28ff])
    assert TTFont(HERE/(STEM+'.woff')).getBestCmap()==cmap
    print(f'Checked {len(cmap)} exact GNU Unifont glyphs; ASCII 8x16 at 16px; WOFF parity.')
if __name__=='__main__':
    if '--check' not in sys.argv:build()
    check()
