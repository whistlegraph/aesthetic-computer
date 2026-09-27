#!/usr/bin/env python3
"""Run on Neo with path to adapter; preserves all unrelated source edits."""
from pathlib import Path
import sys, datetime

root=Path.home()/'aesthetic-computer'
source=root/'xbox/live/oskiewar.js'
stage=root/'xbox/tools/oskiewar-stage-mcp.mjs'
adapter=Path(sys.argv[1]).read_text()
stamp=datetime.datetime.now().strftime('%Y%m%d%H%M%S')
text=source.read_text()
if '// NOTEPAT_SCORE_VISUAL_V1_BEGIN' not in text:
    old="  if (stage.curtain) { drawCurtainDirections(stage); return true; }\n  const music = stage.performance;"
    new="  const music = stage.performance;\n  const notepatPlaying = music?.visual === 'notepat-score-v1' && music.playing && music.look && music.movement;\n  if (stage.curtain && !notepatPlaying) { drawCurtainDirections(stage); return true; }"
    if text.count(old)!=1: raise SystemExit('Curtain branch changed; inspect before patching')
    text=text.replace(old,new)
    old="  if (music.dance === 'femrag-round-v1') drawFemragDance(music, elapsed);"
    new="  if (music.visual === 'notepat-score-v1' && music.playing) drawNotepatScore(music, elapsed);\n  else if (music.dance === 'femrag-round-v1') drawFemragDance(music, elapsed);"
    if text.count(old)!=1: raise SystemExit('Visual branch changed; inspect before patching')
    text=text.replace(old,new)
    source.with_suffix('.js.before-notepat-'+stamp).write_text(source.read_text())
    source.write_text(text+'\n'+adapter)
else:
    start=text.index('// NOTEPAT_SCORE_VISUAL_V1_BEGIN')
    end=text.index('// NOTEPAT_SCORE_VISUAL_V1_END',start)+len('// NOTEPAT_SCORE_VISUAL_V1_END')
    source.write_text(text[:start]+adapter.rstrip()+text[end:])
text=stage.read_text()
old="p?.playing && p.dance==='femrag-round-v1' && Number.isFinite(p.elapsed)"
new="p?.playing && (p.dance==='femrag-round-v1' || p.visual==='notepat-score-v1') && Number.isFinite(p.elapsed)"
if old in text:
    stage.with_suffix('.mjs.before-notepat-'+stamp).write_text(text)
    stage.write_text(text.replace(old,new))
elif new not in text: raise SystemExit('Stage feed filter changed; inspect before patching')
print('Patched Notepat visual while preserving curtain; backups:',stamp)
