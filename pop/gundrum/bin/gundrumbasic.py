"""Gundrumbasic: the release files of gundrum and bass 2.

The stamp and the cuckoo are voices of the score itself (bin/cuckoo-stamp.py, then
bin/gundrum-and-bass-2.mjs), so this only names the render and puts the cover on it:
out/gundrumbasic.wav (24-bit) and out/gundrumbasic.mp3 (320 k, cover embedded, 1 dB under).

  python bin/gundrumbasic.py
"""
from pathlib import Path
import shutil
import subprocess

HERE = Path(__file__).resolve().parent.parent
OUT = HERE / 'out'
COVER = HERE / 'cover/gundrumbasic-cover-1400.jpg'

shutil.copyfile(OUT / 'gundrum-and-bass-2.wav', OUT / 'gundrumbasic.wav')
subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', OUT / 'gundrumbasic.wav', '-i', COVER, '-map', '0:a', '-map', '1',
                '-af', 'volume=-1dB', '-c:a', 'libmp3lame', '-b:a', '320k', '-c:v', 'copy', '-id3v2_version', '3',
                '-metadata', 'title=Gundrumbasic', '-metadata', 'artist=Aesthetic Dot Computer',
                '-metadata:s:v', 'title=Album cover', '-metadata:s:v', 'comment=Cover (front)', OUT / 'gundrumbasic.mp3'],
               check=True)
print('wrote', OUT / 'gundrumbasic.wav', 'and', OUT / 'gundrumbasic.mp3')
