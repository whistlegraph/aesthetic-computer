#!/usr/bin/env python3
"""tag.py — write the single's credits into an mp3 (ID3v2.4) and stamp the cover.
  pop/.venv/bin/python pop/sailor-song/bin/tag.py <in.mp3> <out.mp3> --cover cover/x.jpg [--version v118]
Credits: Sage sings and plays (performer); Gigi Perez wrote the song (composer + lyricist);
Aesthetic Dot Computer produced, engineered, mixed and arranged the remix.
"""
import argparse, shutil
from mutagen.id3 import ID3, ID3NoHeaderError, TIT2, TPE1, TPE2, TALB, TCOM, TEXT, TIPL, TMCL, TCON, TDRC, COMM, APIC, TPUB, TIT3
ap = argparse.ArgumentParser()
ap.add_argument("src"); ap.add_argument("dst")
ap.add_argument("--cover", required=True)
ap.add_argument("--version", default="")
ap.add_argument("--performer", default="Sage")
a = ap.parse_args()
shutil.copyfile(a.src, a.dst)
try: tags = ID3(a.dst)
except ID3NoHeaderError: tags = ID3()
tags.delete(a.dst); tags = ID3()
P, W, S = a.performer, "Gigi Perez", "Aesthetic Dot Computer"
tags.add(TIT2(encoding=3, text="Sailor Song"))
tags.add(TIT3(encoding=3, text="cover of Gigi Perez — remixed by Aesthetic Dot Computer"))
tags.add(TPE1(encoding=3, text=P))                     # artist / performer
tags.add(TPE2(encoding=3, text=P))                     # album artist
tags.add(TALB(encoding=3, text="Sailor Song"))
tags.add(TCOM(encoding=3, text=W))                     # composer
tags.add(TEXT(encoding=3, text=W))                     # lyricist
tags.add(TIPL(encoding=3, people=[["producer", S], ["engineer", S], ["mix", S], ["arranger", S]]))
tags.add(TMCL(encoding=3, people=[["vocals", P], ["nylon-string guitar", P]]))
tags.add(TPUB(encoding=3, text=S))
tags.add(TCON(encoding=3, text="Pop"))
tags.add(TDRC(encoding=3, text="2026"))
note = f"{P} sings and plays Gigi Perez's 'Sailor Song' (writer: Gigi Perez). Produced, engineered, mixed and arranged by Aesthetic Dot Computer" + (f" ({a.version})" if a.version else "") + "."
tags.add(COMM(encoding=3, lang="eng", desc="", text=note))
with open(a.cover, "rb") as f: tags.add(APIC(encoding=3, mime="image/jpeg", type=3, desc="Cover", data=f.read()))
tags.save(a.dst, v2_version=4)
print("✓", a.dst)
for k, v in ID3(a.dst).items():
    print(" ", k, (f"<{len(v.data)} bytes {v.mime}>" if k.startswith("APIC") else (v.people if hasattr(v, "people") else str(v))))
