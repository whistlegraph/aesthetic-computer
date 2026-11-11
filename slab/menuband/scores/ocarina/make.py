"""Ocarina Songs: one .mbscore per song for Menu Band's ocarina instrument.

Keys are the Switch 2 ocarina buttons spelled on QWERTY: L R A Y X = D5 F5 A5
B5 D6. The game checks note order, not rhythm, so each note is one even beat
and the last is held. The printed cards (cards.pdf) come from the notepat-charts
renderer: `NotepatCharts cards.pdf --ocarina` (OcarinaCard.swift).
"""
import json, os

HERE = os.path.dirname(os.path.abspath(__file__))
K2M = {"L": 74, "R": 77, "A": 81, "Y": 83, "X": 86}
NAMES = {74: "D", 77: "F", 81: "A", 83: "B", 86: "D'"}
SONGS = [  # keep in step with ocarinaSongs in OcarinaCard.swift
    ("zeldas-lullaby", "Zelda's Lullaby", "Y X A Y X A"),
    ("eponas-song", "Epona's Song", "X Y A X Y A"),
    ("sarias-song", "Saria's Song", "R A Y R A Y"),
    ("suns-song", "Sun's Song", "A R X A R X"),
    ("song-of-time", "Song of Time", "A L R A L R"),
    ("song-of-storms", "Song of Storms", "L R X L R X"),
    ("minuet-of-forest", "Minuet of Forest", "L X Y A Y A"),
    ("bolero-of-fire", "Bolero of Fire", "R L R L A R A R"),
    ("serenade-of-water", "Serenade of Water", "L R A A Y"),
    ("nocturne-of-shadow", "Nocturne of Shadow", "Y A A L Y A R"),
    ("requiem-of-spirit", "Requiem of Spirit", "L R L A R L"),
    ("prelude-of-light", "Prelude of Light", "X A X A Y X"),
]
BPM = 96
OCARINA = 79  # General MIDI 80, Ocarina (0-based)

tracks = []
for slug, title, keys in SONGS:
    ks = keys.split()
    beats = [1] * len(ks)
    beats[-1] = 2
    notes = ",".join(f"{K2M[k]}:{b}" for k, b in zip(ks, beats))
    score = {
        "title": title, "composer": "Ocarina Songs for Menu Band's ocarina",
        "machines": 1, "bpm": BPM, "lead": 2.0, "timeSignature": "4/4",
        "description": f"type {keys.lower()} on the ocarina — " + " ".join(NAMES[K2M[k]] for k in ks),
        "voices": [{"name": "ocarina", "program": OCARINA, "velocity": 96, "notes": notes}],
    }
    json.dump(score, open(f"{HERE}/{slug}.mbscore", "w"), indent=2)
    tracks.append({"file": f"{slug}.mbscore", "title": title, "machines": 1, "bpm": BPM,
                   "seconds": round(sum(beats) * 60 / BPM, 1), "section": "Ocarina Songs"})
json.dump({"title": "Ocarina Songs",
           "description": "Twelve ocarina songs on L R A Y X. Set the instrument to Ocarina; the cards (cards.pdf) show the keys.",
           "tracks": tracks}, open(f"{HERE}/ocarina-songs.mbscore", "w"), indent=2)
print(f"wrote {len(tracks)} songs")
