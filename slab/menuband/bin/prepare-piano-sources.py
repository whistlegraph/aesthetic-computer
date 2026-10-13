#!/usr/bin/env python3
"""Build the two CC0 piano previews from pinned VCSL recordings.

Uses the host ffmpeg QoS shim. Writes mono 48 kHz float32 anchors, three
seconds each (matching the existing Salamander bank), with a 10 ms tail fade.
One gain per bank matches its middle-register attack RMS to Salamander.
"""
import array
import concurrent.futures
import hashlib
import json
import math
from pathlib import Path
import re
import statistics
import subprocess
import urllib.parse
import urllib.request

REVISION = "c1ea7bcc3c7309650ab0da9d15c9cd1fbc4a4c7e"
ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / "slab/menuband/Sources/MenuBand/Resources/piano-sources"
CACHE = Path.home() / ".cache/menuband-pianos"
BASE = f"https://raw.githubusercontent.com/sgossner/VCSL/{REVISION}/"
CONFIG = [
    ("steinway", "Steinway B", r"Grand Piano, Steinway B/.*JHPiano_NoSus_Close_.*_vl3_rr1\.wav$", 0, 42),
    ("yamaha", "Yamaha Upright", r"Upright Piano, Yamaha/.*Upright1_Sus_.*_vl2_rr1\.wav$", 12, 13),
]


def read_url(url):
    request = urllib.request.Request(url, headers={"User-Agent": "MenuBand-sample-preparation"})
    with urllib.request.urlopen(request, timeout=90) as response:
        return response.read()


def midi_of(path):
    note = re.search(r"_([A-G])(#?)(\d)_", Path(path).name)
    return 12 * (int(note[3]) + 1) + dict(C=0, D=2, E=4, F=5, G=7, A=9, B=11)[note[1]] + bool(note[2])


def rms(frames):
    attack = frames[:24_000]
    return math.sqrt(sum(x * x for x in attack) / len(attack))


def prepare(job):
    prefix, shift, node = job
    url = BASE + urllib.parse.quote(node["path"])
    cached = CACHE / (node["sha"] + ".wav")
    data = cached.read_bytes() if cached.exists() else read_url(url)
    blob = hashlib.sha1(f"blob {len(data)}\0".encode() + data).hexdigest()
    assert blob == node["sha"], f"Source checksum mismatch: {url}"
    if not cached.exists():
        cached.write_bytes(data)
    frames = array.array("f", subprocess.check_output([
        "ffmpeg", "-v", "error", "-threads", "1", "-i", str(cached),
        "-ac", "1", "-ar", "48000", "-t", "3", "-f", "f32le", "-",
    ]))
    assert frames and all(math.isfinite(x) for x in frames)
    for i in range(480):
        frames[len(frames) - 480 + i] *= (479 - i) / 480
    midi = midi_of(node["path"]) + shift
    return prefix, midi, frames, dict(midi=midi, original=node["path"], url=url,
                                    original_sha256=hashlib.sha256(data).hexdigest())


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    CACHE.mkdir(parents=True, exist_ok=True)
    tree = json.loads(read_url(f"https://api.github.com/repos/sgossner/VCSL/git/trees/{REVISION}?recursive=1"))["tree"]
    jobs = []
    for prefix, name, pattern, shift, count in CONFIG:
        files = [node for node in tree if re.search(pattern, node["path"])]
        assert len(files) == count, (prefix, len(files))
        jobs += [(prefix, shift, node) for node in files]
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        results = list(pool.map(prepare, jobs))
    reference = []
    for path in (ROOT / "fedac/native/samples/piano").glob("*.raw"):
        if 48 <= int(path.stem) <= 84:
            reference.append(rms(array.array("f", path.read_bytes())))
    target = statistics.median(reference)
    manifest = dict(library="Versilian Community Sample Library", author="Versilian Studios LLC",
                    license="CC0-1.0", revision=REVISION, rate=48000, channels=1,
                    preparation="First 3 seconds, 10 ms tail fade; one attack-RMS gain per bank", banks=[])
    for prefix, name, _, shift, count in CONFIG:
        bank = sorted((r for r in results if r[0] == prefix), key=lambda r: r[1])
        measured = statistics.median(rms(r[2]) for r in bank if 48 <= r[1] <= 84)
        peak = max(abs(x) for r in bank for x in r[2])
        gain = min(target / measured, 0.95 / peak)
        records = []
        for _, midi, frames, record in bank:
            frames = array.array("f", (x * gain for x in frames))
            path = OUT / f"piano-{prefix}-{midi}.raw"
            data = frames.tobytes()
            path.write_bytes(data)
            record.update(file=path.name, sha256=hashlib.sha256(data).hexdigest(), frames=len(frames))
            records.append(record)
        manifest["banks"].append(dict(id=prefix, name=name, gain=gain, filename_pitch_shift=shift, anchors=records))
        print(f"{name}: {count} anchors, MIDI {bank[0][1]}–{bank[-1][1]}, gain {gain:.3f}", flush=True)
    (OUT / "piano-sources.json").write_text(json.dumps(manifest, indent=2) + "\n")
    (OUT / "Piano-source-credits.txt").write_text(
        "Steinway B and Yamaha Upright recordings: Versilian Studios LLC, VCSL (CC0-1.0).\n"
        "https://github.com/sgossner/VCSL\nRevision: " + REVISION + "\n"
        "The included VCSL-CC0.txt applies to these recordings.\n"
        "Prepared as mono 48 kHz float32, one velocity layer, three-second decays.\n"
        "Original paths, source and prepared checksums, pitch mapping, and gains: piano-sources.json.\n")


if __name__ == "__main__":
    main()
