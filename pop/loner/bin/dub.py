# dub.py — Camille's voice, singing the line in another language.
#
# @jeffrey: "we have her consent · im thinking we can use it to translate
# the song across some languages · or have like a french verse or spanish
# or danish verse · similar to how we translated the goodiepal / prutti vid
# from danish to elevenlabs".
#
# Same move as the klokkentales reel (2026-08-13): the ElevenLabs Dubbing
# API is VOICE-PRESERVING — it does not read the line in a stock voice, it
# re-sings it in hers. There the direction was da→en on Prutti; here it is
# en→fr/es/da on Camille.
#
# THE GATE, which is the lane's and not mine: consent is on record
# (@jeffrey, 2026-08-19) and synthetic voice is ALWAYS labelled. Every
# output carries `-dub-<lang>` in its name and a line in .dub.json saying
# what made it, so a file can never drift loose from the fact that a
# machine sang it. Nothing here is presented as a take she performed.
#
# WHY `dub` ALONE DOES NOT WORK HERE, measured 2026-08-19: the Dubbing API
# returned status "dubbed" with no error for fr/es/da and handed back audio
# that is still English — because its transcript came back EMPTY. dubbing_v1
# transcribes speech, and this is 24 s of slow unaccompanied singing on held
# vowels, so it heard no words, had nothing to translate, and passed the
# source through. The klokkentales reel worked because Prutti was TALKING.
#
# So the route for a SONG is different, and it is better anyway: dubbing
# preserves the prosody of the source, and we do not want her English
# phrasing on a French verse — we want the French words on HER MELODY,
# which is the thing this lane's warp already does. Hence:
#
#   clone   an IVC from her SPOKEN slices (o-heres-loner, n-emo-again,
#           n-i-knew-it) — real speech, which is what an IVC wants
#   say     the translated lyric, spoken in that voice
#   then    hand it to align.py + halo3's chart, and she sings the
#           translation on the melody she wrote
#
#   ELEVENLABS_API_KEY=... pop/.venv/bin/python pop/loner/bin/dub.py clone
#   ELEVENLABS_API_KEY=... pop/.venv/bin/python pop/loner/bin/dub.py say fr
#   ELEVENLABS_API_KEY=... pop/.venv/bin/python pop/loner/bin/dub.py dub fr

import json, os, sys, time, urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
REPO = os.path.dirname(os.path.dirname(LANE))
VAULT = os.path.join(REPO, "aesthetic-computer-vault",
                     ".devcontainer", "envs", "devcontainer.env")
API = "https://api.elevenlabs.io/v1"
SOURCE = os.path.join(LANE, "samples", "f-whole-line.wav")   # the whole sentence
LANGS = {"fr": "French", "es": "Spanish", "da": "Danish", "de": "German",
         "it": "Italian", "pt": "Portuguese", "ja": "Japanese",
         "ru": "Russian", "hi": "Hindi"}

# The whole song is one sentence, so each translation is one sentence too.
# Kept literal rather than singable — the melody does the singing, and a
# rhyme scheme she never wrote would be us writing her lyric for her.
LYRIC = {
    "en": "sitting curled up in myself, i think of a stone, "
          "just waiting very patiently for time to pass",
    "fr": "assise recroquevillée en moi-même, je pense à une pierre, "
          "qui attend très patiemment que le temps passe",
    "es": "sentada acurrucada en mí misma, pienso en una piedra, "
          "que espera muy pacientemente a que pase el tiempo",
    "da": "jeg sidder krøllet sammen i mig selv, jeg tænker på en sten, "
          "der bare venter meget tålmodigt på at tiden går",
    "ru": "сижу, свернувшись в себе, я думаю о камне, "
          "который очень терпеливо ждёт, пока пройдёт время",
    "hi": "अपने भीतर सिमटी हुई बैठी हूँ, मैं एक पत्थर के बारे में सोचती हूँ, "
          "जो बहुत धीरज से समय के बीतने का इंतज़ार कर रहा है",
}

# her SPOKEN slices — an IVC trained on singing learns the singing, and
# then every line it speaks arrives on that melody
SPOKEN = ["o-heres-loner", "n-emo-again", "n-i-knew-it"]

# …but eight seconds is not a voice, it is an impression of one. @jeffrey:
# "these aren't using her voice · like we did with prutti". The Prutti clone
# had a whole reel behind it. `clone --all` uses the WHOLE archive instead:
# nineteen posts, 7.7 minutes, downsampled to fit the upload budget.
#
# ONE take is excluded and must stay excluded: 6988619239657622790 is the
# ensemble performance, with @jeffrey and Alex singing on it. Their voices
# are not Camille's and were not consented into this clone; a model trained
# on all three is not her.
ENSEMBLE = "6988619239657622790"
VOICE_NAME = "Camille Klein · loner IVC (consented)"


def key():
    k = os.environ.get("ELEVENLABS_API_KEY")
    if k:
        return k
    with open(VAULT) as f:
        for line in f:
            if line.startswith("ELEVENLABS_API_KEY="):
                return line.split("=", 1)[1].strip().strip("'\"")
    raise SystemExit("no ELEVENLABS_API_KEY (env or vault devcontainer.env)")


def post_multipart(url, k, fields, filepath):
    b = "----acdub"
    body = b""
    for name, val in fields.items():
        body += (f"--{b}\r\nContent-Disposition: form-data; name=\"{name}\"\r\n\r\n"
                 f"{val}\r\n").encode()
    with open(filepath, "rb") as f:
        data = f.read()
    body += (f"--{b}\r\nContent-Disposition: form-data; name=\"file\"; "
             f"filename=\"{os.path.basename(filepath)}\"\r\n"
             f"Content-Type: audio/wav\r\n\r\n").encode() + data + b"\r\n"
    body += f"--{b}--\r\n".encode()
    req = urllib.request.Request(url, data=body, headers={
        "xi-api-key": k, "Content-Type": f"multipart/form-data; boundary={b}"})
    with urllib.request.urlopen(req, timeout=300) as r:
        return json.load(r)


def get(url, k, raw=False, timeout=300):
    req = urllib.request.Request(url, headers={"xi-api-key": k})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.read() if raw else json.load(r)


def post_files(url, k, fields, paths):
    b = "----acdub"
    body = b""
    for name, val in fields.items():
        body += (f"--{b}\r\nContent-Disposition: form-data; name=\"{name}\"\r\n\r\n"
                 f"{val}\r\n").encode()
    for fp in paths:
        with open(fp, "rb") as f:
            data = f.read()
        body += (f"--{b}\r\nContent-Disposition: form-data; name=\"files\"; "
                 f"filename=\"{os.path.basename(fp)}\"\r\n"
                 f"Content-Type: audio/wav\r\n\r\n").encode() + data + b"\r\n"
    body += f"--{b}--\r\n".encode()
    req = urllib.request.Request(url, data=body, headers={
        "xi-api-key": k, "Content-Type": f"multipart/form-data; boundary={b}"})
    with urllib.request.urlopen(req, timeout=300) as r:
        return json.load(r)


def voice_path():
    return os.path.join(LANE, "vox-dub", ".voice.json")


def clone(k, whole_archive=False):
    if whole_archive:
        import glob, subprocess, tempfile
        src = sorted(glob.glob(os.path.join(LANE, "source", "*-48k.wav")))
        src = [p for p in src if ENSEMBLE not in os.path.basename(p)]
        tmp = tempfile.mkdtemp(prefix="acdub-")
        paths, total = [], 0.0
        for f in src:               # 22 kHz mono mp3 — the upload has a budget
            d = os.path.join(tmp, os.path.basename(f).replace("-48k.wav", ".mp3"))
            subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", f, "-ac", "1",
                            "-ar", "22050", "-b:a", "96k", d], check=True)
            paths.append(d)
            total += os.path.getsize(d)
        print(f"  {len(paths)} takes, {total / 1e6:.1f} MB — excluded the "
              f"ensemble take {ENSEMBLE} (@jeffrey and Alex are on it)")
    else:
        paths = [os.path.join(LANE, "samples", f"{n}.wav") for n in SPOKEN]
        paths = [p for p in paths if os.path.exists(p)]
    if not paths:
        raise SystemExit("no audio to clone from")
    print(f"→ cloning from {len(paths)} spoken slices: "
          f"{', '.join(os.path.basename(p) for p in paths)}")
    r = post_files(f"{API}/voices/add", k, {
        "name": VOICE_NAME,
        "description": "Consented IVC of Camille Klein (@cksuperstore) for the "
                       "loner remix. SYNTHETIC VOICE — label on any release.",
    }, paths)
    vid = r.get("voice_id")
    os.makedirs(os.path.join(LANE, "vox-dub"), exist_ok=True)
    json.dump(dict(voice_id=vid, name=VOICE_NAME,
                   sources=[os.path.basename(p) for p in paths],
                   synthetic=True,
                   consent="Camille Klein (@cksuperstore), via @jeffrey 2026-08-19"),
              open(voice_path(), "w"), indent=1)
    print(f"  ✓ voice_id {vid}  → {voice_path()}")
    return vid


def say(k, lang):
    vp = voice_path()
    if not os.path.exists(vp):
        raise SystemExit("no voice yet — run: dub.py clone")
    vid = json.load(open(vp))["voice_id"]
    text = LYRIC[lang]
    body = json.dumps({
        "text": text, "model_id": "eleven_multilingual_v2",
        "voice_settings": {"stability": 0.55, "similarity_boost": 0.85,
                           "style": 0.20, "use_speaker_boost": True},
    }).encode()
    req = urllib.request.Request(
        f"{API}/text-to-speech/{vid}", data=body,
        headers={"xi-api-key": k, "Content-Type": "application/json",
                 "Accept": "audio/mpeg"})
    with urllib.request.urlopen(req, timeout=300) as r:
        audio = r.read()
    out_dir = os.path.join(LANE, "vox-dub")
    os.makedirs(out_dir, exist_ok=True)
    dest = os.path.join(out_dir, f"spoken-{lang}.mp3")
    open(dest, "wb").write(audio)
    print(f"  ✓ {LANGS[lang]}: {dest}  ({len(audio) // 1024} KB)")
    print(f"    “{text}”")
    return dest


# A native speaker of each language, to PERFORM the line before her voice
# is mapped onto it. Which premade voice matters less than that it is
# female and unhurried — speech-to-speech transfers timbre, not timing, so
# whatever phrasing this voice chooses is the phrasing she will have.
CARRIER = "XrExE9yKIg1WjnnlVkGX"          # Matilda — even, unhurried


def sts(k, lang):
    """TTS the translation with a carrier voice, then wear her timbre.

    Better than TTS straight from the IVC because the model only has to
    move TIMBRE — the prosody is already a real reading of a real French
    sentence, rather than an English-trained clone guessing at one. It is
    also what the Prutti dub was doing underneath.
    """
    vp = voice_path()
    if not os.path.exists(vp):
        raise SystemExit("no voice yet — run: dub.py clone --all")
    vid = json.load(open(vp))["voice_id"]

    body = json.dumps({
        "text": LYRIC[lang], "model_id": "eleven_multilingual_v2",
        "voice_settings": {"stability": 0.60, "similarity_boost": 0.75,
                           "style": 0.0, "use_speaker_boost": True},
    }).encode()
    req = urllib.request.Request(
        f"{API}/text-to-speech/{CARRIER}", data=body,
        headers={"xi-api-key": k, "Content-Type": "application/json",
                 "Accept": "audio/mpeg"})
    with urllib.request.urlopen(req, timeout=300) as r:
        carrier = r.read()

    out_dir = os.path.join(LANE, "vox-dub")
    os.makedirs(out_dir, exist_ok=True)
    carrier_path = os.path.join(out_dir, f".carrier-{lang}.mp3")
    open(carrier_path, "wb").write(carrier)

    b = "----acsts"
    fields = {"model_id": "eleven_multilingual_sts_v2",
              "voice_settings": json.dumps({"stability": 0.45,
                                            "similarity_boost": 0.90,
                                            "style": 0.0,
                                            "use_speaker_boost": True})}
    payload = b""
    for name, val in fields.items():
        payload += (f"--{b}\r\nContent-Disposition: form-data; name=\"{name}\"\r\n\r\n"
                    f"{val}\r\n").encode()
    payload += (f"--{b}\r\nContent-Disposition: form-data; name=\"audio\"; "
                f"filename=\"carrier.mp3\"\r\nContent-Type: audio/mpeg\r\n\r\n"
                ).encode() + carrier + b"\r\n"
    payload += f"--{b}--\r\n".encode()
    req = urllib.request.Request(
        f"{API}/speech-to-speech/{vid}", data=payload,
        headers={"xi-api-key": k, "Accept": "audio/mpeg",
                 "Content-Type": f"multipart/form-data; boundary={b}"})
    with urllib.request.urlopen(req, timeout=600) as r:
        audio = r.read()
    dest = os.path.join(out_dir, f"sts-{lang}.mp3")
    open(dest, "wb").write(audio)
    print(f"  ✓ {LANGS[lang]}: {dest}  ({len(audio) // 1024} KB)")
    return dest


def scribe(k, lang):
    """Word timestamps for a translation — the shape of ITS words.

    @jeffrey: "per language we should be able to map the shape of the
    words etc". singdub was finding syllables in the audio with an energy
    detector, which is guessing; here the text is already known, so the
    words come back labelled and a French verse can be charted word by
    word the way the English one was. Same ElevenLabs scribe step the
    klokkentales reel used for its karaoke subs.
    """
    # `lang` may also name a SLICE — scribing her own takes is the same
    # job as scribing a translation, and singdub needs word spans for any
    # take it is asked to put on the chart.
    slice_wav = os.path.join(LANE, "samples", f"{lang}.wav")
    src = os.path.join(LANE, "vox-dub", f"sts-{lang}.mp3")
    if os.path.exists(slice_wav):
        src = slice_wav
    elif not os.path.exists(src):
        raise SystemExit(f"no sts-{lang}.mp3 and no samples/{lang}.wav")
    b = "----acscribe"
    code = lang if lang in LANGS else "en"
    fields = {"model_id": "scribe_v1", "language_code": code,
              "timestamps_granularity": "word", "diarize": "false"}
    payload = b""
    for name, val in fields.items():
        payload += (f"--{b}\r\nContent-Disposition: form-data; name=\"{name}\"\r\n\r\n"
                    f"{val}\r\n").encode()
    with open(src, "rb") as f:
        data = f.read()
    mime = "audio/wav" if src.endswith(".wav") else "audio/mpeg"
    payload += (f"--{b}\r\nContent-Disposition: form-data; name=\"file\"; "
                f"filename=\"{os.path.basename(src)}\"\r\n"
                f"Content-Type: {mime}\r\n\r\n").encode() + data + b"\r\n"
    payload += f"--{b}--\r\n".encode()
    req = urllib.request.Request(
        f"{API}/speech-to-text", data=payload,
        headers={"xi-api-key": k,
                 "Content-Type": f"multipart/form-data; boundary={b}"})
    with urllib.request.urlopen(req, timeout=600) as r:
        res = json.load(r)

    words = [w for w in res.get("words", []) if w.get("type") == "word"]
    out = os.path.join(LANE, "vox-dub", ".words.json")
    allw = json.load(open(out)) if os.path.exists(out) else {}
    allw[lang] = [dict(t=w["text"], start=round(w["start"], 3),
                       end=round(w["end"], 3)) for w in words]
    json.dump(allw, open(out, "w"), indent=1, ensure_ascii=False)
    print(f"  ✓ {LANGS.get(lang, lang)}: {len(words)} words → {out}")
    print("    " + " ".join(w["text"] for w in words))
    return allw[lang]


def main():
    argv = sys.argv[1:]
    k = key()
    if argv and argv[0] == "clone":
        clone(k, whole_archive="--all" in argv); return
    if argv and argv[0] == "scribe":
        for lang in argv[1:] or ["fr"]:
            scribe(k, lang)
        return
    if argv and argv[0] == "sts":
        for lang in [a for a in argv[1:] if a in LANGS] or ["fr"]:
            sts(k, lang)
        return
    if argv and argv[0] == "say":
        for lang in [a for a in argv[1:] if a in LANGS] or ["fr"]:
            say(k, lang)
        return
    langs = [a for a in argv if a in LANGS] or ["fr"]
    k = key()
    out_dir = os.path.join(LANE, "vox-dub")
    os.makedirs(out_dir, exist_ok=True)
    receipt_path = os.path.join(out_dir, ".dub.json")
    receipt = json.load(open(receipt_path)) if os.path.exists(receipt_path) else {}

    for lang in langs:
        print(f"→ {LANGS[lang]} ({lang}) — dubbing {os.path.basename(SOURCE)}")
        job = post_multipart(f"{API}/dubbing", k, {
            "target_lang": lang, "source_lang": "en", "num_speakers": "1",
            "name": f"loner-{lang}",
        }, SOURCE)
        did = job.get("dubbing_id")
        if not did:
            print(f"  ! no dubbing_id: {job}"); continue

        for _ in range(120):                       # ~10 min ceiling
            st = get(f"{API}/dubbing/{did}", k)
            s = st.get("status")
            if s == "dubbed":
                break
            if s == "failed":
                print(f"  ! failed: {st.get('error')}"); did = None; break
            time.sleep(5)
        if not did:
            continue

        audio = get(f"{API}/dubbing/{did}/audio/{lang}", k, raw=True)
        dest = os.path.join(out_dir, f"whole-line-dub-{lang}.mp3")
        open(dest, "wb").write(audio)
        # …and the label travels with the file, not just in a README
        receipt[lang] = dict(
            language=LANGS[lang], source=os.path.relpath(SOURCE, LANE),
            dubbing_id=did, engine="elevenlabs/dubbing",
            synthetic=True, consent="Camille Klein (@cksuperstore), via @jeffrey",
            note="SYNTHETIC VOICE — her voice model singing a translation; "
                 "not a take she performed. Label on any release.")
        json.dump(receipt, open(receipt_path, "w"), indent=1, sort_keys=True)
        print(f"  ✓ {dest}  ({len(audio) // 1024} KB)")

    print(f"\nreceipt: {receipt_path}")


if __name__ == "__main__":
    main()
