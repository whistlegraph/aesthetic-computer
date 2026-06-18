#!/usr/bin/env python3
"""Two-machine menuband performance: a spoken skit, a count-in, then a little
four-movement Baroque suite played across neo + blueberry, each part on its own
GM voice (+ percussion), all locked to a shared UTC downbeat.

Self-contained: writes its own poster/clock helpers to /tmp on both machines,
measures each machine's offset from aesthetic.computer/api/clock (Cristian's
algorithm), and conducts the whole thing with sleeps. Run it in the background.

  neo  = local host (this machine)   voice: Daniel
  blueberry = ssh host               voice: Samantha
"""
import os, ssl, subprocess, sys, time, urllib.request
from datetime import datetime

REMOTE = "blueberry"
NEO_VOICE, BB_VOICE = "Daniel", "Samantha"
PLAY = "computer.aestheticcomputer.menuband.play"
SAY = "computer.aestheticcomputer.menuband.say"

POSTER = r'''import Foundation
let env = ProcessInfo.processInfo.environment
let name = env["MB_NAME"] ?? ""
var ui: [String: String] = [:]
if let kv = env["MB_KV"] {
    for pair in kv.split(separator: ";") {
        let p = pair.split(separator: "=", maxSplits: 1)
        if p.count == 2 { ui[String(p[0])] = String(p[1]) }
    }
}
DistributedNotificationCenter.default().postNotificationName(
    NSNotification.Name(name), object: nil,
    userInfo: ui.isEmpty ? nil : ui, deliverImmediately: true)
RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.3))
'''

OFFSET = r'''import urllib.request, ssl, time, sys
from datetime import datetime
ctx = ssl.create_default_context(); ctx.check_hostname=False; ctx.verify_mode=ssl.CERT_NONE
HDRS={"User-Agent":"Mozilla/5.0 (Macintosh) ac-clock-sync"}
URL="https://aesthetic.computer/api/clock"
best=None
for _ in range(25):
    try:
        r=urllib.request.Request(URL,headers=HDRS)
        t0=time.time(); s=urllib.request.urlopen(r,timeout=5,context=ctx).read().decode().strip(); t1=time.time()
    except Exception: continue
    srv=datetime.fromisoformat(s.replace("Z","+00:00")).timestamp()
    L=(t0+t1)/2.0-srv; rtt=t1-t0
    if best is None or rtt<best[0]: best=(rtt,L)
print("0 0" if best is None else f"{best[1]:.4f} {best[0]:.4f}")
'''

def _q(s):
    return "'" + s.replace("'", "'\\''") + "'"

def sh_local(args, **kw):
    return subprocess.run(args, **kw)

def deploy_helpers():
    with open("/tmp/mbpost.swift", "w") as f: f.write(POSTER)
    with open("/tmp/clock_offset.py", "w") as f: f.write(OFFSET)
    subprocess.run(["scp", "-q", "/tmp/mbpost.swift", f"{REMOTE}:/tmp/mbpost.swift"])
    subprocess.run(["scp", "-q", "/tmp/clock_offset.py", f"{REMOTE}:/tmp/clock_offset.py"])

def offset(host):
    if host == "neo":
        out = subprocess.run(["python3", "/tmp/clock_offset.py"], capture_output=True, text=True).stdout
    else:
        out = subprocess.run(["ssh", host, "python3 /tmp/clock_offset.py"], capture_output=True, text=True).stdout
    return float(out.split()[0])

def volume(v):
    subprocess.run(["osascript", "-e", f"set volume output volume {v} without output muted"])
    subprocess.run(["ssh", REMOTE, f'osascript -e "set volume output volume {v} without output muted"'])

L = {}

def post(host, name, kv):
    kvstr = ";".join(f"{k}={val}" for k, val in kv.items())
    if host == "neo":
        env = dict(os.environ, MB_NAME=name, MB_KV=kvstr)
        subprocess.run(["/usr/bin/swift", "/tmp/mbpost.swift"], env=env)
    else:
        script = (f"export MB_NAME={_q(name)}; export MB_KV={_q(kvstr)}; "
                  "/usr/bin/swift /tmp/mbpost.swift\n")
        subprocess.run(["ssh", host, "bash -s"], input=script, text=True)

def db(lead):                       # shared downbeat (server-UTC) `lead` s out
    return time.time() - L["neo"] + lead

def se(host, T):                    # server-UTC instant -> host's local clock
    return T + L[host]

def speak(host, text, voice, at, pitch=None):
    kv = {"text": text, "voice": voice, "volume": "1", "startEpoch": f"{at:.3f}"}
    if pitch: kv["pitch"] = str(pitch)
    post(host, SAY, kv)

def est(text):
    """Rough spoken duration (s) at the default voice rate."""
    return max(0.9, len(text.split()) * 0.34 + 0.45)

def line(host, text, voice, lead=0.3, pitch=None, tail=0.35):
    """Speak a line and wait just long enough for the next speaker to answer
    snappily — ~`tail` seconds after this line finishes."""
    T = db(lead); speak(host, text, voice, se(host, T), pitch)
    time.sleep(lead + est(text) + tail)

def play(host, program, bpm, tracks, at, vel=100):
    kv = {"program": str(program), "bpm": str(bpm), "velocity": str(vel),
          "startEpoch": f"{at:.3f}"}
    for i, (notes, v) in enumerate(tracks):
        sfx = ["", "2", "3", "4"][i]
        kv["notes" + sfx] = notes
        kv["velocity" + sfx] = str(v)
    post(host, PLAY, kv)

# ---- note generators -------------------------------------------------------
def arp(prog, pattern, dur, octave=0):
    out = []
    for ch in prog:
        for i in pattern:
            out.append(f"{ch[i % len(ch)] + octave}:{dur}")
    return ",".join(out)

def voice_line(prog, v, dur):
    return ",".join(f"{ch[v % len(ch)]}:{dur}" for ch in prog)

def steady_bass(prog, dur, per_bar):
    out = []
    for ch in prog:
        out += [f"{ch[0]}:{dur}"] * per_bar
    return ",".join(out)

# ---- the four movements ----------------------------------------------------
C, Am, F, G = [48,60,64,67], [45,60,64,69], [41,60,65,69], [43,59,62,67]
PRELUDE = [C, Am, F, G, C, F, G, C]

def prelude(at):
    play("neo", 6, 84, [(arp(PRELUDE, [1,2,3,2]*4, 0.25), 92)], at)
    play(REMOTE, 32, 84, [(steady_bass(PRELUDE, 1, 4), 95)], at)
PRELUDE_DUR = 8 * 4 * 60 / 84

am, dm, e, fm = [57,60,64], [57,62,65], [56,59,64], [57,60,65]
SARA = [am, dm, am, e, am, fm, e, am]
SARA_MEL = "72:1,76:2,74:1,77:2,76:1,72:2,71:1,75:2,72:1,76:2,77:1,81:2,75:1,71:2,69:3"

def sarabande(at):
    play("neo", 68, 60, [(SARA_MEL, 98)], at)            # oboe melody
    play(REMOTE, 48, 60, [(voice_line(SARA, 0, 3), 64),  # strings, 3 voices
                          (voice_line(SARA, 1, 3), 64),
                          (voice_line(SARA, 2, 3), 64)], at)
SARA_DUR = 8 * 3 * 60 / 60

g, d, c, d7 = [55,59,62], [57,62,66], [55,60,64], [57,60,66]
MEN = [g, d, g, c, g, d, d7, g]
MEN_MEL = ("67:1,71:1,74:1,74:1,73:1,74:1,71:1,67:1,71:1,72:2,76:1,"
           "74:1,71:1,67:1,69:1,71:1,73:1,74:2,72:1,71:3")

def menuet(at):
    play("neo", 73, 120, [(MEN_MEL, 100)], at)           # flute melody
    play(REMOTE, 6, 120, [                                # harpsichord oom-pah
        (",".join(f"{ch[0]-12}:1,r:2" for ch in MEN), 92),
        (",".join(f"r:1,{ch[1]}:1,{ch[1]}:1" for ch in MEN), 70),
        (",".join(f"r:1,{ch[2]}:1,{ch[2]}:1" for ch in MEN), 70)], at)
MEN_DUR = 8 * 3 * 60 / 120

dM, aM, gM, a7 = [50,54,57,62], [49,57,61,64], [50,55,59,62], [48,57,61,64]
GIGUE = [dM, aM, dM, gM, dM, aM, a7, dM]

def gigue(at):
    play("neo", 6, 132, [(arp(GIGUE, [1,2,3,2,3,2], 0.5), 102)], at)   # harpsichord runs
    play(REMOTE, 6, 132, [
        (",".join(f"{ch[0]}:0.5,r:0.5,r:0.5,{ch[0]}:0.5,r:0.5,r:0.5" for ch in GIGUE), 108),
        (",".join("k:0.5,r:0.5,r:0.5,s:0.5,r:0.5,r:0.5" for _ in GIGUE), 116),
        (",".join("r:0.5,h:0.5,h:0.5,r:0.5,h:0.5,h:0.5" for _ in GIGUE), 52)], at)
GIGUE_DUR = 8 * 3 * 60 / 132

# ---- conduct ---------------------------------------------------------------
def announce(title):
    T = db(0.4); speak("neo", title, NEO_VOICE, se("neo", T)); time.sleep(0.4 + est(title) + 0.5)

def main():
    deploy_helpers()
    L["neo"] = offset("neo")
    L[REMOTE] = offset(REMOTE)
    print(f"offsets neo={L['neo']:.3f} {REMOTE}={L[REMOTE]:.3f} "
          f"(skew {(L[REMOTE]-L['neo'])*1000:+.0f}ms)", flush=True)
    volume(78)

    # --- skit ---
    line("neo", "Hi blueberry. Would you like to play menuband?", NEO_VOICE)
    line(REMOTE, "Sure! What should we play?", BB_VOICE)
    line("neo", "How about a little suite. In four movements.", NEO_VOICE)
    line(REMOTE, "Ooh, classical. I am in.", BB_VOICE)
    line("neo", "I will take the melodies. You hold it down.", NEO_VOICE)
    line(REMOTE, "Deal. Count us in!", BB_VOICE)

    # --- count-in (neo, at the Prelude tempo) then Movement I ---
    beat = 60 / 84
    Tc = db(2.0)
    for i, w in enumerate(["One.", "Two.", "Three.", "Four."]):
        speak("neo", w, NEO_VOICE, se("neo", Tc + i * beat))
    down = Tc + 4 * beat
    prelude(down)
    time.sleep((down - db(0)) + PRELUDE_DUR + 1.4)

    announce("A Sarabande."); T = db(2.4); sarabande(T); time.sleep(2.4 + SARA_DUR + 1.4)
    announce("A Menuet.");    T = db(2.4); menuet(T);    time.sleep(2.4 + MEN_DUR + 1.4)
    announce("And a Gigue!"); T = db(2.4); gigue(T);     time.sleep(2.4 + GIGUE_DUR + 1.2)

    # --- bows ---
    line(REMOTE, "That was lovely.", BB_VOICE)
    line("neo", "Bravo, blueberry.", NEO_VOICE)
    print("suite complete", flush=True)

if __name__ == "__main__":
    main()
