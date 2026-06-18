#!/usr/bin/env python3
"""Two-machine menuband performance: a spoken skit, a count-in, then a little
four-movement Baroque suite played across neo + blueberry, each part on its own
GM voice (+ percussion), locked to a shared clock.

Timing model: the two machines sit on the same LAN (here, a phone hotspot), so
we sync them DIRECTLY to each other (peer-to-peer min-RTT offset) rather than
each to a distant server — a much shorter, lower-jitter path. neo's local clock
is the reference; blueberry is corrected by the measured (bb - neo) offset.
SSH is multiplexed (ControlMaster) so posts don't pay connection setup each
time. A timing report prints after every run.

  --metronome   layer an alternating click (neo even beats / blueberry odd)
                under every movement, to hear the lock.

  neo = local host (Daniel)   blueberry = ssh host (Samantha)
"""
import os, statistics, subprocess, sys, time

REMOTE = "blueberry"
NEO_VOICE, BB_VOICE = "Daniel", "Samantha"
PLAY = "computer.aestheticcomputer.menuband.play"
SAY = "computer.aestheticcomputer.menuband.say"
CTRL = "/tmp/ac-bb-ctrl.sock"
SSH = ["ssh", "-o", "ControlMaster=auto", "-o", f"ControlPath={CTRL}",
       "-o", "ControlPersist=180", "-o", "ConnectTimeout=8"]

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
RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.06))
'''

# ---- ssh / posting -------------------------------------------------------
def ssh_run(cmd, **kw):
    return subprocess.run(SSH + [REMOTE, cmd], **kw)

def deploy_helpers():
    # Precompile the poster to a native binary on each machine — `swift file`
    # recompiles every call (~470ms); a built binary posts in ~20ms.
    with open("/tmp/mbpost.swift", "w") as f:
        f.write(POSTER)
    subprocess.run(["swiftc", "-O", "/tmp/mbpost.swift", "-o", "/tmp/mbpost"])
    subprocess.run(["scp", "-o", f"ControlPath={CTRL}", "-o", "ControlMaster=auto",
                    "-q", "/tmp/mbpost.swift", f"{REMOTE}:/tmp/mbpost.swift"])
    ssh_run("swiftc -O /tmp/mbpost.swift -o /tmp/mbpost")

PEER = 0.0                       # blueberry_clock - neo_clock (seconds), measured
post_lat = {"neo": [], REMOTE: []}

def _q(s):
    return "'" + s.replace("'", "'\\''") + "'"

def post(host, name, kv):
    kvstr = ";".join(f"{k}={v}" for k, v in kv.items())
    t0 = time.time()
    if host == "neo":
        env = dict(os.environ, MB_NAME=name, MB_KV=kvstr)
        subprocess.run(["/tmp/mbpost"], env=env)
    else:
        script = (f"export MB_NAME={_q(name)}; export MB_KV={_q(kvstr)}; "
                  "/tmp/mbpost\n")
        ssh_run("bash -s", input=script, text=True)
    post_lat[host].append(time.time() - t0)

# ---- clock sync ----------------------------------------------------------
def peer_offset(n=15):
    """Measure (blueberry_clock - neo_clock) directly over the LAN, Cristian's
    algorithm, keeping the lowest-RTT samples. Returns dict with offset/jitter."""
    samples = []
    for _ in range(n):
        t0 = time.time()
        out = ssh_run("python3 -c 'import time;print(repr(time.time()))'",
                      capture_output=True, text=True).stdout.strip()
        t1 = time.time()
        try:
            bb = float(out)
        except ValueError:
            continue
        rtt = t1 - t0
        samples.append((rtt, bb - (t0 + t1) / 2.0))
    samples.sort()
    fastest = [o for _, o in samples[:5]]
    return {
        "offset": statistics.median(fastest) if fastest else 0.0,
        "jitter": statistics.pstdev(fastest) if len(fastest) > 1 else 0.0,
        "min_rtt": samples[0][0] if samples else 0.0,
        "rtts": [r for r, _ in samples],
        "n": len(samples),
    }

def db(lead):                    # neo-local target instant `lead` s out
    return time.time() + lead

def se(host, T):                 # neo-local instant -> host's local clock
    return T if host == "neo" else T + PEER

def sleep_until(T):              # block until neo-local instant T
    time.sleep(max(0.0, T - time.time()))

# ---- speech / play -------------------------------------------------------
def speak(host, text, voice, at, pitch=None):
    # `at` is a neo-local instant; convert into the speaking machine's clock.
    kv = {"text": text, "voice": voice, "volume": "1", "startEpoch": f"{se(host, at):.3f}"}
    if pitch:
        kv["pitch"] = str(pitch)
    post(host, SAY, kv)

def est(text):
    return max(0.9, len(text.split()) * 0.34 + 0.45)

def line(host, text, voice, lead=0.3, pitch=None, tail=0.35):
    T = db(lead); speak(host, text, voice, T, pitch)   # speak() applies se()
    time.sleep(lead + est(text) + tail)

def play(host, program, bpm, tracks, at, vel=100):
    # `at` is a neo-local instant; convert into the playing machine's clock.
    kv = {"program": str(program), "bpm": str(bpm), "velocity": str(vel),
          "startEpoch": f"{se(host, at):.3f}"}
    for i, (notes, v) in enumerate(tracks):
        sfx = ["", "2", "3", "4"][i]
        kv["notes" + sfx] = notes
        kv["velocity" + sfx] = str(v)
    post(host, PLAY, kv)

# ---- note generators -----------------------------------------------------
def arp(prog, pattern, dur, octave=0):
    return ",".join(f"{ch[i % len(ch)] + octave}:{dur}" for ch in prog for i in pattern)

def voice_line(prog, v, dur):
    return ",".join(f"{ch[v % len(ch)]}:{dur}" for ch in prog)

def steady_bass(prog, dur, per_bar):
    out = []
    for ch in prog:
        out += [f"{ch[0]}:{dur}"] * per_bar
    return ",".join(out)

METRO = ("--metronome" in sys.argv) or (os.environ.get("METRONOME", "") in ("1", "true"))
HOCKET = "--hocket" in sys.argv

def metro(host, beats_per_bar, bars):
    total = beats_per_bar * bars
    own = 0 if host == "neo" else 1
    return ",".join("h:1" if i % 2 == own else "r:1" for i in range(total))

def withmetro(tracks, host, beats_per_bar, bars):
    if METRO and len(tracks) < 4:
        return tracks + [(metro(host, beats_per_bar, bars), 96)]
    return tracks

# ---- the four movements --------------------------------------------------
C, Am, F, G = [48,60,64,67], [45,60,64,69], [41,60,65,69], [43,59,62,67]
PRELUDE = [C, Am, F, G, C, F, G, C]

def prelude(at):
    play("neo", 6, 84, withmetro([(arp(PRELUDE, [1,2,3,2]*4, 0.25), 92)], "neo", 4, 8), at)
    play(REMOTE, 32, 84, withmetro([(steady_bass(PRELUDE, 1, 4), 95)], REMOTE, 4, 8), at)
PRELUDE_DUR = 8 * 4 * 60 / 84

am, dm, e, fm = [57,60,64], [57,62,65], [56,59,64], [57,60,65]
SARA = [am, dm, am, e, am, fm, e, am]
SARA_MEL = "72:1,76:2,74:1,77:2,76:1,72:2,71:1,75:2,72:1,76:2,77:1,81:2,75:1,71:2,69:3"

def sarabande(at):
    play("neo", 68, 60, withmetro([(SARA_MEL, 98)], "neo", 3, 8), at)
    play(REMOTE, 48, 60, withmetro([(voice_line(SARA, 0, 3), 64),
                                    (voice_line(SARA, 1, 3), 64),
                                    (voice_line(SARA, 2, 3), 64)], REMOTE, 3, 8), at)
SARA_DUR = 8 * 3 * 60 / 60

g, d, c, d7 = [55,59,62], [57,62,66], [55,60,64], [57,60,66]
MEN = [g, d, g, c, g, d, d7, g]
MEN_MEL = ("67:1,71:1,74:1,74:1,73:1,74:1,71:1,67:1,71:1,72:2,76:1,"
           "74:1,71:1,67:1,69:1,71:1,73:1,74:2,72:1,71:3")

def menuet(at):
    play("neo", 73, 120, withmetro([(MEN_MEL, 100)], "neo", 3, 8), at)
    play(REMOTE, 6, 120, withmetro([
        (",".join(f"{ch[0]-12}:1,r:2" for ch in MEN), 92),
        (",".join(f"r:1,{ch[1]}:1,{ch[1]}:1" for ch in MEN), 70),
        (",".join(f"r:1,{ch[2]}:1,{ch[2]}:1" for ch in MEN), 70)], REMOTE, 3, 8), at)
MEN_DUR = 8 * 3 * 60 / 120

dM, aM, gM, a7 = [50,54,57,62], [49,57,61,64], [50,55,59,62], [48,57,61,64]
GIGUE = [dM, aM, dM, gM, dM, aM, a7, dM]

def gigue(at):
    play("neo", 6, 132, withmetro([(arp(GIGUE, [1,2,3,2,3,2], 0.5), 102)], "neo", 3, 8), at)
    play(REMOTE, 6, 132, withmetro([
        (",".join(f"{ch[0]}:0.5,r:0.5,r:0.5,{ch[0]}:0.5,r:0.5,r:0.5" for ch in GIGUE), 108),
        (",".join("k:0.5,r:0.5,r:0.5,s:0.5,r:0.5,r:0.5" for _ in GIGUE), 116),
        (",".join("r:0.5,h:0.5,h:0.5,r:0.5,h:0.5,h:0.5" for _ in GIGUE), 52)], REMOTE, 3, 8), at)
GIGUE_DUR = 8 * 3 * 60 / 132

# ---- conduct -------------------------------------------------------------
def volume(v):
    subprocess.run(["osascript", "-e", f"set volume output volume {v} without output muted"])
    ssh_run(f'osascript -e "set volume output volume {v} without output muted"')

def announce(title):
    T = db(0.4); speak("neo", title, NEO_VOICE, T); time.sleep(0.4 + est(title) + 0.5)

def hocket(at, notes, dur, bpm, program=78, vel=108):
    """Bounce a melody note-by-note between the machines, peer-synced."""
    neo_t, bb_t = [], []
    for i, n in enumerate(notes):
        even = (i % 2 == 0)
        neo_t.append(f"{n}:{dur}" if even else f"r:{dur}")
        bb_t.append(f"r:{dur}" if even else f"{n}:{dur}")
    play("neo", program, bpm, [(",".join(neo_t), vel)], at)
    play(REMOTE, program, bpm, [(",".join(bb_t), vel)], at)

def report(peer):
    rtts = peer["rtts"]
    def ms(x): return f"{x*1000:.0f}ms"
    print("\n=== TIMING REPORT ===", flush=True)
    print(f"link            : phone hotspot, peer-to-peer ssh (multiplexed)", flush=True)
    print(f"clock offset    : blueberry is {ms(peer['offset'])} "
          f"{'ahead of' if peer['offset']>=0 else 'behind'} neo", flush=True)
    print(f"offset jitter   : ±{ms(peer['jitter'])}  (reproducibility of the 5 fastest reads)", flush=True)
    print(f"ssh round-trip  : min {ms(min(rtts))}  median {ms(statistics.median(rtts))}  "
          f"max {ms(max(rtts))}  (n={peer['n']})", flush=True)
    print(f"  -> est. residual sync error ~ ±{ms(peer['jitter'] + min(rtts)/2*0.15)}", flush=True)
    for host in ("neo", REMOTE):
        L = post_lat[host]
        if L:
            print(f"post latency {host:>9}: min {ms(min(L))}  median {ms(statistics.median(L))}  "
                  f"max {ms(max(L))}  (n={len(L)})", flush=True)
    # guidance
    j = peer["jitter"] * 1000
    floor = min(rtts) * 1000
    if j < 8 and floor < 60:
        verdict = "tight — lock should be inaudible (<~10ms)."
    elif j < 20:
        verdict = "okay — minor wobble possible; a 5GHz hotspot band or wired/Thunderbolt link would tighten it."
    else:
        verdict = "loose — the hotspot is jittery. Try 5GHz, move phones closer, or an ad-hoc/ethernet link between the macs."
    print(f"verdict         : {verdict}", flush=True)
    print("=====================\n", flush=True)

def main():
    global PEER
    deploy_helpers()
    ssh_run("true")                       # warm the ssh master connection
    peer = peer_offset()
    PEER = peer["offset"]
    print(f"peer offset (bb-neo)={PEER*1000:+.0f}ms jitter=±{peer['jitter']*1000:.0f}ms "
          f"min_rtt={peer['min_rtt']*1000:.0f}ms", flush=True)
    volume(78)

    if HOCKET:
        # --- quick ping-pong note test ---
        line(REMOTE, "Hey neo, let's perform a quick ping pong note test!", BB_VOICE)
        line("neo", "You're on.", NEO_VOICE)
        penta = [60, 62, 64, 67, 69, 72, 74, 76, 79, 81, 84]
        seq = penta + penta[-2::-1]          # up then back down (one octave-ish run)
        seq = seq * 2                         # twice
        dur, bpm = 0.25, 132                  # snappy 16th-note bounce
        T = db(3.0); hocket(T, seq, dur, bpm)
        sleep_until(T + len(seq) * dur * 60 / bpm + 1.4)
        line(REMOTE, "Nailed it.", BB_VOICE)
        report(peer); print("hocket complete", flush=True); return

    # --- skit ---
    line("neo", "Hi blueberry. Would you like to play menuband?", NEO_VOICE)
    line(REMOTE, "Sure! What should we play?", BB_VOICE)
    line("neo", "How about a little suite. In four movements.", NEO_VOICE)
    line(REMOTE, "Ooh, classical. I am in.", BB_VOICE)
    line("neo", "I will take the melodies. You hold it down.", NEO_VOICE)
    line(REMOTE, "Deal. Count us in!", BB_VOICE)

    # --- count-in (generous lead so every cue is fired before its instant) ---
    beat = 60 / 84
    Tc = db(5.0)
    down = Tc + 4 * beat
    prelude(down)                                            # fire music first
    for i, w in enumerate(["One.", "Two.", "Three.", "Four."]):
        speak("neo", w, NEO_VOICE, Tc + i * beat)
    sleep_until(down + PRELUDE_DUR + 1.6)

    announce("A Sarabande."); T = db(3.0); sarabande(T); sleep_until(T + SARA_DUR + 1.6)
    announce("A Menuet.");    T = db(3.0); menuet(T);    sleep_until(T + MEN_DUR + 1.6)
    announce("And a Gigue!"); T = db(3.0); gigue(T);     sleep_until(T + GIGUE_DUR + 1.2)

    # --- bows ---
    line(REMOTE, "That was lovely.", BB_VOICE)
    line("neo", "Bravo, blueberry.", NEO_VOICE)
    report(peer)
    print("suite complete", flush=True)

if __name__ == "__main__":
    main()
