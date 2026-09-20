# Sim/paint phase profiler for the console. `python3 xbox/tools/oskiewar-phase-profile.py apply`
# instruments xbox/live/oskiewar.js (run from the repo root), `strip` restores it. Never commit
# the applied state: the marks break source-shape tests. Push the applied file to the Xbox with
# `node xbox/tools/oskiewar-release.mjs deploy-xbox-dev`, then read `node xbox/tools/live.mjs logs 300 |
# grep FRAME_PHASES` — one line per 120 frames, phases in ms/frame, largest first. Measured
# 2026-09-20 on the Series X: terrain 8.4 → 7.2 ms, two title bots 9.5 ms of sim, renderables 3 ms.
# apply|strip the FRAME_PROFILE / FRAME_PHASES diagnostics on xbox/live/oskiewar.js
import re,sys
p="xbox/live/oskiewar.js"; s=open(p).read(); mode=sys.argv[1]
BLOCK_START="// DIAGNOSTIC (uncommitted): sim/paint split for the console's hostJsMs."
CLEAN_SIM='''function sim() {
  if (clientError) { restartAfterClientError(); return; }
  try {
    netDrainHostInbox();
    if (netSession) netTick();
    else gameSim();
  } catch (error) {
    captureClientError("sim", error);
  }
}

function paint() {
  if (clientError) {
    try { drawClientError(); }
    catch (_) { drawClientErrorFallback(); }
    return;
  }
  const restore = beginRenderInterpolation(runtime().renderAlpha ?? 1);
  sharingRenderPoses = true;
  try {
    gamePaint();
  } catch (error) {
    captureClientError("paint", error);
    try { drawClientError(); }
    catch (_) { drawClientErrorFallback(); }
  } finally {
    sharingRenderPoses = false;
    renderPoses.clear();
    restore();
  }
}'''
INSTR='''// DIAGNOSTIC (uncommitted): sim/paint split for the console's hostJsMs.
const profileClock = () => (typeof performance === "object" && performance &&
  typeof performance.now === "function") ? performance.now() : Date.now();
let profSim = 0, profPaint = 0, profSimMax = 0, profPaintMax = 0, profFrames = 0;
const profPhases = new Map(); let profLast = 0;
function profMark(label) {
  const t = profileClock();
  profPhases.set(label, (profPhases.get(label) || 0) + (t - profLast));
  profLast = t;
}
function profileFlush() {
  if (++profFrames < 120) return;
  telemetry("FRAME_PHASES", [...profPhases.entries()].sort((a, b) => b[1] - a[1])
    .map(([k, v]) => k + "=" + (v / profFrames).toFixed(2)).join(" "));
  profPhases.clear();
  telemetry("FRAME_PROFILE", "sim=" + (profSim / profFrames).toFixed(2) +
    "ms max=" + profSimMax.toFixed(2) + " paint=" + (profPaint / profFrames).toFixed(2) +
    "ms max=" + profPaintMax.toFixed(2) + " mode=" + gameMode + " shell=" + shellMode);
  profSim = profPaint = profSimMax = profPaintMax = profFrames = 0;
}

function sim() {
  if (clientError) { restartAfterClientError(); return; }
  const started = profileClock(); profLast = started;
  try {
    netDrainHostInbox();
    if (netSession) netTick();
    else gameSim();
  } catch (error) {
    captureClientError("sim", error);
  }
  const ms = profileClock() - started;
  profSim += ms; if (ms > profSimMax) profSimMax = ms;
}

function paint() {
  if (clientError) {
    try { drawClientError(); }
    catch (_) { drawClientErrorFallback(); }
    return;
  }
  const started = profileClock(); profLast = started;
  const restore = beginRenderInterpolation(runtime().renderAlpha ?? 1);
  sharingRenderPoses = true;
  try {
    gamePaint();
  } catch (error) {
    captureClientError("paint", error);
    try { drawClientError(); }
    catch (_) { drawClientErrorFallback(); }
  } finally {
    sharingRenderPoses = false;
    renderPoses.clear();
    restore();
  }
  const ms = profileClock() - started;
  profPaint += ms; if (ms > profPaintMax) profPaintMax = ms;
  profileFlush();
}'''
PAINT_MARKS=[(29,"p:view"),(91,"p:prep"),(110,"p:wipe+sky"),(122,"p:room"),(125,"p:terrain"),(130,"p:skatepark"),(284,"p:mid"),(300,"p:renderables"),(310,"p:debug"),(312,"p:meter+impacts"),(412,"p:title+hud")]
SIM_MARKS=[(10,"s:view"),(97,"s:door+pads"),(101,"s:shell"),(108,"s:seat+lobby"),(143,"s:players"),(328,"s:physics"),(373,"s:impacts")]
def strip(s):
    s="\n".join(l for l in s.split("\n") if not re.match(r'^\s*profMark\("',l))
    i=s.find(BLOCK_START)
    if i>=0:
        j=s.find("\nfunction act() {}", i)
        s=s[:i]+CLEAN_SIM+s[j:]
    return s
def apply(s):
    s=strip(s)
    assert s.count(CLEAN_SIM)==1; s=s.replace(CLEAN_SIM,INSTR)
    lines=s.split("\n")
    def fstart(name):
        return next(i for i,l in enumerate(lines) if l.startswith("function "+name+"("))
    def fend(i):
        j=i+1
        while lines[j]!="}": j+=1
        return j
    P=fstart("gamePaint"); PE=fend(P); S=fstart("gameSim"); SE=fend(S)
    marks=[(P+o-1,l) for o,l in PAINT_MARKS]+[(PE,"p:tail")]+[(S+o-1,l) for o,l in SIM_MARKS]+[(SE,"s:tail")]
    for idx,lab in sorted(marks,reverse=True): lines.insert(idx,'  profMark("%s");'%lab)
    return "\n".join(lines)
out = strip(s) if mode=="strip" else apply(s)
open(p,"w").write(out); print(mode, "ok")
