// numrank, 2026.09.07
// 60-second mental math sprint — type the answer and it advances itself.
// Wrong digits never print: they buzz, shake, and break the streak.
// Clean solves build a streak; runs rank globally via /api/nom-scores
// (game: "numrank") and earn a rank title. `numrank:120` runs long (unranked).

const { floor, min, max } = Math;

const DURATION = 60; // ranked run length in seconds
const KEY_ROWS = [[7, 8, 9], [4, 5, 6], [1, 2, 3], ["", 0, ""]];

// The ladder IS the difficulty curve: the rank you hold mid-run decides the
// problem ranges — you go through the ranks to earn the complexity.
// kinds: 2 = add/sub only, 3 = +multiply, 4 = +divide. Strongest first.
const RANKS = [
  { at: 50, title: "oracle", color: [255, 215, 60], add: [2, 100], mulA: [2, 12], mulB: [2, 100], kinds: 4 },
  { at: 40, title: "wizard", color: [200, 120, 255], add: [2, 100], mulA: [2, 12], mulB: [2, 100], kinds: 4 },
  { at: 31, title: "mathlete", color: [90, 220, 255], add: [2, 100], mulA: [2, 12], mulB: [2, 100], kinds: 4 },
  { at: 23, title: "computer", color: [80, 255, 160], add: [2, 100], mulA: [2, 12], mulB: [2, 100], kinds: 4 },
  { at: 16, title: "calculator", color: [120, 200, 255], add: [2, 100], mulA: [2, 9], mulB: [2, 25], kinds: 4 },
  { at: 11, title: "reckoner", color: [255, 170, 80], add: [2, 50], mulA: [2, 6], mulB: [2, 12], kinds: 4 },
  { at: 6, title: "adder", color: [255, 130, 130], add: [2, 20], mulA: [2, 5], mulB: [2, 9], kinds: 3 },
  { at: 0, title: "counter", color: [180, 180, 200], add: [2, 9], mulA: [2, 5], mulB: [2, 5], kinds: 2 },
];

const TITLE_COLORS = [
  [255, 105, 97], [255, 170, 80], [255, 215, 60], [120, 220, 100],
  [90, 220, 255], [120, 160, 255], [200, 120, 255], [255, 130, 180],
];

let state; // "title" | "count" | "play" | "over"
let duration, ranked;
let problem, entry, dirty;
let solved, streak, bestStreak;
let countStart, startMs, remaining, lastTickSecond;
let flash, shake, keyFlash, overTimer, frames;
let ghost, rankUp; // fading solve notation; mid-run rank-up banner
let best, improved;
let playerHandle, authorizeFn, storeRef, numRef;
let leaderboard, leaderboardStatus, leaderboardRank, lastSubmittedKey;
let generation = 0;

function rankFor(score) {
  return RANKS.find((r) => score >= r.at);
}

// One line of working — the mental route (round-and-correct, left-to-right,
// distributive splits). Shown when you slip; echoed after every solve.
function makeHint(kind, a, b) {
  if (kind === 0) {
    const ones = b % 10, tens = b - ones;
    if (ones >= 8) return `${a}+${tens + 10}=${a + tens + 10}  -${10 - ones}  ${a + b}`;
    if (tens === 0 || ones === 0) return `${a}+${b}=${a + b}`;
    return `${a}+${tens}=${a + tens}  +${ones}  ${a + b}`;
  }
  if (kind === 1) {
    const c = a + b, ones = a % 10, tens = a - ones;
    if (ones >= 8) return `${c}-${tens + 10}=${c - tens - 10}  +${10 - ones}  ${b}`;
    if (tens === 0 || ones === 0) return `${c}-${a}=${b}`;
    return `${c}-${tens}=${c - tens}  -${ones}  ${b}`;
  }
  if (kind === 2) {
    const ones = b % 10, tens = b - ones;
    if (tens === 0 || ones === 0) return `${a}x${b}=${a * b}`;
    return `${a}x${tens}=${a * tens}  +${a}x${ones}=${a * ones}  ${a * b}`;
  }
  const c = a * b, ones = b % 10, tens = b - ones;
  if (tens === 0 || ones === 0) return `${c}/${a}=${b}`;
  return `${c}=${a}x${tens}+${a}x${ones}  ${tens}+${ones}=${b}`;
}

function makeProblem() {
  const rank = rankFor(solved);
  const kind = numRef.randInt(rank.kinds - 1);
  let a, b;
  if (kind <= 1) {
    a = numRef.randIntRange(...rank.add);
    b = numRef.randIntRange(...rank.add);
  } else {
    a = numRef.randIntRange(...rank.mulA);
    b = numRef.randIntRange(...rank.mulB);
  }
  const hint = makeHint(kind, a, b);
  if (kind === 0) return { text: `${a} + ${b}`, answer: String(a + b), hint };
  if (kind === 1) return { text: `${a + b} - ${a}`, answer: String(b), hint };
  if (kind === 2) return { text: `${a} x ${b}`, answer: String(a * b), hint };
  return { text: `${a * b} / ${a}`, answer: String(b), hint };
}

function resetRun() {
  solved = 0; // before makeProblem — the opening rank picks the ranges
  streak = 0;
  bestStreak = 0;
  problem = makeProblem();
  entry = "";
  dirty = false;
  remaining = duration;
  lastTickSecond = null;
  flash = 0;
  shake = 0;
  ghost = null;
  rankUp = null;
  improved = false;
  leaderboardRank = null;
  leaderboardStatus = "";
  lastSubmittedKey = "";
}

async function boot({ params, colon, store, handle, authorize, num, hud }) {
  hud?.label?.("");
  numRef = num;
  storeRef = store;
  authorizeFn = authorize || null;
  const rawHandle = handle?.();
  playerHandle = rawHandle
    ? (String(rawHandle).startsWith("@") ? String(rawHandle) : `@${rawHandle}`)
    : null;

  const p = parseInt(colon?.[0] ?? params?.[0], 10); // numrank:120 → colon
  duration = Number.isFinite(p) && p >= 10 && p <= 600 ? p : DURATION;
  ranked = duration === DURATION;

  state = "title";
  frames = 0;
  keyFlash = null;
  leaderboard = [];
  generation += 1;
  resetRun();

  best = (await store.retrieve("numrank:best")) || 0;
  loadLeaderboard(generation);
}

async function loadLeaderboard(gen) {
  if (typeof fetch !== "function") return;
  try {
    const response = await fetch("/api/nom-scores?game=numrank");
    if (!response.ok) throw new Error(`scores ${response.status}`);
    const data = await response.json();
    if (gen === generation) leaderboard = Array.isArray(data.scores) ? data.scores : [];
  } catch {
    if (gen === generation) leaderboardStatus = "scores offline";
  }
}

async function submitRun() {
  if (!ranked) {
    leaderboardStatus = `${duration}s runs are unranked`;
    return;
  }
  if (!playerHandle || !authorizeFn || typeof fetch !== "function") {
    if (!playerHandle) leaderboardStatus = "set a handle to rank";
    return;
  }
  const key = `${solved}:${bestStreak}`;
  if (key === lastSubmittedKey) return;
  lastSubmittedKey = key;
  const gen = generation;
  try {
    const token = await authorizeFn();
    if (!token) throw new Error("not authorized");
    const response = await fetch("/api/nom-scores", {
      method: "POST",
      headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
      body: JSON.stringify({
        game: "numrank",
        score: solved,
        level: bestStreak + 1, // tiebreak: cleaner runs outrank
        correct: solved,
      }),
    });
    if (!response.ok) throw new Error(`scores ${response.status}`);
    const data = await response.json();
    if (gen !== generation) return;
    leaderboardRank = data.rank || null;
    leaderboardStatus = data.improved ? `new world best · #${data.rank}` : `world best · #${data.rank}`;
    await loadLeaderboard(gen);
  } catch {
    if (gen === generation) leaderboardStatus = "score not saved";
  }
}

// Shared by paint (drawing) and act (hit-testing) so the pads always line up.
function keypadRects(screen) {
  const sw = screen.width, sh = screen.height;
  const gap = 3;
  const cell = min(floor((sw - gap * 2 - 8) / 3), floor((sh * 0.48 - gap * 3) / 4), 44);
  const w = cell * 3 + gap * 2;
  const x0 = floor((sw - w) / 2);
  const y0 = sh - (cell * 4 + gap * 3) - 6;
  const rects = [];
  KEY_ROWS.forEach((row, r) => {
    row.forEach((value, c) => {
      rects.push({
        value,
        x: x0 + c * (cell + gap),
        y: y0 + r * (cell + gap),
        w: cell,
        h: cell,
      });
    });
  });
  return rects;
}

function startCountdown(sound) {
  resetRun();
  state = "count";
  countStart = performance.now();
  sound.synth({ type: "sine", tone: 440, attack: 0, decay: 0.1, duration: 0.1, volume: 0.3 });
}

function pressDigit(d, sound) {
  if (state !== "play") return;
  const next = entry + d;
  if (!problem.answer.startsWith(next)) {
    // Wrong digit: never printed — buzz, shake, and the streak restarts.
    if (!dirty) {
      dirty = true;
      streak = 0;
    }
    shake = 8;
    sound.synth({ type: "sawtooth", tone: 180, attack: 0, decay: 0.12, duration: 0.12, volume: 0.35 });
    return;
  }
  entry = next;
  keyFlash = { value: d, frames: 6 };
  if (entry === problem.answer) {
    const rankBefore = rankFor(solved);
    solved += 1;
    const rankAfter = rankFor(solved);
    streak = dirty ? 1 : streak + 1;
    bestStreak = max(bestStreak, streak);
    flash = 8;
    ghost = { text: problem.hint, frames: 100 };
    const tone = 660 * Math.pow(2, min(streak, 12) / 12);
    sound.synth({ type: "triangle", tone, attack: 0, decay: 0.09, duration: 0.09, volume: 0.3 });
    if (streak > 0 && streak % 5 === 0) {
      sound.synth({ type: "triangle", tone: tone * 2, attack: 0.05, decay: 0.12, duration: 0.12, volume: 0.2 });
    }
    if (rankAfter !== rankBefore) {
      rankUp = { title: rankAfter.title, color: rankAfter.color, frames: 120 };
      sound.synth({ type: "triangle", tone: 880, attack: 0, decay: 0.15, duration: 0.15, volume: 0.3 });
      sound.synth({ type: "triangle", tone: 1320, attack: 0.08, decay: 0.2, duration: 0.2, volume: 0.25 });
    }
    problem = makeProblem();
    entry = "";
    dirty = false;
  } else {
    sound.synth({ type: "sine", tone: 620, attack: 0, decay: 0.04, duration: 0.04, volume: 0.12 });
  }
}

function finishRun(sound) {
  state = "over";
  overTimer = 0;
  sound.synth({ type: "sine", tone: 523, attack: 0, decay: 0.4, duration: 0.4, volume: 0.35 });
  sound.synth({ type: "sine", tone: 392, attack: 0.12, decay: 0.5, duration: 0.5, volume: 0.3 });
  if (solved > best) {
    best = solved;
    improved = true;
    storeRef["numrank:best"] = best;
    storeRef.persist("numrank:best");
  }
  submitRun();
}

function sim({ sound }) {
  frames += 1;
  flash = max(0, flash - 1);
  shake = max(0, shake - 1);
  if (keyFlash && --keyFlash.frames <= 0) keyFlash = null;
  if (ghost && --ghost.frames <= 0) ghost = null;
  if (rankUp && --rankUp.frames <= 0) rankUp = null;
  if (state === "over") overTimer += 1;

  if (state === "count") {
    const elapsed = (performance.now() - countStart) / 1000;
    const step = floor(elapsed); // 0,1,2 → "3","2","1"
    if (step !== lastTickSecond) {
      lastTickSecond = step;
      if (step > 0 && step < 3) {
        sound.synth({ type: "sine", tone: 440, attack: 0, decay: 0.1, duration: 0.1, volume: 0.3 });
      }
    }
    if (elapsed >= 3) {
      state = "play";
      startMs = performance.now();
      lastTickSecond = null;
      sound.synth({ type: "sine", tone: 880, attack: 0, decay: 0.15, duration: 0.15, volume: 0.35 });
    }
    return;
  }

  if (state === "play") {
    remaining = duration - (performance.now() - startMs) / 1000;
    const second = floor(remaining);
    if (remaining <= 5 && second !== lastTickSecond && remaining > 0) {
      lastTickSecond = second;
      sound.synth({ type: "sine", tone: 330, attack: 0, decay: 0.06, duration: 0.06, volume: 0.2 });
    }
    if (remaining <= 0) {
      remaining = 0;
      finishRun(sound);
    }
  }
}

function drawTitleWord(ink, screen, y, size) {
  const word = "numrank";
  const cw = 6 * size;
  let x = floor((screen.width - word.length * cw) / 2);
  for (let i = 0; i < word.length; i += 1) {
    const wave = floor(Math.sin(frames / 20 + i * 0.8) * 2);
    ink(...TITLE_COLORS[i % TITLE_COLORS.length]).write(word[i], { x, y: y + wave, size });
    x += cw;
  }
}

function drawBoard(ink, screen, y, rows) {
  const medals = [[255, 215, 60], [200, 200, 210], [205, 140, 80]];
  rows.slice(0, 5).forEach((row, i) => {
    const you = playerHandle && row.handle === playerHandle;
    const color = you ? [120, 255, 160] : medals[i] || [150, 150, 180];
    ink(...color, you ? 255 : 190).write(
      `${row.rank}. ${row.handle} ${row.score}`,
      { x: screen.width / 2, y: y + i * 10, center: "x" },
    );
  });
}

function paint({ wipe, ink, screen }) {
  const sw = screen.width, sh = screen.height;
  const cx = floor(sw / 2);

  if (flash > 0) wipe(12, 26 + flash * 3, 20);
  else if (shake > 0) wipe(30, 12, 18);
  else wipe(10, 10, 22);

  if (state === "title") {
    drawTitleWord(ink, screen, floor(sh * 0.14), sw >= 200 ? 3 : 2);
    ink(200, 200, 230).write(`${duration} seconds of mental math`, { x: cx, y: floor(sh * 0.3), center: "x" });
    ink(140, 140, 180).write("type the answer - it advances itself", { x: cx, y: floor(sh * 0.3) + 12, center: "x" });
    if (best > 0) {
      const r = rankFor(best);
      ink(...r.color).write(`your best ${best} - ${r.title}`, { x: cx, y: floor(sh * 0.3) + 28, center: "x" });
    }
    if (leaderboard.length) {
      ink(120, 120, 170).write("world ranks", { x: cx, y: floor(sh * 0.48), center: "x" });
      drawBoard(ink, screen, floor(sh * 0.48) + 12, leaderboard);
    } else if (leaderboardStatus) {
      ink(120, 120, 170).write(leaderboardStatus, { x: cx, y: floor(sh * 0.5), center: "x" });
    }
    const pulse = 150 + floor(Math.sin(frames / 12) * 60);
    ink(255, 255, 255, pulse).write("tap or press any key", { x: cx, y: sh - 18, center: "x" });
    return;
  }

  if (state === "count") {
    const elapsed = (performance.now() - countStart) / 1000;
    const n = max(1, 3 - floor(elapsed));
    ink(255, 215, 60).write(String(n), { center: "xy", size: 6 });
    ink(150, 150, 190).write("get ready", { x: cx, y: floor(sh / 2) + 34, center: "x" });
    return;
  }

  const pads = keypadRects(screen);
  const padTop = pads[0].y;

  if (state === "play" || state === "over") {
    // Time bar across the top, green → red as it runs down.
    const frac = remaining / duration;
    const barW = floor(sw * frac);
    const barColor = frac > 0.5 ? [80, 220, 130] : frac > 0.15 ? [255, 200, 60] : [255, 80, 70];
    ink(...barColor).box(0, 0, barW, 3);
    ink(255, 255, 255, 220).write(String(solved), { x: 5, y: 7 });
    if (state === "play") {
      const liveRank = rankFor(solved);
      ink(...liveRank.color, 210).write(liveRank.title, { x: 5, y: 19 });
    }
    if (streak >= 2) ink(255, 215, 60).write(`streak ${streak}`, { x: sw - 5 - `streak ${streak}`.length * 6, y: 7 });
  }

  if (state === "play") {
    const size = sw >= 220 ? 3 : 2;
    const jitter = shake > 0 ? (frames % 2 === 0 ? -shake / 3 : shake / 3) : 0;
    const zoneH = padTop - 14;
    ink(255, 255, 255).write(problem.text, { x: cx + floor(jitter), y: 14 + floor(zoneH * 0.3), center: "x", size });

    const entryY = 14 + floor(zoneH * 0.62);
    const entryColor = shake > 0 ? [255, 90, 70] : [120, 220, 255];
    const caret = frames % 60 < 36 ? "_" : " ";
    ink(...entryColor).write(entry + caret, { x: cx, y: entryY, center: "x", size });

    // The working: amber while you're off the route, green echo after a solve.
    const hintY = min(entryY + 10 * size + 8, padTop - 14);
    if (dirty) {
      ink(255, 200, 120, 220).write(problem.hint, { x: cx, y: hintY, center: "x" });
    } else if (ghost) {
      ink(140, 255, 180, min(220, ghost.frames * 4)).write(ghost.text, { x: cx, y: hintY, center: "x" });
    }

    if (rankUp) {
      ink(...rankUp.color, min(255, rankUp.frames * 5)).write(`${rankUp.title}!`, { x: cx, y: 30, center: "x", size: 2 });
    }
  }

  if (state === "play") {
    for (const pad of pads) {
      if (pad.value === "") continue; // blank slot beside the zero
      const hot = keyFlash && keyFlash.value === String(pad.value);
      ink(hot ? 90 : 36, hot ? 90 : 36, hot ? 140 : 60).box(pad.x, pad.y, pad.w, pad.h);
      ink(70, 70, 110, 160).box(pad.x, pad.y, pad.w, 1);
      ink(hot ? 255 : 200, hot ? 255 : 200, hot ? 255 : 230).write(String(pad.value), {
        x: pad.x + floor(pad.w / 2),
        y: pad.y + floor(pad.h / 2) - 4,
        center: "x",
      });
    }
  }

  if (state === "over") {
    ink(0, 0, 0, 120).box(0, 0, sw, sh);
    const finalRank = rankFor(solved);
    ink(255, 255, 255).write(`${solved} solved`, { x: cx, y: floor(sh * 0.16), center: "x", size: 2 });
    ink(...finalRank.color).write(finalRank.title, { x: cx, y: floor(sh * 0.16) + 22, center: "x", size: 2 });
    let y = floor(sh * 0.16) + 48;
    if (improved) {
      const pulse = 180 + floor(Math.sin(frames / 8) * 70);
      ink(120, 255, 160, pulse).write("new personal best!", { x: cx, y, center: "x" });
      y += 12;
    } else if (best > 0) {
      ink(150, 150, 190).write(`personal best ${best}`, { x: cx, y, center: "x" });
      y += 12;
    }
    if (bestStreak >= 2) {
      ink(255, 215, 60, 200).write(`longest streak ${bestStreak}`, { x: cx, y, center: "x" });
      y += 12;
    }
    if (leaderboardStatus) {
      ink(120, 200, 255).write(leaderboardStatus, { x: cx, y: y + 4, center: "x" });
      y += 16;
    }
    if (leaderboard.length) {
      drawBoard(ink, screen, y + 8, leaderboard);
    }
    if (overTimer > 45) {
      const pulse = 150 + floor(Math.sin(frames / 12) * 60);
      ink(255, 255, 255, pulse).write("tap to go again", { x: cx, y: sh - 14, center: "x" });
    }
  }
}

function act({ event: e, screen, sound, needsPaint }) {
  if (state === "title" && (e.is("touch") || (e.is("keyboard:down") && !e.repeat))) {
    startCountdown(sound);
    needsPaint();
    return;
  }

  if (state === "over" && overTimer > 45 && (e.is("touch") || e.is("keyboard:down:space") || e.is("keyboard:down:enter"))) {
    startCountdown(sound);
    needsPaint();
    return;
  }

  if (state !== "play") return;

  for (let d = 0; d <= 9; d += 1) {
    if (e.is(`keyboard:down:${d}`)) {
      pressDigit(String(d), sound);
      needsPaint();
      return;
    }
  }

  if (e.is("touch")) {
    for (const pad of keypadRects(screen)) {
      if (pad.value === "") continue;
      if (e.x >= pad.x && e.x < pad.x + pad.w && e.y >= pad.y && e.y < pad.y + pad.h) {
        pressDigit(String(pad.value), sound);
        needsPaint();
        return;
      }
    }
  }
}

function meta() {
  return {
    title: "numrank",
    desc: "60-second mental math sprint — type answers, build streaks, climb the world ranks.",
  };
}

export { boot, sim, paint, act, meta };
export const nohud = true;
