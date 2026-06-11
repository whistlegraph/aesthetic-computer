// besospesos, 2026.06.10
// A ceo dating sim. besos or pesos — you can't have both.
// The cast is the marketing-campaign roster (collectors-altman-facetime,
// jeffrey-helping-elon-igstory, …): the most powerful people in tech can't
// make their own machines work, and you are calmly there to help. Every
// choice trades affection (besos) against invoicing (pesos).
// Simple logic in the nom-game spirit: one state machine, data-driven
// dates, parametric pixel portraits, no assets.

const { floor, round, max, min } = Math;

// 💘 The roster. Each date is three questions; every choice moves the two
// meters. besos >= KISS_AT after three questions wins that ceo's kiss.
const KISS_AT = 5;

const CEOS = [
  {
    key: "sam",
    name: "sam altman",
    tagline: "his facetime keeps freezing",
    face: {
      skin: [232, 190, 160],
      hair: [104, 72, 48],
      style: "wavy",
      shirt: [38, 48, 92], // navy crewneck
      vest: [168, 170, 176], // grey fleece vest
    },
    dates: [
      {
        line: "my facetime to shanghai keeps freezing. the board call is in four minutes. you seem... calm.",
        choices: [
          { label: "four minutes is plenty. let me ping it.", besos: 2, pesos: 0, reply: "he watches you type. 'you're so composed,' he whispers." },
          { label: "have you tried turning it off and on?", besos: 1, pesos: 0, reply: "'i... have not.' he looks down, humbled." },
          { label: "i charge rush rates. $800.", besos: -1, pesos: 800, reply: "he wires it without blinking. the romance dims." },
        ],
      },
      {
        line: "it froze again. the man in shanghai is stuck mid-gesture. he looks so peaceful.",
        choices: [
          { label: "it's the router. hand me that cat6.", besos: 2, pesos: 0, reply: "your hands brush over the butter-yellow cable." },
          { label: "maybe the call is frozen because you are.", besos: 1, pesos: 0, reply: "'that's... profound.' he writes it down." },
          { label: "packet loss audit: $1200.", besos: -1, pesos: 1200, reply: "he nods grimly. 'invoice the foundation.'" },
        ],
      },
      {
        line: "...it works. shanghai says hello. how do i ever thank you?",
        choices: [
          { label: "stay for coffee. it's already half-drunk.", besos: 3, pesos: 0, reply: "he stays. the monstera approves." },
          { label: "a kiss would settle the invoice.", besos: 2, pesos: 0, reply: "he blushes the color of the cream paper." },
          { label: "wire transfer. you have my routing number.", besos: -2, pesos: 2000, reply: "'of course.' the door closes softly." },
        ],
      },
    ],
  },
  {
    key: "elon",
    name: "elon musk",
    tagline: "his smart house locked him out",
    face: {
      skin: [228, 182, 152],
      hair: [54, 42, 36],
      style: "short",
      shirt: [22, 22, 26], // black tee
      vest: null,
    },
    dates: [
      {
        line: "my house locked me out. the app says 'unexpected error.' i built three rockets today.",
        choices: [
          { label: "rockets are easy. doors are hard. let me look.", besos: 2, pesos: 0, reply: "'finally someone who understands,' he sighs." },
          { label: "did you subscribe to the door?", besos: 1, pesos: 0, reply: "he checks. he had not subscribed to the door." },
          { label: "lockout consult: $950.", besos: -1, pesos: 950, reply: "he pays in stock options. they vest in four years." },
        ],
      },
      {
        line: "now the sprinklers are on. inside the house. is that normal?",
        choices: [
          { label: "no. but it's beautiful. look at the light.", besos: 2, pesos: 0, reply: "you both watch the indoor rain. a moment." },
          { label: "i'll downgrade the firmware from my neo.", besos: 1, pesos: 0, reply: "he watches the green laptop like it's a ufo." },
          { label: "water damage assessment: $1500.", besos: -1, pesos: 1500, reply: "'fine. but i'm tweeting about this.'" },
        ],
      },
      {
        line: "the door opened. the house apologized. nobody's ever fixed anything for me before.",
        choices: [
          { label: "walk me through the wet living room.", besos: 3, pesos: 0, reply: "you leave two sets of footprints in the hall." },
          { label: "next time, just use a key.", besos: 2, pesos: 0, reply: "'a key...' he murmurs. 'disruptive.'" },
          { label: "my rate doubled while we talked.", besos: -2, pesos: 1900, reply: "he respects it. but he doesn't love it." },
        ],
      },
    ],
  },
  {
    key: "marc",
    name: "marc andreessen",
    tagline: "his blog post deleted itself",
    face: {
      skin: [236, 196, 168],
      hair: [210, 200, 188],
      style: "bald",
      shirt: [120, 128, 138], // grey polo
      vest: null,
    },
    dates: [
      {
        line: "my 40,000-word post about the future deleted itself. it was called 'why everything is good now.'",
        choices: [
          { label: "nothing is ever deleted. let me check the cache.", besos: 2, pesos: 0, reply: "'you're a builder,' he says, misting up." },
          { label: "maybe the draft knew something we don't.", besos: 1, pesos: 0, reply: "he laughs once, like a printer jam clearing." },
          { label: "data recovery: $1100.", besos: -1, pesos: 1100, reply: "he pays and calls it 'aligned incentives.'" },
        ],
      },
      {
        line: "you found 39,000 words. the missing thousand were the best ones. probably.",
        choices: [
          { label: "the best words are the ones you cut.", besos: 2, pesos: 0, reply: "he stares into the middle distance. 'it's time to edit.'" },
          { label: "i can regenerate them from the rhythm.", besos: 1, pesos: 0, reply: "'like jazz,' he nods. it was not like jazz." },
          { label: "per-word recovery fee. $1 a word.", besos: -1, pesos: 1000, reply: "he counts it as a seed round." },
        ],
      },
      {
        line: "it's published. it's trending. i couldn't have shipped without you.",
        choices: [
          { label: "celebrate with me. no screens. just snacks.", besos: 3, pesos: 0, reply: "you split a sleeve of crackers in silence. perfect." },
          { label: "put me in the acknowledgments.", besos: 2, pesos: 0, reply: "footnote 114. it's something." },
          { label: "equity. point five percent.", besos: -2, pesos: 2200, reply: "'done.' the friendship converts to a SAFE." },
        ],
      },
    ],
  },
  {
    key: "bill",
    name: "bill gates",
    tagline: "the printer says PC LOAD LETTER",
    face: {
      skin: [234, 192, 162],
      hair: [176, 150, 110],
      style: "side",
      shirt: [96, 60, 120], // purple sweater
      vest: null,
      glasses: [60, 60, 70],
    },
    dates: [
      {
        line: "the printer says PC LOAD LETTER. i have given this industry forty years and i don't know what that means.",
        choices: [
          { label: "nobody does, bill. nobody ever has. sit down.", besos: 2, pesos: 0, reply: "he sits. forty years of tension leave his shoulders." },
          { label: "it means the paper tray is lonely.", besos: 1, pesos: 0, reply: "he feeds it paper, gently. it whirs with joy." },
          { label: "legacy hardware surcharge: $700.", besos: -1, pesos: 700, reply: "he pays by check. the printer prints the receipt. ironic." },
        ],
      },
      {
        line: "now it's printing every page i've queued since 1998. there are... many pages.",
        choices: [
          { label: "let them print. it's your whole life. look.", besos: 2, pesos: 0, reply: "page 4,022 is a clippy memo. you both go quiet." },
          { label: "i'll cancel the queue from my neo.", besos: 1, pesos: 0, reply: "'that green machine,' he says. 'we never made one of those.'" },
          { label: "toner is on you. $900.", besos: -1, pesos: 900, reply: "he signs the toner receipt with a tiny sigh." },
        ],
      },
      {
        line: "it printed a test page with a smiling dog on it. i feel like i can finally rest. thank you.",
        choices: [
          { label: "keep the dog page. frame it together.", besos: 3, pesos: 0, reply: "two pushpins. one wall. the dog smiles forever." },
          { label: "you owe me one (1) hug.", besos: 2, pesos: 0, reply: "a careful, well-architected hug. solid foundation." },
          { label: "consulting retainer. monthly.", besos: -2, pesos: 2400, reply: "he signs. the warmth files itself away." },
        ],
      },
    ],
  },
];

// 🧠 State.
let state = "title"; // title | select | date | fin
let ceo = null; // current date (a CEOS entry)
let q = 0; // question index within the date
let phase = "ask"; // ask | reply | verdict (within a date)
let picked = -1; // choice index taken this question
let dateBesos = 0; // besos earned with the current ceo
let besos = 0; // total kisses won (one per conquered ceo)
let pesos = 0; // total pesos invoiced across all dates
let progress = {}; // key -> { kissed, pesos } once dated
let hover = -1; // hovered choice / card index
let sel = 0; // keyboard selection index
let typed = 0; // typewriter reveal counter for the current line
let frames = 0;
let snd = null;
let melody = []; // scheduled notes, nom-style: { at, type, tone, duration, volume }
let floaters = []; // { x, y, vy, life, text, col } little ❤/$ pops
let layout = null; // rebuilt in paint, hit-tested in act

function reset() {
  state = "title";
  ceo = null;
  q = 0;
  phase = "ask";
  picked = -1;
  dateBesos = 0;
  besos = 0;
  pesos = 0;
  progress = {};
  hover = -1;
  sel = 0;
  typed = 0;
  floaters = [];
}

function boot() {
  reset();
}

// 🎵 nom-style scheduled blips.
function note({ type = "sine", tone, duration = 0.1, volume = 0.3 }) {
  snd?.synth?.({
    type, tone, attack: 0.004, decay: duration * 0.5,
    sustain: 0, release: 0.04, volume, duration,
  });
}

function playMelody(notes, gap = 5) {
  let t = 0;
  notes.forEach((n) => {
    melody.push({ at: frames + t, type: n.type || "sine", tone: n.tone, duration: n.dur || 0.1, volume: n.vol || 0.25 });
    t += gap;
  });
}

const blip = () => note({ tone: 520, duration: 0.03, volume: 0.15 });
const smooch = () => playMelody([{ tone: 880 }, { tone: 1175 }, { tone: 1568, dur: 0.2 }], 4);
const cha = () => playMelody([{ type: "square", tone: 1320, vol: 0.12 }, { type: "square", tone: 1760, vol: 0.12, dur: 0.16 }], 3);
const womp = () => playMelody([{ tone: 220, dur: 0.16 }, { tone: 165, dur: 0.25 }], 7);

function spawnFloater(text, col, x, y) {
  floaters.push({ x, y, vy: -0.6, life: 60, text, col });
}

function sim() {
  frames += 1;
  typed += 1.4; // typewriter speed (chars per frame-ish)
  melody = melody.filter((n) => {
    if (frames < n.at) return true;
    note(n);
    return false;
  });
  floaters.forEach((f) => { f.y += f.vy; f.life -= 1; });
  floaters = floaters.filter((f) => f.life > 0);
}

// The line currently being typed out (for the typewriter + advance gating).
function currentLine() {
  if (state !== "date" || !ceo) return "";
  if (phase === "ask") return ceo.dates[q].line;
  if (phase === "reply") return ceo.dates[q].choices[picked].reply;
  return "";
}

function choose(i, { x = 0, y = 0 } = {}) {
  const c = ceo.dates[q].choices[i];
  picked = i;
  dateBesos += c.besos;
  if (c.besos > 0) { smooch(); spawnFloater("+" + c.besos, [255, 105, 150], x, y); }
  else if (c.besos < 0) { womp(); spawnFloater("" + c.besos, [150, 110, 130], x, y); }
  if (c.pesos > 0) { cha(); pesos += c.pesos; spawnFloater("+$" + c.pesos, [120, 220, 120], x + 20, y); }
  phase = "reply";
  typed = 0;
}

function advance() {
  if (state === "title") { state = "select"; sel = 0; blip(); return; }

  if (state === "select") return; // select advances by picking a card, not tapping through

  if (state === "date") {
    // Don't skip mid-typewriter — first tap completes the line.
    if (typed < currentLine().length && phase !== "verdict") { typed = 9999; return; }
    if (phase === "reply") {
      if (q + 1 < ceo.dates.length) {
        q += 1; phase = "ask"; typed = 0; sel = 0; blip();
      } else {
        phase = "verdict"; typed = 0;
        const kissed = dateBesos >= KISS_AT;
        progress[ceo.key] = { kissed, besos: dateBesos };
        if (kissed) { besos += 1; smooch(); } else womp();
      }
    } else if (phase === "verdict") {
      const allDated = CEOS.every((c) => progress[c.key]);
      state = allDated ? "fin" : "select";
      if (state === "fin") playMelody([{ tone: 523 }, { tone: 659 }, { tone: 784 }, { tone: 1047, dur: 0.3 }], 5);
      sel = 0;
      blip();
    }
    return;
  }

  if (state === "fin") { reset(); state = "select"; blip(); }
}

function startDate(c) {
  ceo = c;
  q = 0;
  phase = "ask";
  picked = -1;
  dateBesos = 0;
  typed = 0;
  sel = 0;
  playMelody([{ tone: 392 }, { tone: 494 }, { tone: 587, dur: 0.18 }], 5);
}

function act({ event: e, sound }) {
  snd = sound;

  if (e.is("keyboard:down:escape") && state === "date") {
    // Walking out mid-date counts as a dud (gentle, but the door is real).
    progress[ceo.key] = { kissed: false, besos: dateBesos };
    state = "select"; womp();
    return;
  }

  // Number keys = direct choice; arrows + enter = browse.
  const items = state === "select" ? CEOS.filter((c) => !progress[c.key]).length
    : state === "date" && phase === "ask" ? ceo.dates[q].choices.length : 0;

  if (items > 0) {
    if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:arrowright")) { sel = (sel + 1) % items; blip(); }
    if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:arrowleft")) { sel = (sel + items - 1) % items; blip(); }
    for (let i = 0; i < items; i += 1) {
      if (e.is(`keyboard:down:${i + 1}`)) {
        if (state === "select") pickCard(i);
        else if (typed >= currentLine().length) choose(i, choiceCenter(i));
      }
    }
    if (e.is("keyboard:down:enter") || e.is("keyboard:down:return") || e.is("keyboard:down:space")) {
      if (state === "select") pickCard(sel);
      else if (typed < currentLine().length) typed = 9999;
      else choose(sel, choiceCenter(sel));
      return;
    }
  } else if (e.name?.startsWith?.("keyboard:down")) {
    advance();
    return;
  }

  if (e.is("move") || e.is("draw")) {
    hover = hitTest(e.x, e.y);
    if (hover >= 0) sel = hover;
  }

  if (e.is("touch")) {
    const hit = hitTest(e.x, e.y);
    if (state === "select") {
      if (hit >= 0) pickCard(hit);
    } else if (state === "date" && phase === "ask" && typed >= currentLine().length) {
      if (hit >= 0) choose(hit, choiceCenter(hit));
      // a tap off the buttons does nothing mid-question — the date waits.
    } else {
      advance();
    }
  }
}

// The i-th selectable card (select screen counts only un-dated ceos).
function pickCard(i) {
  const open = CEOS.filter((c) => !progress[c.key]);
  if (open[i]) { startDate(open[i]); }
}

function hitTest(px, py) {
  if (!layout) return -1;
  const boxes = layout.hits || [];
  for (let i = 0; i < boxes.length; i += 1) {
    const b = boxes[i];
    if (px >= b.x && px < b.x + b.w && py >= b.y && py < b.y + b.h) return i;
  }
  return -1;
}

function choiceCenter(i) {
  const b = layout?.hits?.[i];
  return b ? { x: b.x + b.w / 2, y: b.y } : { x: 0, y: 0 };
}

// ✏️ Word-wrap for the default 6px-advance font.
function wrap(str, maxChars) {
  const words = String(str).split(" ");
  const lines = [];
  let line = "";
  words.forEach((w) => {
    if ((line + " " + w).trim().length > maxChars) { lines.push(line.trim()); line = w; }
    else line += " " + w;
  });
  if (line.trim()) lines.push(line.trim());
  return lines;
}

// 🖍️ Parametric pixel portrait — head + hair + glasses + shirt, all boxes.
// (x, y) is the chin center; s is the unit pixel size; mood bends the mouth.
function drawCEO(ink, box, c, x, y, s, mood = 0) {
  const f = c.face;
  const u = (n) => round(n * s);
  // shoulders + shirt
  ink(...f.shirt).box(x - u(7), y + u(1), u(14), u(6));
  if (f.vest) {
    ink(...f.vest).box(x - u(7), y + u(1), u(3.5), u(6));
    ink(...f.vest).box(x + u(3.5), y + u(1), u(3.5), u(6));
  }
  // neck + head
  ink(...f.skin).box(x - u(1.5), y - u(1), u(3), u(2));
  ink(...f.skin).box(x - u(4), y - u(9), u(8), u(8));
  // hair
  ink(...f.hair).box(x - u(4), y - u(10), u(8), u(2)); // crown
  if (f.style === "wavy") {
    ink(...f.hair).box(x - u(5), y - u(9.5), u(1.5), u(4));
    ink(...f.hair).box(x + u(3.5), y - u(9.5), u(1.5), u(4));
  } else if (f.style === "side") {
    ink(...f.hair).box(x - u(4), y - u(9.5), u(6), u(1.5)); // side part sweep
  } else if (f.style === "bald") {
    ink(...f.skin).box(x - u(4), y - u(10), u(8), u(2)); // erase the crown
    ink(...f.hair).box(x - u(4.4), y - u(7), u(1), u(2.5)); // temple wisps
    ink(...f.hair).box(x + u(3.4), y - u(7), u(1), u(2.5));
  }
  // eyes (+ glasses)
  const eyeY = y - u(6);
  ink(30, 30, 40).box(x - u(2.5), eyeY, u(1), u(1));
  ink(30, 30, 40).box(x + u(1.5), eyeY, u(1), u(1));
  if (f.glasses) {
    ink(...f.glasses).box(x - u(3.2), eyeY - u(0.7), u(2.4), u(2.2), "outline");
    ink(...f.glasses).box(x + u(0.8), eyeY - u(0.7), u(2.4), u(2.2), "outline");
    ink(...f.glasses).box(x - u(0.8), eyeY + u(0.2), u(1.6), u(0.4));
  }
  // mouth — mood 1 smile, 0 flat, -1 frown; blush when smiling
  const mY = y - u(3);
  ink(160, 80, 80).box(x - u(1), mY, u(2), u(0.6));
  if (mood > 0) {
    ink(160, 80, 80).box(x - u(1.6), mY - u(0.6), u(0.7), u(0.7));
    ink(160, 80, 80).box(x + u(0.9), mY - u(0.6), u(0.7), u(0.7));
    ink(255, 150, 160, 140).box(x - u(3.8), y - u(4.4), u(1.4), u(1));
    ink(255, 150, 160, 140).box(x + u(2.4), y - u(4.4), u(1.4), u(1));
  } else if (mood < 0) {
    ink(160, 80, 80).box(x - u(1.6), mY + u(0.5), u(0.7), u(0.7));
    ink(160, 80, 80).box(x + u(0.9), mY + u(0.5), u(0.7), u(0.7));
  }
}

// ❤️ Tiny bitmap heart.
const HEART = ["0110110", "1111111", "1111111", "0111110", "0011100", "0001000"];
function drawHeart(ink, box, x, y, s, col) {
  ink(...col);
  for (let r = 0; r < HEART.length; r += 1) {
    for (let c = 0; c < 7; c += 1) {
      if (HEART[r][c] === "1") box(x + c * s, y + r * s, s, s);
    }
  }
}

function paint({ wipe, ink, box, screen, write }) {
  const w = screen.width;
  const h = screen.height;
  wipe(26, 12, 28); // dusky telenovela plum
  layout = { hits: [] };

  const charW = 6;
  const maxChars = max(12, floor((w - 16) / charW));

  // HUD — besos top-left, pesos top-right (skip on title).
  if (state !== "title") {
    drawHeart(ink, box, 6, 6, 1, [255, 90, 140]);
    ink(255, 190, 210).write(`${besos}`, { x: 16, y: 6 });
    const ptxt = `$${pesos}`;
    ink(140, 230, 140).write(ptxt, { x: w - 8 - ptxt.length * charW, y: 6 });
  }

  if (state === "title") {
    const cx = w / 2;
    // floating hearts + pesos backdrop
    for (let i = 0; i < 8; i += 1) {
      const fy = (frames * (0.3 + (i % 3) * 0.2) + i * 47) % (h + 20) - 10;
      const fx = (i * 73 + 20) % max(1, w - 14);
      if (i % 2) drawHeart(ink, box, fx, h - fy, 1, [120, 40, 70]);
      else ink(50, 90, 50).write("$", { x: fx, y: h - fy });
    }
    ink(255, 120, 160).write("besos", { x: cx - 5 * charW * 2, y: h / 3, size: 2 });
    ink(140, 230, 140).write("pesos", { x: cx, y: h / 3 + 18, size: 2 });
    ink(230, 210, 220).write("a ceo dating sim", { x: cx - 8 * charW / 2 - charW * 4, y: h / 3 + 44 });
    if ((frames >> 5) % 2 === 0) {
      ink(255, 230, 120).write("tap to fall in love", { x: cx - 9.5 * charW, y: h - 24 });
    }
    return;
  }

  if (state === "select") {
    ink(230, 210, 220).write("who needs you today?", { x: 8, y: 20 });
    const open = CEOS.filter((c) => !progress[c.key]);
    const cardH = max(34, floor((h - 56) / CEOS.length) - 4);
    let cy = 34;
    let openIdx = 0;
    CEOS.forEach((c) => {
      const done = progress[c.key];
      const isSel = !done && openIdx === sel;
      if (!done) {
        if (isSel) ink(70, 36, 66).box(4, cy, w - 8, cardH);
        ink(isSel ? [255, 160, 190] : [120, 80, 110]).box(4, cy, w - 8, cardH, "outline");
        layout.hits.push({ x: 4, y: cy, w: w - 8, h: cardH });
      } else {
        ink(40, 26, 40).box(4, cy, w - 8, cardH, "outline");
      }
      const s = max(2, floor(cardH / 18));
      drawCEO(ink, box, c, 8 + 8 * s, cy + cardH - 7 * s, s, done ? (done.kissed ? 1 : -1) : 0);
      const tx = 8 + 17 * s;
      ink(done ? [120, 100, 115] : [255, 235, 240]).write(c.name, { x: tx, y: cy + 6 });
      if (done) {
        if (done.kissed) { drawHeart(ink, box, tx, cy + 17, 1, [255, 90, 140]); ink(255, 150, 180).write("besada", { x: tx + 10, y: cy + 17 }); }
        else ink(120, 100, 115).write("se fue...", { x: tx + 1, y: cy + 17 });
      } else {
        ink(170, 140, 160).write(wrap(c.tagline, floor((w - tx - 8) / charW))[0] || "", { x: tx, y: cy + 17 });
        openIdx += 1;
      }
      cy += cardH + 4;
    });
    return;
  }

  if (state === "date") {
    const s = max(2, floor(min(w, h) / 56));
    const px = w / 2;
    const py = 18 + 10 * s;
    const mood = phase === "ask" ? 0 : (ceo.dates[q].choices[picked]?.besos ?? 0) > 0 ? 1 : (ceo.dates[q].choices[picked]?.besos ?? 0) < 0 ? -1 : 0;
    drawCEO(ink, box, ceo, px, py, s, phase === "verdict" ? (dateBesos >= KISS_AT ? 1 : -1) : mood);
    ink(255, 235, 240).write(ceo.name, { x: px - (ceo.name.length * charW) / 2, y: py + 8 * s });

    // date besos meter — little hearts under the name
    for (let i = 0; i < KISS_AT; i += 1) {
      drawHeart(ink, box, px - (KISS_AT * 9) / 2 + i * 9, py + 8 * s + 11, 1, i < dateBesos ? [255, 90, 140] : [70, 45, 60]);
    }

    const textY = py + 8 * s + 24;

    if (phase === "verdict") {
      const kissed = dateBesos >= KISS_AT;
      const msg = kissed ? "un beso!!!" : "no beso. solo pesos.";
      ink(kissed ? [255, 130, 170] : [160, 140, 160]).write(msg, { x: px - (msg.length * charW) / 2, y: textY + 8, size: 1 });
      if (kissed) drawHeart(ink, box, px - 7, textY + 24, 2, [255, 90, 140]);
      if ((frames >> 5) % 2 === 0) ink(255, 230, 120).write("tap", { x: px - 9, y: h - 16 });
    } else {
      // dialogue (typewriter)
      const line = currentLine();
      const shown = line.slice(0, floor(typed));
      const lines = wrap(shown, maxChars);
      lines.forEach((ln, i) => {
        ink(phase === "ask" ? [235, 225, 235] : [200, 235, 200]).write(ln, { x: 8, y: textY + i * 10 });
      });

      if (phase === "ask" && typed >= line.length) {
        // choice buttons stacked at the bottom
        const ch = ceo.dates[q].choices;
        const bh = 22;
        let by = h - (bh + 4) * ch.length - 6;
        ch.forEach((c, i) => {
          const isSel = i === sel;
          if (isSel) ink(70, 36, 66).box(4, by, w - 8, bh);
          ink(isSel ? [255, 160, 190] : [120, 80, 110]).box(4, by, w - 8, bh, "outline");
          const lab = wrap(c.label, maxChars - 3);
          ink(isSel ? [255, 240, 245] : [200, 180, 200]).write(`${i + 1}.${lab[0]}`, { x: 9, y: by + (lab.length > 1 ? 3 : 8) });
          if (lab[1]) ink(isSel ? [255, 240, 245] : [200, 180, 200]).write(lab[1], { x: 9 + charW * 2, y: by + 12 });
          layout.hits.push({ x: 4, y: by, w: w - 8, h: bh });
          by += bh + 4;
        });
      } else if (phase === "reply" && typed >= line.length && (frames >> 5) % 2 === 0) {
        ink(255, 230, 120).write("tap", { x: px - 9, y: h - 16 });
      }
    }
  }

  if (state === "fin") {
    const cx = w / 2;
    ink(255, 120, 160).write("fin", { x: cx - charW * 1.5 * 2, y: 24, size: 2 });
    let y = 56;
    CEOS.forEach((c) => {
      const p = progress[c.key] || {};
      if (p.kissed) drawHeart(ink, box, 10, y, 1, [255, 90, 140]);
      ink(p.kissed ? [255, 235, 240] : [130, 110, 125]).write(c.name, { x: 22, y });
      y += 12;
    });
    y += 6;
    ink(255, 190, 210).write(`besos: ${besos}/${CEOS.length}`, { x: 10, y });
    ink(140, 230, 140).write(`pesos: $${pesos}`, { x: 10, y: y + 12 });
    const verdict =
      besos === CEOS.length ? "pure of heart. broke of pocket." :
      besos === 0 && pesos > 0 ? "cold. rich. alone. iconic." :
      pesos > 4000 ? "love is a line item." :
      "some kisses, some invoices. balance.";
    wrap(verdict, maxChars).forEach((ln, i) => {
      ink(230, 210, 220).write(ln, { x: 10, y: y + 30 + i * 10 });
    });
    if ((frames >> 5) % 2 === 0) ink(255, 230, 120).write("tap to love again", { x: cx - 9 * charW, y: h - 16 });
  }

  // floaters on top of everything
  floaters.forEach((f) => {
    ink(...f.col, min(255, f.life * 6)).write(f.text, { x: f.x, y: f.y });
  });
}

function meta() {
  return {
    title: "besospesos · aesthetic.computer",
    desc: "a ceo dating sim. besos or pesos — you can't have both.",
  };
}

export { boot, sim, paint, act, meta };
