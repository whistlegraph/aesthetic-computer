// model-router.mjs — the model a piece request gets when nobody chose one.
//
// The ask is sorted into one of three tiers before any work starts, and each
// tier has one model. Rules only, no classifier call: they are free, instant
// and testable, and the trial (bin/route-trial.mjs) says whether they miss
// often enough to need a model reading the ask too.
//
// Within a turn the tier can rise once per two failed checks, never fall:
// a cheap model that cannot clear `ac check` hands the turn up rather than
// spending more rounds. A new turn starts from its own ask again.
//
// No imports, so the phone's browser build can read it like open-models.mjs.

// Tier → model name in HOSTED_MODEL_INFO (open-models.mjs).
export const ROUTE_TIERS = ["small", "normal", "hard"];
export const ROUTE_MODELS = { small: "qwen", normal: "flash", hard: "sonnet" };

// Two failed checks in one turn before the turn moves up a tier.
export const ESCALATE_AFTER = 2;

// Words that put an ask out of reach of the default model outright.
const HARD_SIGNALS = [
  [/\b(multi-?player|multiplayer|online|other players?|two players? on|network(ed)?|chat room|lobby|net\.)\b/i, "multiplayer"],
  [/\b(3d|3-d|three[- ]d|first[- ]person|camera orbit|mesh|raytrac\w*|voxel\w*|perspective)\b/i, "3D"],
  [/\b(kidlisp|\.lisp)\b/i, "KidLisp"],
];

// Systems a game or tool can be made of. Three or more in one ask is hard.
const SYSTEMS = [
  /\b(levels?|stages?|waves?)\b/i,
  /\b(score|high ?score|points|lives)\b/i,
  /\b(enem(y|ies)|bosses?|monsters?|npcs?)\b/i,
  /\b(physics|gravity|collisions?|bounce|friction)\b/i,
  /\b(inventory|items?|power-?ups?|pickups?|shop)\b/i,
  /\b(menus?|title screen|pause|settings)\b/i,
  /\b(save|load|persist|remember)\b/i,
  /\b(procedural\w*|random(ly)? generated|generat(e|es|ed) (a |the )?(map|world|level))\b/i,
  /\b(ai|pathfind\w*|chase|flee)\b/i,
  /\b(sound|music|notes?|drums?|synth|melody|beat)\b/i,
  /\b(drag|swipe|tap|keys?|keyboard|mouse|gamepad|tilt)\b/i,
  /\b(animat\w*|particles?|trails?)\b/i,
];

// A short ask against a piece that already exists, changing what is there.
const TWEAK = /\b(make|change|set|turn|swap|rename|use|slower|faster|bigger|smaller|darker|lighter|brighter|thicker|thinner|more|less|fewer|red|blue|green|yellow|pink|purple|orange|black|white|colou?rs?|size|speed)\b/i;
// …unless it reaches for something the piece does not do yet.
const NEW_FEATURE = /\b(add|now (each|every|it|they|the)|also|new|another|plays?|sounds?|music|game|when (i|you) (tap|click|press|drag))\b/i;

const words = (text) => (text.match(/\S+/g) || []).length;
const lines = (source) => (source ? source.split("\n").length : 0);

// A session opens on a blank (runtimes.mjs) before anything is asked, so a
// blank is no piece at all: the first ask is a new piece, not an edit. The
// KidLisp blank carries no marker, only a name line and a wipe.
export function blankPiece(source = "") {
  if (/A blank (Aesthetic Computer piece|L5 piece|p5\.js sketch)[.:]/.test(source)) return true;
  return source.split("\n").filter((line) => line.trim()).length <= 3;
}

// The ask's text and any images, whatever shape the message content took.
export function askParts(content) {
  if (typeof content === "string") return { text: content, images: 0 };
  if (!Array.isArray(content)) return { text: "", images: 0 };
  return {
    text: content.filter((b) => b?.type === "text").map((b) => b.text || "").join("\n"),
    images: content.filter((b) => b?.type === "image").length,
  };
}

// The tier for one ask. `source` is the piece as it stands ("" for a new one).
export function routeAsk({ text = "", images = 0, source = "" } = {}) {
  const ask = String(text);
  const reasons = [];
  const size = blankPiece(source) ? 0 : lines(source);

  for (const [pattern, why] of HARD_SIGNALS) if (pattern.test(ask)) reasons.push(why);
  if (images > 0) reasons.push("reference image");
  if (size > 400) reasons.push(`${size}-line piece`);
  const systems = SYSTEMS.filter((p) => p.test(ask)).length;
  if (systems >= 4) reasons.push(`${systems} systems`);
  if (words(ask) > 120) reasons.push("long ask");
  if (reasons.length) return route("hard", reasons);

  if (size > 0 && words(ask) <= 14 && TWEAK.test(ask) && !NEW_FEATURE.test(ask)) {
    return route("small", ["small edit"]);
  }
  return route("normal", [size ? "revision" : "new piece"]);
}

// One tier up, or the same route when it is already at the top.
export function escalate(current) {
  const next = ROUTE_TIERS[ROUTE_TIERS.indexOf(current.tier) + 1];
  if (!next) return current;
  return { ...route(next, [...current.reasons, "failed checks"]), escalated: true };
}

function route(tier, reasons) {
  return { tier, model: ROUTE_MODELS[tier], reasons };
}
