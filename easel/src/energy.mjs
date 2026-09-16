// energy.mjs — roughly what a turn cost in electricity.
//
// A session here is someone making a picture by talking to a machine in a
// datacenter, and nothing in the interface has ever said what that costs to
// run. Tokens are the wrong unit for the question: they are the provider's
// billing unit, they are not comparable between models, and nobody has an
// intuition for twelve thousand of them. Watt-hours are a unit people already
// own — a lightbulb, a kettle, a phone charge.
//
// What this is honest about: it is an estimate, and it cannot be anything else.
// No hosted provider publishes per-token energy, and the two frontier labs that
// have published anything at all published a per-prompt median, not a model
// card. So the number here is derived, and the derivation is written down so it
// can be argued with:
//
//   1. Generating one token runs the model's *active* parameters once. For the
//      open-weight models Aesel hosts, that count is published (a mixture of
//      experts announces both numbers: GLM-4.6 is 355B total, 32B active), so
//      the models differ by a factor this formula can actually see.
//   2. Energy per output token is taken as a fixed part plus a part that scales
//      with active parameters. The fixed part is everything that does not care
//      how big the model is — host CPU, memory, networking, cooling, and the
//      share of an idle-but-provisioned accelerator — which published
//      full-stack figures put at a large fraction of the total.
//   3. The slope is anchored so a frontier-class model lands near the only
//      measurements anyone has released: Google's median text prompt at 0.24 Wh
//      (Aug 2025, full-stack, including idle and overhead), Epoch AI's estimate
//      of ~0.3 Wh for a GPT-4o query, and OpenAI's own ~0.34 Wh average. At a
//      few hundred output tokens per answer those all land at 1–3 J per token.
//   4. Reading the prompt is cheap per token compared to writing the answer.
//      Prefill runs dense and batched; decode is memory-bound and runs one
//      token at a time. An input token is counted at a tenth of an output
//      token, a cached one at a hundredth — cheap, but not free, because the
//      cache still has to be read out of memory and attended to.
//
// What it excludes: training, the water, the embodied cost of the hardware, and
// your own laptop. It is the marginal electricity of serving the turn.
//
// Two consequences for how this gets shown. Absolute watt-hours carry a
// precision they have not earned, so every number that reaches a person wears a
// `~`. And the comparison that *is* defensible is the relative one — the same
// conversation on a 32B-active model and on a frontier model differ by a factor
// the formula derives from published parameter counts rather than from guessed
// hardware — so `/energy` leads with the ratio and treats the watt-hours as the
// supporting detail.

// Joules per output token: a floor that every model pays, plus a slope on
// billions of active parameters. Anchored at 200B active ≈ 2.4 J/token, which
// puts a 300-token answer at 0.2 Wh — inside the published per-prompt range.
const FIXED_J = 0.6;
const PER_BILLION_J = 0.009;

// What a token of each other kind costs, as a share of one output token.
const SHARE = {
  input: 0.1,
  cacheWrite: 0.125, // prefill, plus writing the block out.
  cacheRead: 0.01,
};

// The basis line every readout carries, so the numbers are never mistaken for
// measurements.
export const BASIS =
  "Estimated from active parameters and published per-prompt figures (Google 0.24 Wh median; Epoch ~0.3 Wh). Serving only — no training, water or your own machine.";

// Active parameters in billions. `known` marks the difference between a number
// the lab published and one this file guessed, because that difference is the
// whole reason to trust or distrust a row.
//
// The hosted models are open-weight mixtures of experts and announce both
// counts. The closed ones announce nothing, so they are placed by class — which
// is a guess, and says so wherever it is printed.
const HOSTED = {
  "z-ai/glm-4.6": { label: "glm", active: 32, known: true },
  "qwen/qwen3-coder": { label: "qwen", active: 35, known: true },
  "deepseek/deepseek-chat-v3.1": { label: "deepseek", active: 37, known: true },
  "anthropic/claude-sonnet-4.6": { label: "sonnet", active: 200, known: false },
  "openai/gpt-5.4": { label: "gpt", active: 300, known: false },
};

// Vendor-CLI models, matched by family. A session on `/backend claude` can name
// any model its subscription allows, so the fallback has to be a family rather
// than a list — and an unrecognized name is placed at the frontier class rather
// than at the cheap end, so an unknown model is never flattered.
const FAMILIES = [
  [/opus/i, { active: 500, known: false }],
  [/fable/i, { active: 250, known: false }],
  [/sonnet/i, { active: 200, known: false }],
  [/haiku/i, { active: 40, known: false }],
  [/gpt-5|o[34]|codex/i, { active: 300, known: false }],
  [/mini|flash|small|lite/i, { active: 40, known: false }],
];

const UNKNOWN = { active: 200, known: false };

export function profileFor(model) {
  const id = String(model || "").trim();
  if (Object.hasOwn(HOSTED, id)) return { id, ...HOSTED[id] };
  // `/model glm` names a model by the short name the hosted endpoint allowlists
  // under. The bridge resolves it before reporting, but a session that never
  // heard back from one still knows what it asked for.
  for (const [hosted, profile] of Object.entries(HOSTED)) {
    if (profile.label === id.toLowerCase()) return { id: hosted, ...profile };
  }
  for (const [pattern, profile] of FAMILIES) {
    if (pattern.test(id)) return { id, label: id, ...profile };
  }
  return { id, label: id || "unknown", ...UNKNOWN };
}

function perOutputToken(active) {
  return FIXED_J + PER_BILLION_J * active;
}

// Providers name these fields differently — Anthropic's cache pair, Codex's
// `cached_input_tokens` — so every caller can hand over whatever it was given.
export function readUsage(usage = {}) {
  const number = (value) => (Number.isFinite(value) && value > 0 ? Math.round(value) : 0);
  return {
    input: number(usage.input_tokens ?? usage.inputTokens),
    output: number(usage.output_tokens ?? usage.outputTokens),
    cacheRead: number(
      usage.cache_read_input_tokens ??
        usage.cacheReadInputTokens ??
        usage.cached_input_tokens ??
        usage.cachedInputTokens,
    ),
    cacheWrite: number(usage.cache_creation_input_tokens ?? usage.cacheCreationInputTokens),
  };
}

export function joulesFor(tokens, model) {
  const { active } = profileFor(model);
  const perToken = perOutputToken(active);
  return (
    tokens.output * perToken +
    tokens.input * perToken * SHARE.input +
    tokens.cacheWrite * perToken * SHARE.cacheWrite +
    tokens.cacheRead * perToken * SHARE.cacheRead
  );
}

export function formatJoules(joules) {
  if (!(joules > 0)) return "0 Wh";
  const wh = joules / 3600;
  if (wh < 0.001) return `${joules.toFixed(0)} J`;
  if (wh < 0.1) return `${wh.toFixed(3)} Wh`;
  if (wh < 10) return `${wh.toFixed(2)} Wh`;
  return `${wh.toFixed(1)} Wh`;
}

// One everyday equivalent, picked so the number in front of it is small enough
// to picture. Watt-hours are a unit people own once something is plugged into
// them.
const APPLIANCES = [
  { watts: 10, name: "an LED bulb" },
  { watts: 30, name: "a laptop" },
  { watts: 2000, name: "an electric kettle" },
];

export function everyday(joules) {
  if (!(joules > 0)) return "";
  // The bulb answers almost everything a session can spend, which is the point:
  // one appliance across a whole range keeps consecutive readouts comparable.
  // Bigger draws are borrowed only once the bulb's own number stops being
  // pictureable.
  for (const { watts, name } of APPLIANCES) {
    const seconds = joules / watts;
    if (seconds <= 120) return `${name} for ${seconds < 10 ? seconds.toFixed(1) : seconds.toFixed(0)} s`;
    const minutes = seconds / 60;
    if (minutes <= 90) return `${name} for ${minutes.toFixed(0)} min`;
  }
  const hours = joules / 2000 / 3600;
  return `an electric kettle for ${hours.toFixed(1)} h`;
}

// A phone charge is the other intuition people have, and it is the one that
// makes a whole session legible rather than a single turn.
export function phoneCharges(joules, wattHours = 15) {
  return joules / 3600 / wattHours;
}

// The tally a session keeps. Per-model, because switching models mid-session is
// one command and the point of the whole readout is that the choice matters.
export class Energy {
  constructor() {
    this.joules = 0;
    this.turns = 0;
    this.tokens = { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 };
    this.byModel = new Map();
  }

  get counted() {
    return this.tokens.input + this.tokens.output + this.tokens.cacheRead + this.tokens.cacheWrite;
  }

  // `usage` is whatever the bridge was handed; `model` is what ran it.
  add(model, usage) {
    const tokens = readUsage(usage);
    if (!(tokens.input + tokens.output + tokens.cacheRead + tokens.cacheWrite)) return 0;
    const joules = joulesFor(tokens, model);
    this.joules += joules;
    this.turns += 1;
    for (const key of Object.keys(this.tokens)) this.tokens[key] += tokens[key];
    const id = String(model || "unknown");
    const seen = this.byModel.get(id) || { joules: 0, tokens: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 } };
    seen.joules += joules;
    for (const key of Object.keys(seen.tokens)) seen.tokens[key] += tokens[key];
    this.byModel.set(id, seen);
    return joules;
  }
}

// What this session's tokens would have cost on each hosted model, cheapest
// first and expressed as a multiple of the cheapest. This is the defensible
// half of the estimate: the ratios come from published active-parameter counts,
// so they hold even if the absolute watt-hours are off by a factor.
export function relativeModels(tokens, current = "") {
  const rows = Object.keys(HOSTED).map((id) => {
    const profile = profileFor(id);
    return { ...profile, joules: joulesFor(tokens, id), current: id === current };
  });
  rows.sort((a, b) => a.joules - b.joules);
  const floor = rows[0]?.joules || 0;
  for (const row of rows) row.ratio = floor > 0 ? row.joules / floor : 1;
  return rows;
}

// The `/energy` readout, as lines. Built here rather than in the interface so
// the wording and the caveat travel with the arithmetic.
export function energyReport(energy, model = "") {
  if (!energy || !energy.counted) {
    return [
      "No metered turns yet — energy is counted from the usage the engine reports.",
      BASIS,
    ];
  }
  const { tokens } = energy;
  const lines = [
    `~${formatJoules(energy.joules)} this session · ${energy.turns} metered turn${energy.turns === 1 ? "" : "s"} · ${everyday(energy.joules)}`,
    `${tokens.output.toLocaleString()} written · ${tokens.input.toLocaleString()} read · ${tokens.cacheRead.toLocaleString()} cached`,
  ];
  const charges = phoneCharges(energy.joules);
  if (charges >= 0.01) lines.push(`About ${charges < 1 ? `${(charges * 100).toFixed(0)}% of` : `${charges.toFixed(1)}×`} a phone charge.`);

  if (energy.byModel.size > 1) {
    lines.push("");
    for (const [id, seen] of energy.byModel) {
      lines.push(`  ${(profileFor(id).label || id).padEnd(9)} ~${formatJoules(seen.joules)}`);
    }
  }

  lines.push("");
  lines.push("Same conversation, other models:");
  for (const row of relativeModels(tokens, model)) {
    const bar = "█".repeat(Math.max(1, Math.min(24, Math.round(row.ratio * 3))));
    lines.push(
      `  ${(row.label || row.id).padEnd(9)} ${bar} ${row.ratio.toFixed(1)}× · ~${formatJoules(row.joules)}` +
        `${row.known ? "" : " (size undisclosed; estimated)"}${row.current ? "  ← running" : ""}`,
    );
  }
  lines.push("");
  lines.push(BASIS);
  return lines;
}
