// The "add yourself" wall — the popover behind the head on the title screen.
//
// Every other door in this shell starts a fight. This one asks a question
// first, and the question is the product: which parts of yourself the game may
// use, to make what, to be seen where. The player answers before any material
// is collected and long before any model is invoked, because a scope offered
// after the upload is a formality and a scope offered before it is a decision.
//
// DOM rather than canvas, for the same reason the sign-in door is DOM: these
// are checkboxes and they have to be reachable by a screen reader, a thumb, a
// gamepad's on-screen cursor, and a keyboard. A hand-rolled canvas checkbox
// would be none of those.
//
// This module holds no policy. It renders the ask, collects the answer, and
// sends it to /api/oskiewar-consent, which is the only thing here that can
// reach REGARDE. An outcome other than "allow" ends the flow — there is no
// retry loop, because asking the same desk the same question until it says yes
// is how a consent wall becomes a nag screen.

const ENDPOINT = "/api/oskiewar-consent";

// The scope card, in the order a person thinks about it: what of mine, to
// make what, seen where, kept how long. `note` is the sentence that stops a
// reasonable wrong assumption before it is made.
const SECTIONS = [
  { key: "source", legend: "what of yours", kind: "check", rows: [
    { value: "appearance", label: "how you look",
      note: "photos or a description you write", on: true },
    { value: "movement", label: "how you move" },
    { value: "voice", label: "your voice",
      note: "a recording, used only for what you tick below" },
    { value: "biography", label: "something about you" },
  ] },
  { key: "outputs", legend: "to make", kind: "check", rows: [
    { value: "portrait", label: "a portrait", on: true },
    { value: "fighter_mesh", label: "a body to fight in" },
    { value: "fighter_animation", label: "the way that body moves" },
    { value: "match_audio", label: "a voice in the match",
      note: "needs the voice sample above" },
  ] },
  { key: "distribution", legend: "seen where", kind: "check", rows: [
    { value: "private_preview", label: "only you, as a preview", on: true },
    { value: "local_gameplay", label: "fights on your own machine" },
    { value: "online_play", label: "fights against other people" },
    { value: "tournament_display", label: "shown at a tournament" },
  ] },
  { key: "retention", legend: "kept how long", kind: "radio", rows: [
    { value: "bound_to_purpose_scope", label: "only while this grant stands",
      on: true },
    { value: "pilot_deadline", label: "until the pilot ends, then purged" },
  ] },
];

// Held apart from the card above, and worded as refusals, because these are
// the three a player is most likely to agree to by momentum. Nothing here is
// a degree of playing the game; each is a different thing being done with a
// person.
const SEPARATE = [
  { value: "marketing", label: "may advertise the game" },
  { value: "merchandise", label: "may be printed on something sold" },
  { value: "model_training", label: "may train a model" },
];

const STYLE = `
#wizard-panel { position: fixed; inset: 0; z-index: 21;
  display: grid; place-items: center; padding: 20px;
  background: rgba(4, 7, 18, .86); backdrop-filter: blur(3px); }
#wizard-panel[hidden] { display: none; }
#wizard-card { width: 100%; max-width: 31rem; max-height: 88vh; overflow: auto;
  display: flex; flex-direction: column; gap: 14px;
  padding: 22px; border: 3px solid #6e768d; border-radius: 14px;
  background: #171b28; box-shadow: 5px 7px 0 rgba(4, 7, 18, .55);
  font: 400 16px/1.4 "Comic Relief", "Comic Sans MS", Arial, sans-serif;
  color: #f3f6ff; }
#wizard-card h2 { margin: 0; font-size: 26px; font-weight: 700; }
#wizard-bargain { margin: 0; font-size: 15px; color: #b0b8ca; }
#wizard-card fieldset { margin: 0; padding: 12px 13px; border: 2px solid #3b4256;
  border-radius: 9px; display: flex; flex-direction: column; gap: 9px; }
#wizard-card legend { padding: 0 6px; font-size: 14px; color: #b0b8ca;
  text-transform: lowercase; }
#wizard-card fieldset.separate { border-color: #7a5560; }
#wizard-card label.row { display: flex; gap: 10px; align-items: flex-start;
  cursor: pointer; }
#wizard-card label.row input { margin: 3px 0 0; width: 20px; height: 20px;
  accent-color: #e6cd5c; flex: 0 0 auto; }
#wizard-card .row span { display: block; }
#wizard-card .row .note { font-size: 13px; color: #b0b8ca; }
#wizard-note { margin: 0; min-height: 1.4em; font-size: 15px; color: #b0b8ca; }
#wizard-note.trouble { color: #ff9a8a; }
#wizard-note.settled { color: #9ce6a8; }
#wizard-receipt { margin: 0; font-size: 13px; color: #b0b8ca;
  word-break: break-all; }
#wizard-actions { display: flex; flex-wrap: wrap; gap: 10px; }
#wizard-panel button { appearance: none; flex: 1 1 auto;
  padding: 11px 14px; border: 2px solid #6e768d; border-radius: 9px;
  background: #b0b8ca; color: #171b28;
  font: 700 17px/1 "Comic Relief", "Comic Sans MS", Arial, sans-serif;
  box-shadow: 2px 3px 0 rgba(4, 7, 18, .45); cursor: pointer; }
#wizard-panel button:hover { background: #d9dfee; }
#wizard-panel button:focus-visible { outline: 3px solid #e6cd5c;
  outline-offset: 2px; }
#wizard-panel button[disabled] { opacity: .55; cursor: default; }
#wizard-panel #wizard-back { flex: 0 1 auto; background: #3b4256;
  color: #e8ecf8; }
#wizard-panel #wizard-back:hover { background: #4d566f; }
#wizard-panel.working #wizard-card { opacity: .7; }
`;

export default function mountWizard({ sfx = () => {}, bearer = async () => null } = {}) {
  const style = document.createElement("style");
  style.textContent = STYLE;
  document.head.append(style);

  const panel = document.createElement("div");
  panel.id = "wizard-panel";
  panel.hidden = true;
  panel.innerHTML = `
    <form id="wizard-card" novalidate>
      <h2>add yourself</h2>
      <p id="wizard-bargain">You decide which parts of you the game may use.
        Nothing is collected and nothing is generated until you answer.</p>
      <div id="wizard-sections"></div>
      <p id="wizard-note" role="status" aria-live="polite"></p>
      <p id="wizard-receipt"></p>
      <div id="wizard-actions">
        <button id="wizard-go" type="submit">allow this much</button>
        <button id="wizard-back" type="button">never mind</button>
      </div>
    </form>`;
  document.body.append(panel);

  const card = panel.querySelector("#wizard-card");
  const sections = panel.querySelector("#wizard-sections");
  const note = panel.querySelector("#wizard-note");
  const receiptLine = panel.querySelector("#wizard-receipt");
  const go = panel.querySelector("#wizard-go");
  const back = panel.querySelector("#wizard-back");
  let busy = false;

  const row = (name, kind, { value, label, note: hint, on }) => {
    const wrap = document.createElement("label");
    wrap.className = "row";
    const input = document.createElement("input");
    input.type = kind === "radio" ? "radio" : "checkbox";
    input.name = name;
    input.value = value;
    input.checked = on === true;
    const text = document.createElement("span");
    text.innerHTML = `<span>${label}</span>` +
      (hint ? `<span class="note">${hint}</span>` : "");
    wrap.append(input, text);
    return wrap;
  };

  for (const section of SECTIONS) {
    const set = document.createElement("fieldset");
    const legend = document.createElement("legend");
    legend.textContent = section.legend;
    set.append(legend, ...section.rows.map((r) => row(section.key, section.kind, r)));
    sections.append(set);
  }
  const separate = document.createElement("fieldset");
  separate.className = "separate";
  const separateLegend = document.createElement("legend");
  separateLegend.textContent = "asked separately, off unless you say so";
  separate.append(separateLegend,
    ...SEPARATE.map((r) => row("separate", "check", r)));
  sections.append(separate);

  const picked = (name) => [...card.querySelectorAll(`input[name="${name}"]`)]
    .filter((input) => input.checked).map((input) => input.value);

  function working(state) {
    busy = state;
    panel.classList.toggle("working", state);
    go.disabled = state;
    for (const input of card.querySelectorAll("input")) input.disabled = state;
  }

  function say(message, tone = "") {
    note.textContent = message;
    note.className = tone;
  }

  function open() {
    if (!panel.hidden) return;
    panel.hidden = false;
    globalThis.__oskiewarWizardOpen = true;
    say("");
    receiptLine.textContent = "";
    sfx("block", .9, 0);
    card.querySelector("input")?.focus();
  }

  function close() {
    if (panel.hidden) return;
    panel.hidden = true;
    globalThis.__oskiewarWizardOpen = false;
    working(false);
  }

  back.addEventListener("click", close);
  // Escape is the only way out that does not touch the answer, which matters
  // on a panel whose accidental submit would be a grant.
  addEventListener("keydown", (event) => {
    if (event.key === "Escape" && !panel.hidden) { event.preventDefault(); close(); }
  });

  card.addEventListener("submit", async (event) => {
    event.preventDefault();
    if (busy) return;
    const token = await bearer();
    // Said plainly rather than by bouncing them to the sign-in door: a grant
    // has to belong to somebody who can come back and withdraw it, and that is
    // a reason worth reading.
    if (!token) {
      say("Sign in first — a grant has to belong to someone who can withdraw it.",
        "trouble");
      return;
    }
    const answer = {
      source: picked("source"),
      outputs: picked("outputs"),
      distribution: picked("distribution"),
      retention: picked("retention")[0],
    };
    for (const { value } of SEPARATE) answer[value] = picked("separate").includes(value);

    working(true);
    say("Asking the consent desk…");
    let result;
    try {
      const response = await fetch(ENDPOINT, {
        method: "POST",
        headers: { "Content-Type": "application/json",
          authorization: "Bearer " + token },
        body: JSON.stringify(answer),
      });
      result = await response.json();
    } catch {
      working(false);
      say("The consent desk did not answer. Nothing was collected.", "trouble");
      return;
    }
    working(false);

    if (result?.outcome === "allow") {
      sfx("hit", .9, 0);
      say("Allowed, and recorded. Nothing has been generated yet.", "settled");
      receiptLine.textContent = result.receipt?.hash
        ? `receipt ${result.receipt.hash}` : "";
      return;
    }
    if (result?.outcome === "edit") {
      // The desk countered. Shown, never auto-accepted: narrower terms are
      // still terms, and they are the player's to take or leave.
      say("The desk offered narrower terms instead. Adjust and ask again.",
        "trouble");
      receiptLine.textContent = result.counter
        ? "offered: " + JSON.stringify(result.counter) : "";
      return;
    }
    say(result?.message || "Not granted. No material was taken and nothing was made.",
      "trouble");
    receiptLine.textContent = "";
  });

  return { open, close, get isOpen() { return !panel.hidden; } };
}
