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

// The pilot offers just two materials. The remaining scope is narrow and
// stated on the card, not an unseen expansion of permission.
const MATERIALS = [
  { value: "appearance", label: "Photo", on: true },
  { value: "voice", label: "Voice" },
];

export function scopeForMedia({ photo, voice }) {
  return {
    source: [...(photo ? ["appearance"] : []), ...(voice ? ["voice"] : [])],
    outputs: [...(photo ? ["portrait"] : []), ...(voice ? ["match_audio"] : [])],
    distribution: ["private_preview"],
    retention: "bound_to_purpose_scope",
    marketing: false, merchandise: false, model_training: false,
  };
}

const STYLE = `
#wizard-panel { position: fixed; inset: 0; z-index: 21; display: grid;
  place-items: center; padding: 20px; background: rgba(0,0,0,.65); }
#wizard-panel[hidden] { display: none; }
#wizard-card { box-sizing: border-box; width: 100%; max-width: 26rem;
  max-height: 88vh; overflow: auto; display: flex; flex-direction: column;
  gap: 20px; padding: 28px; border: 1px solid #777; border-radius: 0;
  background: #f5f5f0; color: #090909;
  box-shadow: 0 12px 60px #0006; font: 16px/1.45 Arial, Helvetica, sans-serif; }
#wizard-brand { font-size: 21px; font-weight: 700; letter-spacing: -.04em; }
#wizard-brand::after { content: ""; display: inline-block; width: 9px;
  height: 9px; margin-left: 7px; background: #e31b23; }
#wizard-card h2 { margin: 0; font-size: 30px; line-height: 1.1; letter-spacing: -.03em; }
#wizard-bargain { margin: 0; color: #444; }
#wizard-card fieldset { border: 0; margin: 0; padding: 0; display: grid; gap: 12px; }
#wizard-card legend { position: absolute; width: 1px; height: 1px; overflow: hidden;
  clip-path: inset(50%); }
#wizard-card label.row { display: flex; gap: 12px; align-items: center;
  padding: 14px; border: 1px solid #777; cursor: pointer; font-size: 19px; }
#wizard-card label.row input { width: 22px; height: 22px; margin: 0; accent-color: #090909; }
#wizard-note, #wizard-receipt { margin: 0; font-size: 14px; }
#wizard-note:empty, #wizard-receipt:empty { display: none; }
#wizard-note.trouble { color: #b00014; }
#wizard-note.settled { color: #006b3c; }
#wizard-receipt { color: #555; overflow-wrap: anywhere; }
#wizard-actions { display: flex; gap: 10px; }
#wizard-panel button { flex: 1; padding: 12px; border: 1px solid #090909;
  border-radius: 0; font: 700 16px/1.2 Arial, Helvetica, sans-serif;
  background: #090909; color: #f5f5f0; cursor: pointer; }
#wizard-panel button:hover { background: #333; }
#wizard-panel button:focus-visible, #wizard-panel input:focus-visible {
  outline: 3px solid #003399; outline-offset: 3px; }
#wizard-panel button[disabled] { opacity: .5; cursor: default; }
#wizard-panel #wizard-back { background: transparent; color: #090909; }
#wizard-panel #wizard-back:hover { background: #e4e4df; }
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
    <form id="wizard-card" role="dialog" aria-modal="true" aria-labelledby="wizard-title" novalidate>
      <div id="wizard-brand" aria-label="REGARDE">regarde</div>
      <h2 id="wizard-title">Add yourself</h2>
      <p id="wizard-bargain">OSKIEWAR asks to make a portrait or game voice for your private preview. Kept until you withdraw.</p>
      <div id="wizard-sections"></div>
      <p id="wizard-note" role="status" aria-live="polite"></p>
      <p id="wizard-receipt"></p>
      <div id="wizard-actions">
        <button id="wizard-go" type="submit">Allow</button>
        <button id="wizard-back" type="button">Not now</button>
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
  let capability = null;
  const upload = document.createElement("div");
  sections.after(upload);

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

  const set = document.createElement("fieldset");
  const legend = document.createElement("legend");
  legend.textContent = "Material OSKIEWAR may use";
  set.append(legend, ...MATERIALS.map(material => row("source", "check", material)));
  sections.append(set);

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
    capability = null;
    upload.replaceChildren();
    sections.hidden = false;
    go.textContent = "Allow";
    go.disabled = false;
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
    if (capability) {
      const selected = [...upload.querySelectorAll('input[type="file"]')]
        .filter(input => input.files.length);
      if (!selected.length) { say("Choose material to submit.", "trouble"); return; }
      if (selected.reduce((sum, input) => sum + input.files[0].size, 0) > 1024 * 1024) {
        say("Choose files totaling at most 1 MiB.", "trouble"); return;
      }
      working(true);
      say("Submitting…");
      try {
        const files = await Promise.all(selected.map(async input => {
          const bytes = new Uint8Array(await input.files[0].arrayBuffer());
          let binary = "";
          for (const byte of bytes) binary += String.fromCharCode(byte);
          return { source: input.name, base64: btoa(binary) };
        }));
        const response = await fetch("/api/oskiewar-submission", {
          method: "POST", headers: { "Content-Type": "application/json", authorization: "Bearer " + token },
          body: JSON.stringify({ capability: capability.jws, files }),
        });
        const result = await response.json();
        if (!response.ok) throw new Error(result.message || "Submission refused.");
        upload.replaceChildren();
        capability = null;
        working(false);
        go.disabled = true;
        say(result.purge_at ? `Submitted. Purge scheduled for ${new Date(result.purge_at).toLocaleString()}. Nothing generated yet.`
          : "Submitted. Kept while your grant stands. Nothing generated yet.", "settled");
      } catch (error) { working(false); say(error.message, "trouble"); }
      return;
    }
    const chosen = picked("source");
    const answer = scopeForMedia({ photo: chosen.includes("appearance"), voice: chosen.includes("voice") });
    if (!answer.source.length) { say("Choose photo, voice, or both.", "trouble"); return; }

    working(true);
    say("Asking REGARDE…");
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
      if (result.capability?.jws && Array.isArray(result.capability.sources)) {
        capability = result.capability;
        sections.hidden = true;
        upload.replaceChildren();
        const hint = document.createElement("p");
        hint.textContent = "Choose your own material. At most 1 MiB total.";
        upload.append(hint);
        for (const source of capability.sources) {
          const label = document.createElement("label");
          label.style.display = "block";
          label.textContent = MATERIALS.find(row => row.value === source)?.label ?? source;
          const input = document.createElement("input");
          input.type = "file";
          input.style.display = "block";
          input.style.margin = "6px 0 12px";
          input.name = source;
          input.accept = source === "appearance" ? "image/*" : "audio/*";
          label.append(input);
          upload.append(label);
        }
        go.textContent = "Submit";
      }
      sfx("hit", .9, 0);
      say("Allowed. Choose your files.", "settled");
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
