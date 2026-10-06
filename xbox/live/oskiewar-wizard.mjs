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

import { mountFighterPreview, validateFighter } from "./oskiewar-fighter.mjs";

const ENDPOINT = "/api/oskiewar-consent";

// The pilot offers a photo. The remaining scope is narrow and
// stated on the card, not an unseen expansion of permission.
const MATERIALS = [
  { value: "appearance", label: "Photo", on: true },
];

export function scopeForMedia({ photo, voice }) {
  return {
    source: [...(photo ? ["appearance"] : []), ...(voice ? ["voice"] : [])],
    outputs: [...(photo ? ["fighter_mesh"] : []), ...(voice ? ["match_audio"] : [])],
    distribution: ["private_preview", "local_gameplay"],
    retention: "bound_to_purpose_scope",
    marketing: false, merchandise: false, model_training: false,
  };
}

const STYLE = `
#wizard-panel { position:fixed; inset:0; z-index:21; display:grid; place-items:center;
  padding:20px; background:rgba(18,28,45,.48); backdrop-filter:blur(12px); }
#wizard-panel[hidden] { display:none; }
#wizard-card { box-sizing:border-box; width:100%; max-width:480px; max-height:calc(100dvh - 40px);
  overflow:auto; display:flex; flex-direction:column; gap:20px; padding:28px;
  border:1px solid #ffffffa8; border-radius:28px; background:#fff; color:#182638;
  box-shadow:0 24px 80px #081b3d40; font:15px/1.5 -apple-system,BlinkMacSystemFont,"Segoe UI",Arial,sans-serif;
  scrollbar-width:thin; scrollbar-color:#ccd5df transparent; }
#wizard-card > * { flex-shrink:0; }
#wizard-brand { align-self:flex-start; display:flex; align-items:center; gap:7px;
  padding:6px 11px; border:1px solid #e1e7ee; border-radius:99px; background:#f6f8fb;
  color:#33445a; font-size:15px; font-weight:650; line-height:1; letter-spacing:-.03em; }
#wizard-brand svg { width:19px; height:19px; }
#wizard-card h2 { margin:0; font-size:29px; font-weight:700; line-height:1.15; letter-spacing:-.045em; }
#wizard-bargain { margin:0; color:#526174; text-wrap:pretty; }
#wizard-card fieldset { border:0; margin:0; padding:0; display:grid; gap:12px; }
#wizard-card legend { position:absolute; width:1px; height:1px; overflow:hidden; clip-path:inset(50%); }
#wizard-card label.row { display:flex; gap:12px; align-items:center; padding:16px;
  border:1px solid #d7e3f3; border-radius:16px; cursor:pointer; font-size:16px; font-weight:600; background:#f4f8fe; }
#wizard-card label.row input { width:20px; height:20px; margin:0; accent-color:#0866ff; }
#wizard-upload { min-width:0; }
#wizard-upload:empty { display:none; }
#wizard-upload p { margin:0 0 12px; color:#526174; }
#wizard-upload input[type=file] { width:100%; font:inherit; font-size:13px; color:#526174;
  padding:10px; border:1px dashed #b7c9e2; border-radius:14px; background:#f4f8fe; }
#wizard-upload input::file-selector-button { border:0; border-radius:9px; padding:10px 13px;
  margin-right:10px; background:#fff; color:#0866dd; font-weight:600; cursor:pointer; }
#wizard-note { margin:0; font-size:14px; }
#wizard-note:empty { display:none; }
#wizard-note.trouble { color:#a42135; background:#fff0f2; padding:12px 14px; border-radius:12px; }
#wizard-note.settled { color:#34634e; }
#wizard-proof { color:#627184; font-size:12px; }
#wizard-proof:has(#wizard-receipt:empty) { display:none; }
#wizard-proof summary { cursor:pointer; }
#wizard-receipt { margin:8px 0 0; padding:10px; background:#f4f6f9; border-radius:10px;
  font:11px/1.5 ui-monospace,monospace; overflow-wrap:anywhere; }
#wizard-actions { display:flex; gap:10px; }
#wizard-panel button { flex:1; min-height:48px; padding:12px 16px; border:1px solid transparent;
  border-radius:14px; font:600 15px/1.25 -apple-system,BlinkMacSystemFont,"Segoe UI",Arial,sans-serif;
  background:#0866ff; color:#fff; cursor:pointer; transition:background .15s,transform .15s; }
#wizard-panel button:hover { background:#0059e0; }
#wizard-panel button:active { transform:scale(.98); }
#wizard-panel button:focus-visible, #wizard-panel input:focus-visible, #wizard-proof summary:focus-visible {
  outline:3px solid #88b7ff; outline-offset:3px; }
#wizard-panel button[disabled] { opacity:.45; cursor:default; }
#wizard-panel #wizard-back { background:#edf1f6; color:#33445a; }
#wizard-panel #wizard-back:hover { background:#e2e8f0; }
#wizard-panel #wizard-withdraw { flex:none; min-height:36px; padding:8px; background:transparent;
  color:#9a3346; font-size:13px; font-weight:500; }
#wizard-panel #wizard-withdraw:hover { background:#fff1f3; }
#wizard-panel.working #wizard-card { cursor:progress; }
#wizard-panel.working #wizard-go { background:#5a91e6; }
@media(max-width:480px) {
  #wizard-panel { padding:12px; }
  #wizard-card { padding:24px; gap:18px; max-height:calc(100dvh - 24px); border-radius:24px; }
  #wizard-card h2 { font-size:26px; }
}
@media(prefers-reduced-motion:reduce) { #wizard-panel button { transition:none; } }
`;

export default function mountWizard({ sfx = () => {}, bearer = async () => null, enterPractice = () => {} } = {}) {
  const style = document.createElement("style");
  style.textContent = STYLE;
  document.head.append(style);

  const panel = document.createElement("div");
  panel.id = "wizard-panel";
  panel.hidden = true;
  panel.innerHTML = `
    <form id="wizard-card" role="dialog" aria-modal="true" aria-labelledby="wizard-title" novalidate>
      <div id="wizard-brand" aria-label="REGARDE"><svg viewBox="0 0 240 240" aria-hidden="true"><polygon points="103,29.82 33.4,70 33.4,170 103,210.18" fill="currentColor"/><polygon points="137,29.82 206.6,70 206.6,170 137,210.18" fill="currentColor"/><circle cx="120" cy="120" r="15" fill="currentColor"/></svg>regarde</div>
      <h2 id="wizard-title">Add yourself</h2>
      <p id="wizard-bargain">Make a fighter from your own photo for private preview and local practice. OpenAI chooses its colors, hair and accessories. Accept it to save it to your AC handle for 24 hours while your permission stays active. Withdraw anytime to remove your material and stop future use.</p>
      <div id="wizard-sections"></div>
      <p id="wizard-note" role="status" aria-live="polite"></p>
      <details id="wizard-proof"><summary>Consent receipt</summary><p id="wizard-receipt"></p></details>
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
  let submitted = null;
  let candidate = null;
  let accepted = null;
  let ownerHandle = null;
  let requestId = null;
  let reviewSession = 0;
  const clearFighter = () => { accepted = null; globalThis.__oskiewarFighterAppearance = null; };
  const upload = document.createElement("div");
  upload.id = "wizard-upload";
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

  async function open({ useSaved = false } = {}) {
    if (!panel.hidden) return;
    if (busy) { panel.hidden = false; globalThis.__oskiewarWizardOpen = true; return; }
    reviewSession++;
    requestId = crypto.randomUUID();
    capability = null;
    submitted = null;
    candidate = null;
    upload.replaceChildren();
    sections.hidden = false;
    panel.querySelector("#wizard-bargain").hidden = false;
    panel.querySelector("#wizard-title").textContent = "Add yourself";
    go.textContent = "Allow";
    back.textContent = "Not now";
    go.disabled = false;
    panel.hidden = false;
    globalThis.__oskiewarWizardOpen = true;
    say("");
    receiptLine.textContent = "";
    sfx("block", .9, 0);
    card.querySelector("input")?.focus();
    const session = reviewSession;
    working(true);
    try {
      const token = await bearer();
      if (session !== reviewSession) return;
      if (!token) { say("Sign in with your AC account to make your fighter."); return; }
      const result = await generationRequest(token, "account", {});
      if (session !== reviewSession) return;
      ownerHandle = result.handle;
      panel.querySelector("#wizard-title").textContent = `Add ${ownerHandle}`;
      if (result.status === "accepted") showCandidate(result, true);
    } catch (error) { say(error.message, "trouble"); }
    finally { if (session === reviewSession) working(false); }
    if (useSaved && session === reviewSession && candidate?.saved) card.requestSubmit();
  }

  function close() {
    if (panel.hidden) return;
    if (panel.contains(document.activeElement)) document.activeElement.blur();
    panel.hidden = true;
    globalThis.__oskiewarWizardOpen = false;
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
    if (candidate) {
      if (Date.now() >= candidate.validUntil) { say("This preview expired. Open Add yourself to ask again.", "trouble"); return; }
      working(true);
      try {
        const result = await generationRequest(token, candidate.saved ? "account" : "accept", candidate.source);
        if (result.status !== "accepted" || result.fighter?.hash !== candidate.fighter.hash)
          throw new Error("This fighter is no longer available. Open Add yourself to check your account.");
        accepted = { ...result, token };
        globalThis.__oskiewarFighterAppearance = { appearance: candidate.appearance,
          handle: result.handle, validUntil: result.validUntil };
        say(`Saved to ${result.handle}. Ready for local practice.`, "settled");
        working(false); go.disabled = true; back.textContent = "Done";
        enterPractice();
      } catch (error) { clearFighter(); working(false); say(error.message, "trouble"); }
      return;
    }
    if (submitted) { await generate(token); return; }
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
        const photo = result.manifest?.find(item => item.source === "appearance");
        if (!photo) {
          working(false); go.disabled = true;
          say("Voice stored. Voice generation is not available yet.", "settled");
          return;
        }
        submitted = { hash: photo.hash.replace(/^sha256:/, ""), capability: capability.jws };
        await generate(token);
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
      // Ownership comes from the authenticated server lookup, never typed input.
      const account = await generationRequest(token, "account", {});
      ownerHandle = account.handle;
      const response = await fetch(ENDPOINT, {
        method: "POST",
        headers: { "Content-Type": "application/json",
          authorization: "Bearer " + token },
        body: JSON.stringify({ ...answer, requestId }),
      });
      result = await response.json();
    } catch (error) {
      working(false);
      say(error.message || "The consent desk did not answer. Nothing was collected.", "trouble");
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
        try {
          const prior = await fetch("/api/oskiewar-submission", {
            method: "POST", headers: { "Content-Type": "application/json", authorization: "Bearer " + token },
            body: JSON.stringify({ action: "status", capability: capability.jws }), signal: AbortSignal.timeout(10000),
          });
          if (prior.ok) {
            const photo = (await prior.json()).manifest?.find(item => item.source === "appearance");
            if (photo) {
              submitted = { hash: photo.hash.replace(/^sha256:/, ""), capability: capability.jws };
              go.textContent = "Make fighter";
              say("Your stored photo is ready.", "settled");
              return;
            }
          }
        } catch { /* Upload remains available if there is no recoverable manifest. */ }
      } else {
        say("The desk returned no upload permission. Nothing was collected.", "trouble");
        return;
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


  async function generationRequest(token, action, source = submitted) {
    const response = await fetch("/api/oskiewar-generation", {
      method: "POST", headers: { "Content-Type": "application/json", authorization: "Bearer " + token },
      body: JSON.stringify({ ...source, action }), signal: AbortSignal.timeout(100000),
    });
    const result = await response.json();
    if (!response.ok) {
      if (result.code === "handle_required") {
        close(); globalThis.__oskiewarAccountDoor = "handle";
      }
      throw new Error(result.message || "Fighter generation failed.");
    }
    return result;
  }
  function showCandidate(result, saved = false) {
    sections.hidden = true;
    panel.querySelector("#wizard-bargain").hidden = true;
    panel.querySelector("#wizard-title").textContent = `${result.handle || ownerHandle}'s fighter`;
    const appearance = mountFighterPreview(upload, result.fighter);
    candidate = { ...result, appearance, saved, source: { ...submitted } };
    go.textContent = saved ? "Use in practice" : "Accept & use in practice";
    say(saved ? "Your saved fighter is ready for local practice." : "Review your fighter. Accept to save it to your AC account for 24 hours.", "settled");
  }
  async function generate(token) {
    const session = reviewSession;
    working(true); say("Making your fighter…");
    try {
      const result = await generationRequest(token, "generate");
      if (session !== reviewSession) return;
      if (result.status === "running") {
        working(false); go.textContent = "Check generation";
        say("Your fighter is being made. Check again in a moment."); return;
      }
      showCandidate(result);
      working(false);
    } catch (error) {
      working(false); go.textContent = "Check generation";
      say(error.message, "trouble");
    }
  }
  // Keep the active play selection only in this page's memory. Current authority is
  // checked while equipped; expiry, sign-out, withdrawal or loss of contact
  // removes it. There is no public asset URL, localStorage copy or replay data.
  const withdraw = document.createElement("button");
  withdraw.type = "button";
  withdraw.textContent = "Withdraw my material";
  withdraw.id = "wizard-withdraw";
  card.append(withdraw);
  withdraw.addEventListener("click", async () => {
    if (busy) return;
    const token = await bearer();
    if (!token) { say("Sign in to withdraw your material.", "trouble"); return; }
    working(true);
    try {
      await generationRequest(token, "withdraw", {});
      clearFighter(); candidate = null; submitted = null; capability = null;
      upload.replaceChildren(); working(false); go.disabled = true;
      say("Withdrawn. Your stored material is removed and future use is blocked.", "settled");
    } catch (error) { working(false); say(error.message, "trouble"); }
  });
  let checking = false;
  setInterval(async () => {
    if (!accepted || checking) return;
    const selected = accepted;
    if (Date.now() >= selected.validUntil) { clearFighter(); return; }
    checking = true;
    try {
      const token = await bearer();
      if (!token) { clearFighter(); return; }
      const result = await generationRequest(token, "account", {});
      if (accepted !== selected) return;
      if (result.status !== "accepted" || result.fighter?.hash !== selected.fighter.hash) { clearFighter(); return; }
      accepted = { ...result, token };
      globalThis.__oskiewarFighterAppearance = { appearance: validateFighter(result.fighter),
        handle: result.handle, validUntil: result.validUntil };
    } catch { if (accepted === selected) clearFighter(); }
    finally { checking = false; }
  }, 15000);
  addEventListener("pagehide", clearFighter);

  return { open, close, get isOpen() { return !panel.hidden; } };
}
