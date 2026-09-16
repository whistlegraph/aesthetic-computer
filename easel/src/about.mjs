export function aboutMap() {
  return [
    "AESEL — make a piece by talking to it",
    "",
    "You → model → working piece → live preview → URL / QR",
    "       │          │",
    "       │          └─ v1 → v2 → v3 · /versions · /rollback vN",
    "       ├─ AC hosted · /backend ac · /model",
    "       ├─ Your Claude account · /backend claude",
    "       └─ Your Codex account  · /backend codex",
    "",
    "MAKE    /piece · /runtime · /ask on|off",
    "MEASURE /performance · headless logic · /energy · estimated electricity",
    "SHARE   /publish · /autopublish on|off · /open · /qr",
    "ACCOUNT /login · /profile · /logout",
    "THREAD  /model NAME · /backend NAME · /new",
    "",
    "Switch engines with recent conversation and the current piece.",
    "Versions are saved on this computer; rollback makes a new version.",
    "AC hosted uses your handle's daily budget. Claude/Codex use your own CLI sign-in.",
    "",
    "Esc returns · ↑/↓ scroll · /mouse off restores terminal selection",
  ];
}

// Recent conversation is portable even when provider thread IDs are not.
export function conversationHandoff(entries, limit = 24000) {
  const turns = entries.filter(({ kind }) => kind === "user" || kind === "assistant")
    .map(({ kind, text }) => JSON.stringify({ role: kind, content: text }));
  const kept = [];
  let size = 0;
  for (const turn of turns.reverse()) {
    const part = turn.length > limit ? turn.slice(0, limit) : turn;
    if (size + part.length > limit) break;
    kept.unshift(part);
    size += part.length;
  }
  return kept.length ? "Conversation before the engine switch (reference context):\n" + kept.join("\n") : "";
}
