import { freshSession } from "../../../shared/ac-token.mjs";

// The same metered endpoint Aesel uses, with no file or publishing tools.
// An explicit session and server-verified handle bill the selected AC account.
// The Tezos signing key is not used for inference.
//
// The daily was written by `claude -p --model sonnet` until the Claude Code
// subscription stopped serving the appliance (2026-10-16); the same register
// comes from Sonnet through AC's own OpenRouter account, paid in braincells.
export const DAILY_BRAIN_MODEL = "anthropic/claude-sonnet-5.5";

export async function braincellText(prompt, {
  file, handle, model = DAILY_BRAIN_MODEL, fetch = globalThis.fetch, session = freshSession,
  timeoutMs = 180_000,
} = {}) {
  if (!file || !handle) throw new Error("Set DAILY_BRAIN_TOKEN_FILE and DAILY_BRAIN_HANDLE for the daily's AC account");
  const expected = handle.replace(/^@/, "");
  const { access_token: token } = await session({ file });
  if (!token) throw new Error("The daily's AC session has no access token");
  const headers = { Authorization: `Bearer ${token}`, "Content-Type": "application/json" };
  const base = "https://aesthetic.computer/api";
  const balance = await fetch(`${base}/easel-credits`, { headers, signal: AbortSignal.timeout(15_000) });
  if (!balance.ok) throw new Error(`Cannot verify the daily's braincells (HTTP ${balance.status})`);
  const credits = await balance.json();
  if (credits.handle !== `@${expected}`) throw new Error(`Daily account mismatch: expected @${expected}`);
  if (![credits.remaining, credits.purchased].every(n => Number.isFinite(n) && n >= 0)) throw new Error("Daily braincell balance is unavailable");
  if (credits.remaining + credits.purchased <= 0) throw new Error(`@${expected} has no braincells available`);

  const response = await fetch(`${base}/easel-inference`, {
    method: "POST", headers, signal: AbortSignal.timeout(timeoutMs),
    // No `thinking` field: Sonnet via OpenRouter answers {type:"disabled"}
    // with a 400 (measured 2026-10-18), and the default is already no thinking.
    body: JSON.stringify({ model, messages: [{ role: "user", content: prompt }], max_tokens: 2048 }),
  });
  if (!response.ok) {
    const detail = await response.text().catch(() => "");
    throw new Error(`Daily braincell inference failed (HTTP ${response.status})${detail ? `: ${detail.slice(0, 200)}` : ""}`);
  }
  if (!response.body) throw new Error("Daily braincell inference returned no stream");
  const reader = response.body.getReader(), decoder = new TextDecoder();
  let pending = "", text = "", stopped = false, reason = "", bytes = 0;
  const line = value => {
    if (!value.startsWith("data:")) return;
    const payload = value.slice(5).trim();
    if (payload === "[DONE]") return;
    const event = JSON.parse(payload);
    if (event.type === "error") throw new Error("Daily braincell inference reported a stream error");
    if (event.type === "content_block_start" && event.content_block?.type === "tool_use") throw new Error("Daily writer unexpectedly requested a tool");
    if (event.type === "content_block_start" && event.content_block?.type === "text") text += event.content_block.text || "";
    if (event.type === "content_block_delta" && event.delta?.type === "text_delta") text += event.delta.text;
    if (event.type === "message_delta") reason = event.delta?.stop_reason || reason;
    if (event.type === "message_stop") stopped = true;
  };
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      bytes += value.byteLength;
      if (bytes > 1_048_576) throw new Error("Daily writer exceeded its response limit");
      pending += decoder.decode(value, { stream: true });
      const lines = pending.split("\n");
      pending = lines.pop();
      for (const value of lines) line(value.trimEnd());
    }
    pending += decoder.decode();
    if (pending.trim()) line(pending.trimEnd());
    if (!stopped || reason !== "end_turn" || !text.trim()) throw new Error("Daily writer returned an incomplete script; nothing saved");
    return text.trim();
  } finally {
    await reader.cancel().catch(() => {});
    reader.releaseLock();
  }
}
