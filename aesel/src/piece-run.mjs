// The plain piece runner for `ac run` (docs/harness2.md, step 3): the hosted
// loop the terminal, phone and web run — AcServer on AC inference — with
// `ac check` standing in for the person's preview.
//
// A task is { id, kind: "piece", domain, requests: [...], start? }: each request
// is a turn on the piece the last one left, and `start` is the source the
// first opens on (the blank when absent). Every save is played in a browser
// for a few seconds with a tap and a space; what that finds is what ac_preview
// reports back, so the model verifies the way it would with a preview open.
// After each turn the final source is checked once more, fresh: a turn
// delivered when it changed the piece and that check found no errors.
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { tmpdir } from "node:os";
import { join } from "node:path";

const CHANNEL = "ac-run";
const sha256 = (value) => createHash("sha256").update(value).digest("hex");

// `fetch` and `play` (in place of `ac check`) are for tests.
export async function runPieceTask({ task, auth, onEvent = () => {}, log = () => {}, fetch = null, play = null }) {
  const requests = task.requests ?? (task.prompt ? [task.prompt] : []);
  if (!requests.length) throw Error("a piece task needs requests");
  const { AcServer } = await import("./ac-server.mjs");
  const { RuntimeFeedback } = await import("./runtime-feedback.mjs");
  const { RUNTIMES } = await import("./runtimes.mjs");
  const checkPiece = play || (await import("./check-piece.mjs")).checkPiece;

  const work = mkdtempSync(join(tmpdir(), "ac-run-piece-"));
  const file = join(work, "piece.mjs");
  writeFileSync(file, task.start ?? RUNTIMES.mjs.blank("piece"));
  const feedback = new RuntimeFeedback(work);
  let turn = 0, renders = 0;
  const emit = (method, params) => onEvent({ turn, method, params });

  const check = async () => {
    const renderID = ++renders;
    let problems, frame = null;
    try { ({ problems, frame = null } = await checkPiece(file, { seconds: task.seconds ?? 4, frame: true })); }
    catch (error) { problems = [`check failed: ${error.message}`]; }
    const revision = sha256(readFileSync(file));
    feedback.select({ channel: CHANNEL, revision, version: renderID, piece: "piece.mjs" });
    for (const problem of problems) feedback.log({ level: "error", text: problem, source: "ac check" });
    if (!problems.length) feedback.log({ level: "info", text: "ac check: ran with a tap and a space; no errors", source: "ac check" });
    emit("piece/check", { renderID, revision, problems, frames: frame ? [{ atMs: (task.seconds ?? 4) * 1000, width: 480, height: 360, png: frame }] : [] });
    return problems;
  };

  // The first request's system prompt is recorded once, so a reader can tell
  // which harness sections this kind of run actually sends.
  const send = fetch || globalThis.fetch.bind(globalThis);
  let recorded = false;
  const engine = new AcServer({
    cwd: work, model: task.model && task.model !== "default" ? task.model : "", rounds: task.rounds ?? 12,
    outputContinuations: task.outputContinuations ?? 0,
    jev: null, frameCapture: false,
    piece: { file, channel: CHANNEL, checkpoint: check },
    token: auth.token,
    fetch: async (url, options) => {
      if (!recorded) {
        recorded = true;
        try { emit("piece/prompt", { system: JSON.parse(options.body).system.map((block) => block.text).join("\n\n") }); } catch {}
      }
      return send(url, options);
    },
  });
  let finished = null;
  engine.on("notification", ({ method, params }) => {
    emit(method, params);
    if (method === "turn/completed") finished?.(params.turn);
  });
  engine.on("request", (request) => engine.reject(request.id, -32000, "No approvals in an unattended run"));
  engine.on("error", (error) => finished?.({ status: "failed", error: { message: String(error?.message || error) } }));

  const turns = [];
  try {
    await engine.connect();
    for (const request of requests) {
      turn += 1;
      emit("piece/turn", { request });
      const before = readFileSync(file, "utf8");
      let outcome, timer;
      try {
        outcome = await Promise.race([
          new Promise((resolve) => { finished = resolve; engine.startTurn(request).catch((error) => resolve({ status: "failed", error: { message: error.message } })); }),
          new Promise((resolve) => { timer = setTimeout(() => resolve({ status: "failed", error: { message: `timed out after ${task.timeout ?? 300} s` } }), (task.timeout ?? 300) * 1000); }),
        ]);
      } finally { clearTimeout(timer); }
      const source = readFileSync(file, "utf8");
      const changed = source !== before;
      const problems = changed ? await check() : [];
      const delivered = outcome?.status === "completed" && changed && !problems.length;
      const error = outcome?.status !== "completed" ? outcome?.error?.message || `turn ${outcome?.status}`
        : !changed ? "the piece was not written" : problems.length ? "the final check found errors" : "";
      turns.push({ turn, delivered, acceptance: delivered ? "checked" : "failed", ...(error ? { error } : {}), notes: problems, source });
      log(`turn ${turn}: ${delivered ? "delivered" : error}`);
      if (!changed) break;
    }
  } finally {
    try { engine.close(); } catch {}
    rmSync(work, { recursive: true, force: true });
  }
  // No picture reviewer here: `reviewed` is null, not a failed review.
  return { turns, delivered: turns.length === requests.length && turns.every((t) => t.delivered), reviewed: null };
}
