#!/usr/bin/env node
// slab/bin/inbox-drain.mjs
// Hook-side drain for the prox inbox. Other agents drop messages in
// $SLAB_HOME/inbox/<session_id>/messages.jsonl; this hands them to the running
// session at its turn boundaries. No keystrokes.
//
//   node inbox-drain.mjs prompt [codex]   # UserPromptSubmit → additionalContext
//   node inbox-drain.mjs stop   [codex]   # Stop → decision:block, turn continues
//
// Always exits 0. A broken drain must never cost the user a prompt.

const HEADER = "Messages from other sessions (via prox inbox):";

// Hook payload arrives on stdin; don't wait forever if it never comes.
function readStdin() {
  return new Promise((resolve) => {
    let data = "";
    let settled = false;
    const done = (v) => {
      if (settled) return;
      settled = true;
      clearTimeout(fallback);
      resolve(v);
    };
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (chunk) => (data += chunk));
    process.stdin.on("end", () => {
      try {
        done(JSON.parse(data));
      } catch {
        done({});
      }
    });
    process.stdin.on("error", () => done({}));
    const fallback = setTimeout(() => done({}), 500);
  });
}

const out = (obj) => process.stdout.write(JSON.stringify(obj) + "\n");
const letter = (msgs, stamp) => [HEADER, ...msgs.map(stamp)].join("\n");

async function main() {
  const [mode, harness] = process.argv.slice(2);
  if (mode !== "prompt" && mode !== "stop") {
    process.stderr.write("inbox-drain: usage: inbox-drain.mjs prompt|stop [codex]\n");
    return;
  }
  const payload = await readStdin();
  // Codex's Stop wants JSON even when there's nothing to say; Claude is fine
  // with silence and treats `{}` as "no decision" too.
  const quiet = () => {
    if (harness === "codex" && mode === "stop") out({});
  };
  const sid = payload.session_id;
  if (!sid) return quiet();

  // Lazy: prox-inbox.mjs is landing in parallel; a missing module is a
  // stderr line, not a failed hook.
  const { drain, peek, stamp } = await import("./prox-inbox.mjs");

  if (mode === "prompt") {
    const msgs = await drain(sid);
    if (!msgs.length) return;
    out({
      hookSpecificOutput: {
        hookEventName: "UserPromptSubmit",
        additionalContext: letter(msgs, stamp),
      },
    });
    return;
  }

  // stop: peek before consuming so an empty inbox costs nothing. This is also
  // the loop guard: when stop_hook_active is set we're inside a continuation
  // this hook caused, and the message that caused it is already consumed, so
  // the inbox reads empty and the turn ends. Fresh messages still go through;
  // the harness's 8-block cap is the backstop for a chatty peer.
  const pending = await peek(sid);
  if (!pending.length) return quiet();
  const msgs = await drain(sid);
  if (!msgs.length) return quiet();
  out({ decision: "block", reason: letter(msgs, stamp) });
}

main()
  .catch((e) => process.stderr.write(`inbox-drain: ${e.message}\n`))
  .finally(() => {
    // exit code 0 and let the loop drain, so stdout flushes. A parent that
    // holds stdin open must not keep us alive past the fallback.
    process.exitCode = 0;
    process.stdin.destroy();
    process.stdin.unref?.();
  });
