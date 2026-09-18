# Jev in Oskiewar

Node 22+; no npm dependencies. This MCP server wraps the existing Oskiewar coach and
workshop tools and adds `coach_jev`. Jev chooses a practice focus from the
coach's accumulated fight statistics; the cue is authored locally.

On each machine, save a [Vercel AI Gateway key](https://vercel.com/ai-gateway/models/jev)
in `~/.config/aesthetic-computer/jev.env`, outside the repository:

```dotenv
AI_GATEWAY_API_KEY=your-key
```

Protect the file and register the coach in installed Codex and Claude clients:

```sh
chmod 600 ~/.config/aesthetic-computer/jev.env
bash toolchain/jev/install.sh
bash toolchain/jev/run.sh --test
bash toolchain/jev/run.sh --smoke
```

Use `install.sh --codex` or `--claude` to register only one client. The installer
updates the `oskiewar-coach` user-level entry and leaves other server entries
alone. Reload MCP connections or start a new client session after installing.

The launcher resolves its own checkout, uses the host's stable fnm Node alias
(then `node` on PATH), and reads the private env file. It works from any cwd
on Blueberry or another host, without a hardcoded username or Node version.
Override `JEV_NODE` or `JEV_ENV_FILE` when needed. An exported
`AI_GATEWAY_API_KEY` takes precedence over the file. Credentials are never
stored in MCP configuration or git.

Keep the installed checkout in place. To move it, rerun the installer from the
new location. For a dirty main checkout, use a separate worktree. The smoke
command performs one paid request using synthetic fight statistics.

For other MCP clients, register `/bin/bash` with the absolute path to `run.sh`
as its argument. In the coach session:

1. `coach_in` with the room printed on Oskiewar's title screen.
2. `coach_watch` while playing several exchanges.
3. `coach_jev` with `seat: 0` (or `1`) for a practice focus and probabilities.
4. `coach_workshop` with `op: "inspect"` to inspect the room before designing
   a drill. Use the returned revision for any subsequent workshop edit.

The wrapper preserves the existing workshop tools. Jev itself issues no edits,
controller inputs, saves or publications. Its evidence is a session aggregate,
so its advice is for the next drill, not frame-by-frame combat. Unknown choices
are rejected; probabilities are model estimates, not validated coaching accuracy.
Only aggregate gameplay statistics are sent by `coach_jev`, excluding player
names, room IDs, chats and images. Requests select zero data retention.

The reusable adapter accepts `{ state, questions }` from other Node tools,
including a future Aesel decision step:

```js
import { evaluate } from './toolchain/jev/evaluate.mjs';
const result = await evaluate({
  state: 'The build exited with code 1.',
  questions: { passed: { type: 'boolean', instructions: 'Did the build pass?' } },
});
console.log(result.answers);
```

It also accepts that JSON on stdin:

```sh
node toolchain/jev/evaluate.mjs < request.json
node --test toolchain/jev/jev.test.mjs xbox/live/tests/coach.test.mjs
```

The HTTP adapter follows the public
[Gateway evaluation implementation](https://github.com/vercel/ai/blob/main/packages/gateway/src/gateway-evaluation-model.ts)
and [provider authentication](https://github.com/vercel/ai/blob/main/packages/gateway/src/gateway-provider.ts).
This is an experimental SDK protocol, not an OpenAI-compatible chat endpoint;
it may change. Using built-in `fetch` avoids a new dependency while the SDK is
inside this repository's seven-day package cooldown.

Local tests mock the gateway and cover request shape, response validation,
evidence filtering and coach delegation. The adapter has also completed a real
Jev choice request with zero data retention enabled; `--smoke` repeats that check.

## OpenRouter and harness benchmark

`openrouter.mjs` exports `evaluateChoices(request)` with the same `{state,
questions}` choice input. Set `OPENROUTER_API_KEY`. It uses
`POST /api/alpha/decisions`, model `~typesafe/jev-latest`, and `provider.zdr: true`.
This alpha endpoint is documented in OpenRouter's
[Decisions SDK implementation](https://github.com/OpenRouterTeam/go-sdk/blob/main/decisions.go).
It does not use chat completions. The existing coach still defaults to Vercel.

Run the paid, synthetic benchmark with credentials exported or Node env files:

```sh
node --env-file="$HOME/.config/aesthetic-computer/jev.env" \
  toolchain/jev/benchmark.mjs --provider vercel --repeats 3 --output /tmp/jev-benchmark.json
# With both API keys in the environment, omit --provider to compare routes.
```

Ten cases cover Oskiewar practice choices and proposed Aesel harness decisions:
API lookup, preview inspection, focused repair, escalation, and continuing the
existing flow. Three repetitions across both providers make 60 paid requests.
The runner sends only synthetic fixtures, alternates provider order by repetition,
and records full round-trip latency, reported cost, choices and probabilities.
The first request is reported separately; later requests reuse the process and
connection pool. Expected-label agreement is a small smoke check, not a general
accuracy or end-to-end speed claim. No live harness behavior changes.
