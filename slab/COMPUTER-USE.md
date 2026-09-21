# Computer use across clients

Frame and Puppet are a shared computer-use service. Codex, Claude, Aesel, and
shell scripts are clients. The client chooses the model and obtains any required
authorization; the service owns capture, targeting, input, and execution results.
Changing the model must not change coordinate conventions or retry an action.

## Where each route fits

| Client or backend | Useful capability | Boundary |
| --- | --- | --- |
| Codex desktop built-in browser | Integrated page navigation, input, screenshots, and verification | Its documented UI workflow is a comparison point, not an API dependency or proof of its internal implementation |
| Playwright | Semantic locators, actionability checks, condition-based waits | Browser DOM automation; a useful pattern for Puppet's next targeting layer |
| Puppet | Warm CDP connections, browser screenshots/input, native keyboard/typing | Browser pixels are viewport CSS pixels; native input affects the desktop |
| Frame | Native windows, OCR, Accessibility, changed regions, staged clicks | Screen coordinates are global macOS points; native capture lives in SlabMenubar |
| Codex CLI / app / IDE | MCP over HTTP or stdio | Tools still follow each client's permissions and available capabilities |
| Claude CLI | MCP over HTTP or stdio | Claude configuration and approvals are independent of Codex's |
| Aesel | A trusted host adapter can call the same services without a vendor CLI | Tool selection, target binding, image display/model routing, and authorization belong to the host |

OpenAI documents the built-in browser's observe/act/verify capabilities; it does
not establish that its internal implementation is Playwright. Playwright's
published actionability checks are the reference for the behavior we want:
resolve the intended element, wait until usable, act once, and check the outcome.

## Available now

- Shared loopback MCP services: Frame on `127.0.0.1:7767/mcp`, Puppet on
  `127.0.0.1:7769/mcp`. Both retain stdio entry points.
- Native Frame transport is imported directly by MCP. The CLI still writes a
  requested image; ordinary Mac MCP captures avoid the extra Node process and
  temporary image round-trip. Xbox's blocking backend remains in a child.
- Puppet's in-memory browser image cache expires after 250 ms and invalidates
  on browser input, navigation, and evaluation. `fresh: true` forces capture.
  Age alone is not proof that a particular action has finished rendering.
- Local AppleScript and clipboard commands bypass login-shell startup. MCP
  native operations run asynchronously, with complete operations queued per
  machine, including clipboard-plus-paste. A filesystem lease now coordinates
  Frame, Puppet, and CLI input across processes on the same controller host.
  Busy requests fail before dispatch after a bounded wait; leases are never
  automatically stolen after a crash. `doctor` reports outstanding owners.
- Frame carries an observation session through MCP and into the native diff
  store. Each session has its own baseline, staged-click record, and action
  trail. The native store retains four image baselines; eviction reports a
  missing baseline and re-establishes it rather than comparing another session.
- Captures identify the observation, time, native window, coordinate space, and
  scale. Native `frame_click`/`frame_key` recheck the last window-scoped frame's
  window ID, owning process, and bounds under the lease before input; an
  optional `observationId` also rejects a superseded observation. Staged commits
  require the native overlay's approval ID still to match.
- `puppet_snapshot`, `puppet_click`, `puppet_fill`, and `puppet_wait` use strict
  Playwright locators and exact page IDs from `puppet_list.pages`. Playwright is
  attached lazily over CDP with `noDefaults`, preserving browser preferences.
  A postcondition timeout reports `performed: true` and failed verification;
  an ambiguous dispatch failure reports `performed: "unknown"`. Neither is an
  invitation to repeat input. Read-only waits do not block actions.
- MCP initialization supplies the same workflow guidance to any client.
- `puppet_choose` reads the exact page's named accessible controls and asks Jev
  to select one for a supplied goal. It returns a strict locator without input;
  use `puppet_click` with a postcondition after checking the suggestion. Known
  locators stay direct. Only the goal and at most 40 control labels/roles go to
  OpenRouter; URLs, IDs, screenshots, field values, and locators stay local.
  Stale/changed controls, ambiguous labels, unknown previous input, unavailable
  decisions, and low confidence cause an observe/wait fallback. This is an
  explicit tool call, not an automatic extra step on every click.
- `lib/computer-use-client.mjs` provides a fetch-only client for these stateless
  HTTP services. It discovers schemas, preserves image/text/error blocks, uses
  explicit tool allowlists, bounds requests, and never retries lost actions.
  It is not a general MCP SDK: SSE, OAuth, sessionful servers, and stdio clients
  are outside this helper's scope.
- `easel/src/computer-use.mjs` adapts that client to a host-selected machine and
  browser target, removing those choices from model-facing schemas and rejecting
  argument attempts to override them. No tools are enabled by default.

### Session and browser examples

For HTTP Frame calls, reuse the `sessionId` returned by the initial `frame` in
`frame_reframe`, `frame_click`, `frame_key`, and staged-action followups. The
fetch client sends a stable session header automatically; stdio has a private
session per frontend process. Native peers need the updated SlabMenubar; older
peers fail explicitly instead of falling back to a shared baseline. The CLI
accepts `frame local --session my-session --json` for its own native baseline.

Get exact browser page IDs from `puppet list`, then:

```sh
node slab/bin/puppet.mjs snapshot local --target=PAGE_ID
node slab/bin/puppet.mjs click local '{"role":"button","name":"Save"}' --target=PAGE_ID --after='{"locator":{"text":"Saved"}}'
node slab/bin/puppet.mjs fill local '{"label":"Title"}' 'A new title' --target=PAGE_ID
node slab/bin/puppet.mjs wait local '{"text":"Ready"}' --target=PAGE_ID --timeout=5000
```

The MCP equivalents use the same names with a `puppet_` prefix and explicit
`machine`, `target`, and `locator` arguments. Role names, labels, and text match
exactly; multiple matches fail. Existing pixel/CDP tools remain available.

Jev selection through MCP uses `puppet_choose` with `{machine, target, goal}`.
Credentials are loaded on demand from `OPENROUTER_API_KEY` or the existing
`~/.config/aesthetic-computer/jev.env`. Captutor's `bin/jev-frame.mjs` uses the
same credential source. Neither route executes Jev's suggestion automatically.

Wordplay is a playable, randomized browser exercise for this route:

```sh
node slab/wordplay/serve.mjs
# Open http://127.0.0.1:7781
node --env-file="$HOME/.config/aesthetic-computer/jev.env" slab/bin/computer-use-smoke.mjs --wordplay
```

The test uses a disposable profile and Puppet daemon. It reads visible clues
through snapshots, asks `puppet_choose`, then clicks and verifies feedback.
It never reads the game's answer key. The first eight-round run scored 8/8:
median choose time 264 ms, answer click plus verification 30.5 ms, combined
292 ms. This small browser task does not measure native macOS use or prove a
speedup over another model. Raw round results are in `wordplay/benchmark.json`.

Read-only service check:

```sh
node slab/bin/computer-use.mjs doctor
```

This checks protocol discovery and guidance, not Screen Recording permission,
CDP page availability, or the accuracy of a model's actions. Use `frame doctor`
and `puppet list` for those backend checks.

### Client configuration

Prefer shared HTTP services on this resource-constrained host. Stdio remains a
compatibility route but launches a separate frontend per client session.

Codex configuration:

```toml
[mcp_servers.frame]
url = "http://127.0.0.1:7767/mcp"

[mcp_servers.puppet]
url = "http://127.0.0.1:7769/mcp"
```

Claude project MCP configuration:

```json
{
  "mcpServers": {
    "frame": { "type": "http", "url": "http://127.0.0.1:7767/mcp" },
    "puppet": { "type": "http", "url": "http://127.0.0.1:7769/mcp" }
  }
}
```

The existing `toolchain/mcp/install-daemons.sh` registers services for both
clients. Do not re-run the whole installer merely to check these two tools.

### Aesel integration

The adapter is ready for a trusted host to import; this change does **not**
wire computer use into Aesel's shipped UI or hosted-inference loop.

```js
import { createAeselComputerUse } from "./computer-use.mjs";

const computer = createAeselComputerUse({
  machine: "local",
  target: selectedPageId,
  allowedTools: ["puppet_shot"],
});
const { tools } = await computer.discover();
const result = await computer.call("puppet_shot", { fresh: true });
// Host renders result.content's image blocks or deliberately attaches them
// to a vision-capable model request. Do not stringify images as tool prose.
```

The current engine paths differ:

- `easel/src/app-server.mjs` launches Codex app-server. Actual MCP availability
  depends on that host's configuration and runtime policies.
- `easel/src/claude-server.mjs` uses `--strict-mcp-config` and admits Aesel's
  own `ac` tools. Frame/Puppet are **not inherited** from ordinary Claude config.
  Integrating them needs an explicit host-selected tool configuration; do not
  remove strict mode or silently add them to its preapproved read-only tools.
- `easel/src/ac-server.mjs` currently offers `write_piece` only. Its model loop
  and hosted endpoint must explicitly support additional tool schemas and image
  results before this adapter is connected to hosted inference.
- An iPhone's loopback address is the phone, not a fleet Mac. Remote computer use
  needs a paired, authenticated host bridge; this change exposes no network
  control endpoint. Generated pieces must not receive that bridge or credentials.

## Remaining boundaries and next work

1. **Coordinate beyond one controller.** Leases cover cooperating processes on
   this controller host. They cannot stop a human pointer, another machine's
   independent controller, arbitrary AppleScript, or external CDP clients.
   Native focus can still change after the pre-input check. Move arbitration to
   the target host for fleet-wide ownership, and design human handoff there.
2. **Replace native fixed waits.** Browser actions now wait for actionability
   and optional locator postconditions. Native hover/click/key still use their
   existing settling delays. Add bounded visual/AX conditions without treating
   "no visible change" as proof of failure.
3. **Keep expensive analysis optional.** No-OCR captures still traverse
   Accessibility and run contour detection. Add a dedicated cheap diff probe,
   window-scoped AX traversal, analysis budgets, and explicit partial results.
4. **Correlate the native transport.** File polling remains underneath Frame.
   A target-host socket should carry request IDs, acknowledgements, cancellation,
   and lease ownership. Late native responses after timeouts still need a full
   request-correlation protocol. Never retry an uncertain action automatically.
5. **Broaden browser fixtures.** Semantic selectors currently address the main
   document. Add explicit iframe selection, navigation and download outcomes,
   moving controls, and long-running app fixtures. CDP attachment is Chromium
   only. Playwright's documented CDP caveats still apply.
6. **Integrate Aesel's product paths.** The adapter now keeps the observation
   session fixed alongside machine/page, but the UI, Claude strict config, and
   hosted model loop still need the explicit integration described above.

Frame's native `timings_ms.wall` now measures elapsed time, including contour
processing and the residual AX wait. Keep controller end-to-end time separate.

## Evidence required before claiming parity

Use one deterministic fixture with a delayed button, moving target, modal,
text field, scroll region, and canvas. Run the same tasks through HTTP, stdio,
Codex, Claude, and the Aesel adapter. Measure cold/warm p50/p95 time,
round trips, image bytes, host CPU, wrong-target actions, stale observations,
and postcondition success. Include two simultaneous clients and a lost reply.
Do not equate a passing protocol test with end-to-end model task success.

Focused checks:

```sh
node --test slab/test/computer-use-*.test.mjs slab/test/macos-automation.test.mjs slab/test/frame-hover-atlas.test.mjs
```

Current tests cover transport framing, image freshness, no action replay after a
lost response, session isolation, cross-process lease exclusion, native target
checks, asynchronous paste ordering, allowlists, Aesel target binding, and
identical HTTP/stdio discovery and tool errors. An isolated headless Chrome
fixture exercises delayed buttons, labels, ambiguous selectors, concurrent
wait/action clients, failed postconditions, closed tabs, and actual CLI/MCP
semantic dispatch through a fixture daemon. Live native checks interleave two
sessions with different crops and verify stale staged-target rejection.

Full model task parity, human/remote-controller arbitration, and Aesel product
integration remain unverified. Protocol and fixture tests do not establish them.

## References

- [OpenAI browser workflow](https://learn.chatgpt.com/docs/browser#computer-use-in-the-browser)
- [Codex MCP support and server instructions](https://developers.openai.com/codex/mcp)
- [Claude Code MCP configuration](https://code.claude.com/docs/en/mcp)
- [Playwright actionability](https://playwright.dev/docs/actionability)
- [Apple stream dirty rectangles](https://developer.apple.com/documentation/screencapturekit/scstreamframeinfo/dirtyrects)
- [Apple rectangle screenshots](https://developer.apple.com/documentation/screencapturekit/scscreenshotmanager/captureimage(in:completionhandler:))
