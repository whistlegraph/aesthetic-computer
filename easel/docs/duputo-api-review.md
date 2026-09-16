# Duputo API review

Observed September 15, 2026, read-only. The monitored turn ran 17:01:29–17:02:45 PDT. Repeated checks during the review found no later transcript activity. No input, approvals or messages were sent to the session; its piece and settings were not changed.

The main problem was unavailable tools, not an agent refusing to consult AC's APIs. Duputo tried the right tools first, and the host returned rejections. Improving prompts alone would leave that failure intact.

## Observed sequence

| Order | Calls and result |
| --- | --- |
| 1 | Discovered AC tools and read the starter piece. |
| 2 | Requested `ac_api` for `shape`, `circle` and `line` in parallel. All three returned `isError` with “user rejected MCP tool call.” |
| 3 | Listed the installed app resources and developer directory, then requested `ac_examples(circle)`. That call returned the same rejection. |
| 4 | Searched the installed source for a repository location, listed the home directory, located drawing functions in `graph.mjs`, and read the 36-line `line` implementation. |
| 5 | Applied one patch, ran `node --check`, and requested `ac_preview` plus `ac_frame`. Syntax passed; both preview calls returned the same rejection. |
| 6 | Finished with an explicit statement that visual verification remained incomplete. |

Counts: nine orchestration calls, eight shell commands, six rejected AC MCP calls, one patch. The eight shell commands comprise the starter read, four repository-location calls, two targeted runtime-source reads and one syntax check. The six fallback shell calls between the first rejection and editing took about 31 seconds. There was no repeated identical API lookup or repeated edit/test loop in this turn.

The rejected API answers were already present in the installed `context/api.json`: polygon `shape`, `circle` signature and `line` overloads. The source hunt therefore duplicated information Easel shipped. That does not establish that every desired drawing operation was fully documented.

The agent did not claim visual verification passed. It attempted both diagnostics and pixel capture, then disclosed their rejection. No frame evidence reached the model, so this review cannot establish that the resulting animation looked correct or ran without runtime errors. A syntax check only established parse validity.

## Host and context findings

**Confirmed from code:** `easel/src/tui.mjs` handles only `item/commandExecution/requestApproval` and `item/fileChange/requestApproval`. Other server requests receive JSON-RPC `-32601`. The installed app has the same limitation. `AppServer` requests user-reviewed approvals with `approvalPolicy: on-request`.

**Confirmed from the installed CLI schema:** the MCP elicitation route is `mcpServer/elicitation/request`. It is not `item/mcpToolCall/requestApproval`. Parameters require `serverName`, `threadId` and a mode-specific payload. Modes include `form`, `openai/form`, `openaiForm`, `url`, and `openai/userVerification`. Form mode includes `message` and `requestedSchema`. Its response requires `action: accept | decline | cancel`, with optional `content` and `_meta`; the command-approval `{decision: accept}` response is not this protocol.

The schema was generated locally with `codex app-server generate-json-schema --experimental`, into a temporary directory. Installed binary strings also contain `codex_approval_kind`, `mcp_tool_call`, persistence choices and the exact rejection text.

**Inference:** the unhandled elicitation route likely produced the six apparent user rejections. The rollout does not retain the actual server-to-client approval requests, so it does not prove that causal chain or that the user personally declined anything. Capture sanitized method/mode/request-ID diagnostics to confirm it. Do not describe an integration rejection as a human decision.

**A second concrete gap:** `ac_examples` searches the workspace's top-level pieces outside the monorepo. In a standalone Easel workspace it does not automatically search bundled AC reference examples. `ac_outline` and `ac_symbol` also intentionally stay within the workspace. The guidance to use them instead of reading runtime libraries is not a complete fallback for an installed app.

**Example quality:** the generated `line` examples include comments containing the word “line”, rather than three useful line calls. The builder's textual symbol matching can produce irrelevant examples. The actual `line` signature was sufficient here, but cleaner call-site selection would reduce further digging.

## Three improvements

1. **Handle MCP elicitation explicitly.** Queue parallel requests rather than replacing a single pending approval. Show the server/tool, requested action and schema-defined options; return the user's selected content using the proper `action` response. Keep URL and device-verification flows distinct. Do not automatically accept arbitrary forms, silently persist trust or bypass a declined call. Record unsupported-method errors separately from user decisions. A protocol fixture can validate request queuing, decline/cancel, mode handling and exact response shape without inference or paid tools.
2. **Make the bundled API genuinely self-contained.** Provide curated runnable examples and precise overloads in the map; make `ac_examples` fall back to a packaged reference index. Explain the scope of outline/symbol tools. When an MCP channel is unavailable, point to the exact bundled map path rather than making the agent discover a developer checkout. No broader filesystem permission is needed to read information already packaged with the app.
3. **Keep verification status explicit.** Distinguish syntax checked, diagnostics received and actual frame inspected for the current revision. Surface tool rejection/unsupported bridge status prominently so the agent does not spend a turn searching for an API-access problem. The existing instruction that missing feedback is not success is good; preserve it. Do not require repeated calls after an explicit denial.

No new session activity appeared in the final transcript check. This is a critique of one observed turn, not a general performance benchmark or proof that the proposed protocol fix resolves every rejection.
