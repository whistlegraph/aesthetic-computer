# Piece and harness requests

| Request | Result |
| --- | --- |
| First prompt: `3+3 = ?` | Edit the AC piece to display `6` or `3 + 3 = 6`, inspect the preview, then answer briefly in chat. |
| Ordinary creative prompt | Read the existing source and express the requested result in the piece. |
| Explicit discussion or explanation | Answer in chat; an artwork edit is optional when requested. |
| “Which model are you using?” | Read the live session settings through `aesel_settings`. |
| “Open your settings” | Open the existing Settings panel through the same tool. |
| “Switch to Codex” | Queue the provider change, finish the current reply, then connect the replacement with conversation context. |

The system prompt carries the piece-first contract. A narrow request matcher skips
canvas capture for obvious harness questions; it does not choose actions or bypass
inference. The hosted bridge calls the settings controller directly. Claude and
Codex reach it through a private session-local Unix socket exposed in their MCP
configuration. No shell edits or source discovery are needed.

Settings reads return the active provider, model, effort, auto-publish state,
account handle, and supported choices. Updates accept only provider, model,
effort, and auto-publish. Changes merge during a turn and apply after successful
completion; interruption cancels them. A failed provider connection preserves
the previous connection. AC chooses its hosted model and effort automatically.
Phone/web clients retain piece-first prompting but do not advertise desktop
settings controls.

Intermediate public text and emitted code use one selectable, horizontally
scrolling line below the donkey, spanning the window. It retains the latest
16,000 characters, follows arriving output, and pauses following while the reader
scrolls back. End resumes following. Final replies remain in the notebook.
Listening and thinking use eight new held poses; tool activity uses the running
cycle. Reduced motion holds one pose and hidden windows stop animation.

Checks: the offline real-TUI fixture verifies queued provider handoff, connection
failure rollback, direct panel opening, and unchanged artwork for settings calls.
The Electron horizon fixture verifies scrolling, placement, changing poses,
settings opening, narrow windows, bounded output, and idle cleanup. Existing
waveform unit failures and notebook text/preview overlap reproduce on the parent
commit; they are separate from these changes.
