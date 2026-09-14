# Roblox testing MCP

Registered as `roblox` in the repo MCP configuration and local Codex settings.
A new MCP connection exposes six tools: `roblox_status`, `roblox_launch`,
`roblox_capture`, `roblox_control`, `roblox_test`, `roblox_client_log`.

Requires Node, Roblox for macOS, Slab Frame, and the host's Accessibility and
Screen Recording grants. Run from the monorepo root:

```sh
node toolchain/robloxplorer/testing-mcp.mjs --call roblox_launch '{}'
node toolchain/robloxplorer/testing-mcp.mjs --call roblox_capture '{}'
node toolchain/robloxplorer/testing-mcp.mjs --call roblox_control '{"action":"forward","milliseconds":300}'
node toolchain/robloxplorer/testing-mcp.mjs --call roblox_control '{"action":"nade"}'
node toolchain/robloxplorer/testing-mcp.mjs --call roblox_client_log '{}'
```

Controls accept nade, forward, back, left, right, jump and escape.
Movement duration is bounded to 50–1500 ms. Roblox must be frontmost; make sure
chat is closed. Launch can target a server UUID but does not guarantee admission
or a fresh published version. Check AC_ARENA_READY in the instrumented log.

Capture saves ignored JPEG and OCR JSON under `roblox/arena/build/captures`.
MCP returns the image; CLI returns metadata. Inspect images, not only OCR matches.
Only AC arena instrumentation is returned from client logs; raw Roblox logs can
contain credentials. Loaded/playing audio flags do not prove audible output.

For motion, use Slab `frame_tape` with explicit host `blueberry`, current window
bounds, 6–10 seconds and 30 fps. Reject footage if focus/bounds changed. The alias
`local` may resolve to another fleet host. Headless test/poll/logs are available
through roblox_test; publishing remains a separate explicit arena CLI command.

Seven focused Node tests pass. Actual two-client networking still needs Studio
Server & Clients and latency simulation; the installed player is a single client.

Queue membership now follows the physical pressure pad. Use movement after inspecting camera orientation; there is no queue key.
