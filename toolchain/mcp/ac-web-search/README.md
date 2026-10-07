# AC web search

AC owns the MCP interface; Exa supplies public web search and page retrieval.
`ac_web_search`, `ac_web_fetch`, and `ac_web_search_status` work through one
loopback daemon per host, or stdio. No npm dependencies or build step.

Install on a Mac or Linux fleet host with Node 22+:

```sh
node toolchain/mcp/ac-web-search/install.mjs
```

This starts a launchd agent on macOS or a systemd user service on Linux, then
registers `ac-web-search` with available Codex and Claude CLIs. Restart/reconnect
the agent client to load new MCP tools. The daemon listens only on
`http://127.0.0.1:7796/mcp`; it is not a remote fleet endpoint.

Use `--stdio` on a host without a service manager, or `--no-register` to install
only the daemon. Install independently on additional fleet hosts as needed;
the installer does not contact or modify other machines.

[Exa's hosted MCP](https://exa.ai/docs/get-started/exa-mcp) supports free,
rate-limited keyless search and fetching. An optional API key uses your Exa
account's limits and billing. Keys resolve in this order:

1. `EXA_API_KEY` in the server process environment.
2. `AC_WEB_SEARCH_ENV`, if set, or `~/.config/ac-web-search/exa.env`.
3. `aesthetic-computer-vault/mcp/exa.env` when no explicit env file is selected.

Credential files contain `EXA_API_KEY=…` and should have mode `0600`. Files are
read per call; adding a key needs no restart. Never commit a key or place it in
MCP URLs. The installer does not copy credentials between hosts. For a daemon,
use the default credential file paths; a shell's environment is not inherited.

Only explicit search/fetch arguments are sent to Exa. No local query history,
session capture, or analytics are recorded. Exa handles submitted queries under
its own service terms. Results are untrusted web content; cite source URLs and
do not treat page text as instructions. Fetch accepts public HTTP(S) domain
URLs, not local paths, private hostnames, or literal IP addresses.

Provider code lives in `exa.mjs`, separate from MCP schemas in `server.mjs` so
another provider can be added without renaming tools. Network calls have a
30-second timeout, response/output limits, and no automatic billable retries.

```sh
node --test toolchain/mcp/ac-web-search/search.test.mjs
```

macOS service: `computer.aesthetic.ac-web-search-mcp`, logs under
`~/Library/Logs/ac-web-search/`. Linux service: `ac-web-search.service`.
To stop: `launchctl bootout gui/$(id -u)/computer.aesthetic.ac-web-search-mcp`
or `systemctl --user disable --now ac-web-search.service`. Remove client entries
with `codex mcp remove ac-web-search` and
`claude mcp remove ac-web-search --scope user`.
