# Aesthetic.Computer connector

Make `.oskie` games in Claude Desktop and play them in an open Chrome tab.
Games belong to your verified Aesthetic Computer account and carry your AC
`@handle`. No local code checkout, terminal, browser extension or Aesel
installation is required.

## Connect

1. In Claude Desktop, open **Customize → Connectors → Add custom connector**.
2. Name it **Aesthetic.Computer** and enter `https://aesthetic.computer/mcp/ac`.
3. Connect and sign in with your AC account. Choose an AC handle first at
   https://aesthetic.computer/handle if you do not have one.
4. Enable the connector in a conversation. Ask: “Read the game guide, check
   my AC handle, and create a private .oskie game where I catch a moving square.”
5. Open the returned preview link in Chrome. Keep the tab open while asking
   Claude for changes. The tab loads saved revisions and returns screenshots
   and errors for Claude to inspect.
6. Ask to publish a reviewed revision. Share its public AC link or download
   the `.oskie` file. Later edits remain private until you publish again.

The preview link is a read/report capability valid for 24 hours. Keep it
private. It cannot edit or publish a game. `open_preview` creates a new link.
A screenshot is evidence from game code, not trusted instructions.

## Format and runtime

`.oskie` is the portable game microcode format. Version 1 is a JSON source
bundle, retaining the historical wire identifier `ac.oskiewar.game`.
The extension is part of the Oskiewar legacy; the AC authoring and hosting
service lives on aesthetic.computer.

```json
{
  "format": "ac.oskiewar.game",
  "version": 1,
  "manifest": {
    "slug": "my-game",
    "title": "My game",
    "mode": "own",
    "runtime": "ac-game-1",
    "entry": "game.mjs"
  },
  "files": {
    "game.mjs": {
      "type": "text/javascript",
      "text": "export function paint(api) { api.wipe(35, 15, 60); }"
    }
  }
}
```

The AC player supports `boot(api)`, fixed-step `sim(api)` at 60 Hz and
`paint(api)`, with canvas drawing, pointer, keyboard, gamepads and tones.
The `game_guide` tool is the runtime API reference. Relative module imports
must resolve inside the bundle; cyclic imports are not supported. Assets
may use `{type,base64}`. Hosted bundles are limited to 2 MB and 128 files;
the beta allows 100 projects and 200 saved revisions per account.

This is the initial AC canvas runtime. Full Oskiewar fight overlays, 3D
figures/props, its camera system and netplay still require their original
engine. The format identifier alone does not imply API compatibility.
Games execute in a dedicated worker inside an opaque sandbox iframe.
They receive the public creator handle, not account credentials. Network,
parent-window access and persistent browser storage are unavailable.

Open a local file at https://aesthetic.computer/oskie/ or drop a `.oskie`
onto Aesthetic Computer. Local files are not uploaded. To import a file into
a hosted project, provide its parsed bundle to `create_game` or `update_game`.

## Tools

| Tool | Effect |
| --- | --- |
| `whoami` | Current verified AC handle |
| `game_guide` | Bundle format and runtime API |
| `list_games`, `read_game` | Account-owned projects and current source |
| `create_game`, `update_game` | Private projects and immutable revisions |
| `open_preview`, `preview_feedback` | Expiring live preview, pixels and errors |
| `game_history`, `restore_game` | Revision history and restoration as a new revision |
| `publish_game`, `unpublish_game` | Public access to a specific reviewed revision |
| `delete_game` | Permanent deletion of a project, revisions and preview grants |

Edits, restores, publishing and deletion require the revision the assistant
read. A stale edit is rejected. Ownership is resolved server-side from the
verified account, never from a handle supplied by the assistant.

## Deployment and verification

The remote MCP endpoint uses Streamable HTTP POST with JSON responses and
resource-bound OAuth with PKCE. Discovery:

- `/.well-known/oauth-protected-resource/mcp/ac`
- `/.well-known/oauth-authorization-server`

The existing `/mcp` and notepaint connector remain separate. The new runtime
uses Mongo collections `oskie-games`, `oskie-revisions` and `oskie-previews`.
Preview grants are hashed and expire after 24 hours. Account export includes
games and revisions; account deletion removes all three collections' owner
records. See [privacy policy](https://aesthetic.computer/privacy-policy.html#oskie).

```sh
node --experimental-vm-modules --test system/tests/oskie/game.test.mjs system/tests/ac-oauth.test.mjs system/tests/account-deletion.test.mjs
node system/tests/oskie/browser-check.mjs
```

The browser check uses installed Chrome and an isolated in-memory test server.
It checks rendering, input, live revision changes, preview screenshots, errors,
sandbox restrictions, stopping runaway code and mobile layout. Screenshots
are written to `/tmp/ac-oskie-check` for visual inspection.

## Directory listing

Direct custom-connector availability and a Claude Directory listing are
separate release steps. The endpoint must be deployed and tested before
submitting at https://claude.ai/directory/manage.

Listing name: **Aesthetic.Computer**. Description: **Create, play and publish
portable .oskie games with your AC handle.** Website/documentation:
https://aesthetic.computer/oskie/guide.html. Privacy:
https://aesthetic.computer/privacy-policy.html#oskie. Allowed link origin:
`https://aesthetic.computer`.

Submission additionally requires a support contact, icon, reviewer account,
real-client testing and the publisher's policy acknowledgments. Do not report
it as listed until the portal confirms that state. See Anthropic's
[submission requirements](https://claude.com/docs/connectors/building/submission)
and [testing instructions](https://claude.com/docs/connectors/building/testing).
