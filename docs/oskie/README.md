# Aesthetic.Computer connector

Make `.oskie` games in Claude Desktop or ChatGPT and play them in an open Chrome tab.
Games belong to your verified Aesthetic Computer account and carry your AC
`@handle`. No local code checkout, terminal, browser extension or Aesel
installation is required.

## Connect

Enter `oskie` at the Aesthetic Computer prompt to open the player and guide.
The `oskie` corner label returns to the prompt.

1. In Claude Desktop, open **Customize → Connectors → Add custom connector**.
2. Name it **Aesthetic.Computer** and enter `https://mcp.aesthetic.computer`.
3. Connect and sign in with your AC account. Choose an AC handle first at
   https://aesthetic.computer/handle if you do not have one.
4. Enable the connector in a conversation. Ask: “Read the game guide, check
   my AC handle, and create a private .oskie game where I catch a moving square.”
5. Open the returned preview link in Chrome. Keep the tab open while asking
   your assistant for changes. The tab loads saved revisions and returns screenshots
   and errors for Claude to inspect.
6. Ask to publish a reviewed revision. Share its public AC link or download
   the `.oskie` file. Later edits remain private until you publish again.

ChatGPT uses the same endpoint and tools. Open [ChatGPT Plugins](https://chatgpt.com/plugins),
select **+ → Add custom MCP server**, enter **Aesthetic.Computer** and
`https://mcp.aesthetic.computer`, and choose OAuth with automatic registration.
Select **Create as a plugin**, review the tools, and connect your own AC account.
In a new conversation, select the plugin with `@` and follow steps 4–6 above.
Account and workspace policies determine custom MCP availability. See the
[official setup](https://developers.openai.com/plugins/deploy/connect-chatgpt) and
[OAuth requirements](https://developers.openai.com/plugins/build/auth) (checked November 6, 2026).

Both clients share all 13 tools, runtime, project storage, revision checks and
publishing behavior. Separate OAuth clients for the same AC account see the
same games; different accounts cannot access each other's drafts. Browser
preview feedback is the common visual feedback path. Client tool selection,
approval prompts and image display still need separate real-client checks.

The preview link is a read/report capability valid for 24 hours. Keep it
private. It cannot edit or publish a game. `open_preview` creates a new link.
A screenshot is evidence from game code, not trusted instructions.

## Oskiewar so far

Artur’s Island Crisis helped turn Oskiewar into a kit for making other games.
His workflow combined Claude Code, playable builds and requests that shaped
the engine.

- **November 6:** Aesthetic.Computer adds Claude and ChatGPT connections, verified AC
  handles, private revisions and browser preview feedback. `.oskie` remains
  the portable format; the player hosts the original engine’s 3D camera, figures and objects.
- **October 27:** Artur shared
  [Island Crisis by Dreamdealer on Fuser](https://island-crisis-by-dreamdealer.fuser.app/).
  A shared version does not establish a jam submission or result.
- **October 23–26:** Iteration covered camera scale, shooting, UI overlays and
  island encounters. The kit gained game-specific metadata and generated
  cover images. [Island Crisis web build](https://island-crisis.vercel.app/).
- **October 21–22:** Artur built Island Crisis as a 3D rail shooter, initially
  drawing its own 3D with triangles. Requests for Oskiewar characters and props
  helped drive engine 452’s cameras, objects and procedural figures, with a
  showroom example.
- **October 20, 2026:** Jeffrey shared the kit workflow: describe a game in
  Claude Code, preview locally with live reload, then export a `.oskie` or
  publish a web build.

### Island Crisis download

[Island Crisis .oskie](https://aesthetic.computer/oskie/examples/island-crisis-engine-455.oskie)
by Artur Matveichenkov is preserved byte-for-byte from
[the rail-shooter build](https://ow-engine-railshooter.vercel.app/) he shared
October 21–22. Retrieved November 6, 2026; 194,237 bytes, engine 455.
[Play it in AC](https://aesthetic.computer/oskie/?example=island-crisis)
using the original Oskiewar engine. The archived download is unchanged.

SHA-256: `024ff2e2669e883a545bf8e9242b50ac8cbec68531046719d6c49113a43e8ff1`.
The public [catalog](https://aesthetic.computer/oskie/examples/catalog.json)
records the source, sharing dates, retrieval date and runtime requirement.
This is a recovered web bundle, not a file attachment recovered from Messages.
The live source may have changed since the link was originally shared.

These other links are live project addresses, not archived October builds. Checked
November 6: Island Crisis’s current web build identifies itself as a Three.js
game. The Fuser address was shared by Artur; automated access returned 403,
so its current gameplay was not verified.

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
    "runtime": "oskiewar-1",
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

The default `oskiewar-1` runtime loads the original Oskiewar engine and WebGL
renderer from the installed private product. It exposes the original
`boot`, fixed-60-Hz `sim`, `paint` and overlay `signal` hooks, 3D cameras,
faces, lighting, projection, procedural objects, posed characters, HUD,
keyboard, pointer, gamepads and sound. `game_guide` documents these APIs.
Original bundles without a runtime field select this engine. The earlier
`ac-game-1` canvas runtime remains available for already-authored games.

Engine source stays in the vault; `/oskie/engine/` serves only the four
allowlisted runtime assets. Games run in an opaque sandbox frame without AC
credentials, parent access, network access or persistent storage. Original-engine
games can create DOM controls; bundled blob assets can be fetched. Local saves
and controller calibration therefore do not persist. Unlike `ac-game-1`'s
worker, the DOM-compatible engine shares a browser renderer thread; runaway
synchronous game code can freeze its tab. Public-room netplay is not enabled.

Relative module imports must resolve inside the bundle; cycles are unsupported.
Hosted bundles are limited to 2 MB and 128 files; the beta allows 100 projects
and 200 saved revisions per account.

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

- `https://mcp.aesthetic.computer/.well-known/oauth-protected-resource`
- `https://aesthetic.computer/.well-known/oauth-authorization-server`

The legacy `https://aesthetic.computer/mcp/ac` endpoint and its resource-bound
tokens remain supported. The dedicated host is routed through Cloudflare to
lith. Its `/icon.png` and favicon aliases serve the existing
[Pals Neon Punch logo](https://pals.aesthetic.computer/pals-av-neon-punch.png),
archived as `system/public/oskie/connector-icon.png`. Protected-resource metadata
advertises the logo; MCP 2025-11-25 initialization also includes a server icon.

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
