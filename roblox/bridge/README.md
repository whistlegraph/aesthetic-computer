# AC → Roblox media bridge

The live, read-only JSON interface is
https://assets.aesthetic.computer/roblox/manifest.json . It is hosted in AC’s
existing asset store, so updates do not require a Lith or game-code deployment.
It is a curated media feed, not a general proxy or a raw-chat mirror.

The first entry is AC’s Pals artwork, imported from its public AC PNG endpoint
into Whistlegraph’s Roblox account. Image asset `85149608937004` is approved and
active. `manifest.json` maps its source URL to the Roblox image ID.

From the repo root:

```sh
node roblox/bridge/import.mjs import ac-pals
node roblox/bridge/import.mjs poll ac-pals
node roblox/bridge/publish.mjs
node roblox/bridge/publish.mjs --live
```

The first upload is already recorded under ignored `build/ac-pals.json`;
repeating import refuses to create duplicates. Add an explicitly approved entry
to `catalog.json` to import another AC-owned PNG. Import reads the existing
Whistlegraph vault credential; the public manifest contains no credentials.
Publish uses the host’s existing Spaces credentials and changes only
`roblox/manifest.json` in `assets-aesthetic-computer`.

After Roblox reports an approved, active image, add its ID to the manifest,
increment `revision`, and publish. The game polls every 60 seconds, validates
schema and approved numeric IDs, and displays the first entry on the lobby wall.
Transport caching is 30 seconds. Failures retain the last good image; newer empty
manifests clear it. Revision rollback is ignored. Up to 12 entries are accepted;
this first display uses one, with no carousel yet.

The renderer uses a fixed credit, not arbitrary external text. Raw clock chat,
player identity linking, live event delivery, audio import and arbitrary remote
URLs are not implemented. Those need their own contracts; a chat bridge must
meet Roblox external-text filtering requirements before displaying messages.

Source: [Roblox assets API](https://create.roblox.com/docs/cloud/guides/usage-assets),
[HTTP service](https://create.roblox.com/docs/cloud-services/http-service).
