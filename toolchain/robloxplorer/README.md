# robloxplorer

Aesthetic Network's Roblox exploration and publishing tool, maintained in the main AC monorepo.

```sh
node toolchain/robloxplorer/robloxplorer.mjs account
node toolchain/robloxplorer/robloxplorer.mjs search whistlegraph
node toolchain/robloxplorer/robloxplorer.mjs games
node toolchain/robloxplorer/robloxplorer.mjs groups
node toolchain/robloxplorer/robloxplorer.mjs endpoints
node toolchain/robloxplorer/robloxplorer.mjs campaign
node toolchain/robloxplorer/robloxplorer.mjs publish ./place.rbxlx UNIVERSE_ID PLACE_ID
```

Node 22+; no dependencies. Output is JSON except help. Public exploration needs no credentials. Follow creators → group memberships → user/group games → universe details. Game lists return Roblox pagination cursors. Search currently searches creators, not the complete experience catalog.

## Whistlegraph access

The public username currently resolves to user `1366409382`; this is not proof of account authorization. Store `ROBLOX_API_KEY=...` in `vault/whistlegraph/roblox.env` (outside tracked source), or supply it through the environment. Optional `ROBLOX_USER_ID` overrides the public account lookup. Restrict the file to mode 600. The `account` command reports credential presence without exposing or validating it.

Create the API key in the [Creator Dashboard](https://create.roblox.com/dashboard/credentials). Grant `universe-places` Write only for the target experience. No browser cookies are used.

`publish` reads the place file and prints the target, size, and SHA-256 without uploading. Add `--live` to upload a published version with the configured key. Roblox enforces that key's resource permissions. The upload response includes the new version number. An existing universe and place are required; creating the first experience and setting its public availability remain Studio/Creator Dashboard steps. Uploading a version does not itself make a private game public.

## Aesthetic Network channel

`campaign.json` is the local draft registry for our Roblox presence. Begin with a playable music/drawing experience; use public visits, current players, and favorites to assess discovery. No telemetry is currently collected or transmitted.

Public lookup found [Sky Pool](https://www.roblox.com/games/4855797621/Sky-Pool), universe `1650467744`, place `4855797621`, and group `5524936` (whistlegraph's pals). These are recorded as discovered resources, not selected publishing targets.

Roblox games do not host arbitrary inbound HTTP endpoints. A future server-side Luau bridge can call AC services through HttpService; Open Cloud messaging can send commands into an owned experience. `endpoints` lists the messaging API for reference, but this version does not send messages, deploy services, or install a bridge. Register concrete service URLs only when their integration exists.

The first game is [Aesthetic Arena](../../roblox/arena/README.md), a private 1v1
ring-out prototype with a lobby, queue, countdown, and server-controlled combat.
Its source, pinned target, API runner, and checked headless release are in
`roblox/arena/`. Creation used the authenticated browser once; configuration,
uploads, and headless testing now use the key. The CLI itself never reads cookies.

The browser interface, MCP adapter, authenticated inventory, and campaign metric
history are future work. Review Roblox's current promotion and link requirements
when preparing a public campaign.

## References

- [Public users API](https://create.roblox.com/docs/cloud/reference/features/users)
- [Universe APIs](https://create.roblox.com/docs/cloud/reference/features/universes)
- [Place publishing](https://create.roblox.com/docs/cloud/guides/usage-place-publishing)
- [HTTP service](https://create.roblox.com/docs/cloud-services/http-service)
- [Ad integrations](https://create.roblox.com/docs/production/promotion/ad-integrations)

Run focused checks with `node --test toolchain/robloxplorer/robloxplorer.test.mjs`.
