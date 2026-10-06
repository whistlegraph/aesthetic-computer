# Whistlegraph Rooms

Private prototype for Whistlegraph's Roblox Room ware. The default ware remains Aesthetic.Computer Piece.

The app stores a separate room history, draws paths into platform segments, offers a bounded room tool to its brain, and exports a self-contained `.rbxlx` place. The plan is a drawing of the room, not a physics simulation. Supported objects are platforms and goals; a platform can bounce a character. No generated script is executed.

`shared/roblox-room.mjs` is the app/service grammar. `src/server/Room.luau` validates the same grammar and constructs a staged Roblox model. `src/server/init.server.luau` fetches the pilot player's current room, replaces it, moves that player to spawn, and acknowledges the applied revision. Five-second polling is for a small private test, not a production fleet. Existing visitors are not managed through geometry changes; multiplayer editing remains unproven.

To connect a private test, configure the service with `WHISTLEGRAPH_ROBLOX_PREVIEW_OWNER` (the AC account subject), `WHISTLEGRAPH_ROBLOX_PREVIEW_USER_ID`, `WHISTLEGRAPH_ROBLOX_SHARE_LINK` (a Creator Dashboard share link for this experience), and a random 32+ character `WHISTLEGRAPH_ROBLOX_BRIDGE_KEY`. Put the same bridge key in the experience's Secrets Store. It is never included in the phone bundle. The configured identities are an operator-owned pilot, not public account linking or an OAuth age workaround.

Build with `rojo build roblox/rooms/default.project.json -o /tmp/whistlegraph-rooms.rbxlx`, then publish to an authorized private place. Deploy the room API and renamed Whistlegraph stream route before using the new phone build with cloud services. Existing `/api/walkieware` and `/api/walkieware-stream` clients remain supported. No share link or place ID is guessed. Until the connection is configured, Play reports that setup is missing; local editing and export work.

Validate on a real phone and Roblox client: draw a path, save and launch, observe the exact applied revision, return to the app, change width or bounce, relaunch, and restore a saved version. Test app suspension, account changes, stale saves, failed fetches and reconnects. The share-link route may open a web landing page depending on the OS and app state. A direct Roblox-to-Whistlegraph callback is not implemented.

Local checks: run `node apple/whistlegraph/bundle.mjs`, then `node --test roblox/rooms/room.test.mjs apple/whistlegraph/Tests/wares.test.mjs apple/whistlegraph/Tests/roblox-connection.test.mjs` and `node apple/whistlegraph/Tests/ware-bridge.test.cjs`. With the official Luau CLI installed, `LUAU_BIN=/path/to/luau node roblox/rooms/contract-test.mjs` runs the same valid and malformed rooms through both validators; `luau-compile --null roblox/rooms/src/server/*.luau` checks script compilation. These checks do not establish Roblox physics or mobile handoff behavior.
