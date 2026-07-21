# Menu Fighter

**Menu Fighter** is a compact competitive fighting game starring two Pals. The
name is written with a space in player-facing copy, `MenuFighter` in code that
needs an identifier, and `menu-fighter` in paths and URLs.

The first playable is already the Aesthetic Computer `fight` prototype. This
directory is the product boundary around that work: identity, platform shells,
shipping decisions, and the migration plan. Gameplay remains in AC's existing
fight modules until there is a measured reason to extract it.

## Implemented now

- The crossed-Pals Menu Fighter icon opens a centered, screen-dimming popover.
- `TRAIN` starts local freefight without an account or network.
- `FIND` unlocks after the session server verifies a real AC token and handle;
  the local vertical slice then uses targeted signaling and direct WebRTC
  rollback. It is not deployed or tournament-certified.
- Global matchmaking, spectating, TURN, tournament services, and new
  infrastructure remain plans only.

## Play it

```sh
npm start
```

Then open <http://localhost:8888/menu-fighter>. The original `/fight` route
remains the engineering playground, including `fight:boxes`, `fight:synctest`,
and `fight:lag`.

## Project map

| Area | Source of truth today |
| --- | --- |
| Deterministic simulation | `system/public/aesthetic.computer/lib/fight/sim.mjs` |
| Rollback session | `system/public/aesthetic.computer/lib/fight/session.mjs` |
| Determinism harness | `system/public/aesthetic.computer/lib/fight/rollback.mjs` |
| Game presentation | `system/public/aesthetic.computer/disks/fight.mjs` |
| Public piece name | `system/public/aesthetic.computer/disks/menu-fighter.mjs` |
| macOS comparison shell | `macos/fight-runner/` |
| Xbox experiments | `xbox/dynamic-shell/`, `xbox/native-bios/` |
| Product art | `menu-fighter/art/` |

See [ARCHITECTURE.md](ARCHITECTURE.md) for the cross-platform shape,
[NETCODE.md](NETCODE.md) for the peer-to-peer plan, and
[TOURNAMENT.md](TOURNAMENT.md) for certification, referee, and venue gates.
[LOBBY.md](LOBBY.md) specifies Find Match, Watch Match, handled-user presence,
private `VS` codes, local/LAN play, and guest rules. [INFRASTRUCTURE.md](INFRASTRUCTURE.md)
maps that design onto the current DigitalOcean fleet and purchase stages.

## Product principles

- One deterministic 60 Hz game, not platform-specific gameplay forks.
- Controllers first, keyboard always supported, local versus never dependent
  on a service.
- Online play sends inputs, not world snapshots.
- A menu is part of the game: character, rules, stage, rematch, and social
  rituals should feel playable rather than like setup screens.
- The two-fighter Pals mark is the identity; platform icon masks are derived
  from one unmasked master.

## Next playable milestone

Two Macs join with a short room code, negotiate a direct WebRTC data channel,
complete a deterministic startup handshake, play a full match, rematch, and
save a replay made only from the seed plus confirmed inputs. The same match is
then replayed by a neutral verifier. A TURN or tournament relay is a route, not
a different game path.
