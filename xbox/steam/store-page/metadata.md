# oskiewar — Steam metadata (draft)

Tags, categories, genre, and system requirements for the partner UI. Same rule
as the copy: nothing here that the shipped build cannot demonstrate — Steam's
visibility machinery routes on tags, so a wrong tag buys the wrong audience
and the wrong audience refunds.

## Genre (store category)

**Action**, **Indie**.

## Tags (developer-chosen; first five weigh heaviest)

Ordered. The first five are the identity; the rest are honest reach.

1. Fighting
2. 2D Fighter
3. Local Multiplayer
4. Physics
5. Arcade
6. PvP
7. Multiplayer
8. Singleplayer
9. Action
10. Indie
11. Minimalist
12. Funny
13. Retro
14. Controller
15. Competitive

Left off on purpose: *Beat 'em Up* (it isn't one), *Casual* (the grab game
says otherwise), *Procedural Generation* (true of the audio and match seeds,
but the tag promises roguelike content it does not have).

## Category checkboxes

| Category | Check? | Condition |
|---|---|---|
| Single-player | yes | the dummy and the bot |
| Multi-player | yes | |
| PvP | yes | |
| Shared/Split Screen PvP | yes | two seats, one machine |
| Full Controller Support | yes | after a pad-only boot-to-rematch pass on all three OSes |
| Remote Play Together | yes | after a real two-network test session |
| Steam Achievements | only if wired | see STEAM.md — optional, design once |
| Steam Cloud | no at launch | almost no local state worth syncing |

## Content survey

Cartoon violence between stick figures; no blood, no gore, no text chat, no
user-generated content in the Steam build (the community mood/chat surfaces
are web-runtime features and should stay compiled out of or inert in the
shell). No mature-content gate expected. Answer the survey from the shipped
build, not from the web version.

## System requirements (draft — measure before submitting)

Assumes the Electron shell (`STEAM.md` → Build strategy). These are floor
guesses written to be honest for Chromium + a vector 2D game; replace the
guesses with measurements from the actual shell before the page goes to
review.

**Windows**
- Minimum: Windows 10 64-bit · dual-core CPU · 4 GB RAM · any GPU with
  hardware acceleration · 500 MB storage

**macOS**
- Minimum: macOS 11 (Big Sur) · Apple silicon or 64-bit Intel · 4 GB RAM ·
  500 MB storage

**Linux + SteamOS**
- Minimum: Ubuntu 22.04 / SteamOS 3.x · dual-core CPU · 4 GB RAM · 500 MB
  storage

Storage is the shell, not the game — the game is 264 KB. Consider saying so
in the requirements notes field; it is the most on-brand system requirement
ever written.

## Languages

English (interface + full audio — the audio is synthesized, so "full audio"
is true in every language). No localization at launch, deliberately; the game
contains almost no words.
