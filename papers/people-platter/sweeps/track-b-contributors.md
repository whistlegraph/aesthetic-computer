# Track B — contributors sweep

Sweep date: 2026-05-02
Git committers (non-jeffrey, non-bot): 10
GitHub contributors (non-jeffrey, non-bot): 10
AC-handle mentions in disks/: 7

## Git committers

Real-name + commit counts collapsed across alias rows.
First/last commit dates are taken from `git log --all --no-merges`.

| Name | Email | Commits | First commit | Last commit | Notes |
|------|-------|---------|--------------|-------------|-------|
| Georgica Pettus | georgicajpettus@gmail.com | 95 | 2023-05-21 | 2023-08-23 | also ggcajp (GitHub no-reply); see merged row below |
| ggcajp | 134235108+ggcajp@users.noreply.github.com | 30 | 2023-07-04 | 2023-12-08 | GitHub no-reply alias of Georgica Pettus (combined: 125) |
| Tina Tarighian | tinatarighianwork@gmail.com | 65 | 2023-04-24 | 2023-08-08 | also ttarigh (GitHub no-reply); see merged row below |
| ttarigh | 99256332+ttarigh@users.noreply.github.com | 34 | 2023-12-04 | 2024-02-29 | GitHub no-reply alias of Tina Tarighian (combined: 99). GitHub login `ttarigh` resolves to "wwwtinazone" |
| Miles Peyton | milespeyton@gmail.com | 48 | 2024-02-20 | 2024-02-27 | GitHub login `mileshiroo` |
| idapruitt | 81336943+idapruitt@users.noreply.github.com | 26 | 2023-09-17 | 2024-02-07 | GitHub login `idapruitt`; referenced as `@ida` in disks/sno.mjs |
| rcrdlbl | dan@tlon.io | 17 | 2023-02-22 | 2023-04-07 | Tlon address; GitHub login `rcrdlbl` |
| Esteban Uribe | estebanuribe@mac.com | 14 | 2026-05-01 | 2026-05-01 | Newest contributor (May 2026); GitHub login `estebanuribe` |
| mxsage | jenson.sage@gmail.com | 5 | 2022-12-27 | 2023-02-02 | also "Sage Jenson" same email; see merged row |
| Sage Jenson | jenson.sage@gmail.com | 1 | 2022-12-30 | 2022-12-30 | Same person as mxsage (combined: 6); referenced as `@mxsage` in disks/sage.mjs, sno.mjs, bubble.mjs |
| Maya Man | mayaman26@gmail.com | 4 | 2023-01-09 | 2023-01-23 | GitHub login `mayaman`; referenced as `@maya` in disks/sparkle.mjs |
| Bash Elliott | bashelliott@gmail.com | 4 | 2023-10-17 | 2024-08-05 | GitHub login `rackodo` |

### Excluded (jeffrey — multiple identities collapse to same person)

| Identity | Email | Commits |
|----------|-------|---------|
| Jeffrey Alan Scudder | me@jas.life | 10958 |
| prompt.ac/@jeffrey | mail@aesthetic.computer | 980 |
| jeffrey | me@jas.life | 230 |
| @jeffrey on prompt.ac | me@jas.life | 51 |
| Jeffrey Scudder | mail@aesthetic.computer | 27 |
| Aesthetic.Computer | jas@AestheticComputers-MacBook-Neo.local | 15 |
| Jeffrey Alan Scudder | mail@aesthetic.computer | 8 |
| DIGITPAIN | me@jas.life | 7 |
| Jeffrey Scudder | me@jas.life | 4 |
| Aesthetic.Computer | jas@Mac.localdomain | 1 |

Combined jeffrey total: **12,281 commits**.

### Excluded (automation / bots)

| Identity | Email | Commits | Reason |
|----------|-------|---------|--------|
| Oven (aesthetic.computer) | oven@aesthetic.computer | 4233 | Automated build server (papers PDFs, OTA builds) |
| copilot-swe-agent[bot] | 198982749+Copilot@users.noreply.github.com | 30 | GitHub Copilot bot |
| Claude | noreply@anthropic.com | 1 | AI assistant commit |

## GitHub contributors (mirror at github.com/whistlegraph/aesthetic-computer)

Note: the GitHub remote is now a **stale mirror**. The canonical history lives on `git@knot.aesthetic.computer:aesthetic.computer/core` (`origin`). At sweep time, `origin/main` was 668 commits ahead of `github/main` and 0 commits behind. GitHub commit counts below reflect the mirror as of sweep date and may understate recent activity.

| Login | Commits | Profile URL | Notes |
|-------|---------|-------------|-------|
| ggcajp | 163 | https://github.com/ggcajp | Georgica Pettus |
| tinatari | 81 | https://github.com/tinatari | Tina Tarighian (also commits as `ttarigh`) |
| ttarigh | 58 | https://github.com/ttarigh | Tina Tarighian alt — display name "wwwtinazone" |
| mileshiroo | 52 | https://github.com/mileshiroo | Miles Peyton |
| idapruitt | 26 | https://github.com/idapruitt | "Ida" — referenced as `@ida` in disks/sno.mjs |
| estebanuribe | 14 | https://github.com/estebanuribe | Esteban Uribe |
| mayaman | 7 | https://github.com/mayaman | Maya Man — referenced as `@maya` in disks/sparkle.mjs |
| mxsage | 6 | https://github.com/mxsage | Sage Jenson — referenced as `@mxsage` in disks/sage.mjs, sno.mjs, bubble.mjs |
| rackodo | 4 | https://github.com/rackodo | Bash Elliott |
| rcrdlbl | 1 | https://github.com/rcrdlbl | dan@tlon.io |

### Excluded GitHub accounts

| Login | Commits | Reason |
|-------|---------|--------|
| whistlegraph | 13088 | Jeffrey Alan Scudder (verified: name "@jeffrey on prompt.ac", email mail@aesthetic.computer) |
| Copilot | 30 | GitHub Copilot bot |
| claude | 1 | AI assistant |

## AC handles in disks/ comments

Extracted from `^\s*(//|/\*|\*).*@\w+` matches in `system/public/aesthetic.computer/disks/**.mjs`, after filtering out JSDoc tags (`@param`, `@returns`), generic placeholders (`@handle`, `@user`, `@word`, `@author`), and self-references (`@jeffrey`).

| Handle | Files | Context |
|--------|-------|---------|
| @mxsage | sage.mjs, bubble.mjs, sno.mjs | Co-author credit. "Sage @mxsage + Jeffrey" / "A snowball game by @ida, @mxsage and @jeffrey." Maps to git contributor Sage Jenson (jenson.sage@gmail.com) / GitHub `mxsage` |
| @ida | sno.mjs | Co-author credit on snowball game. Maps to git contributor `idapruitt` |
| @maya | sparkle.mjs | "A sparkle emitter brush by @maya and @jeffrey." Maps to git contributor Maya Man / GitHub `mayaman` |
| @georgica | gargoyle.mjs | "A character playground for @georgica." Maps to git contributor Georgica Pettus / GitHub `ggcajp` |
| @mollysoda | selfie.mjs | "Designed w/ @mollysoda." Design collaborator — NO matching git/GitHub commit identity (AC handle only, not a code committer) |
| @dreamdealer | field.mjs | "@dreamdealer/dragon" — references a piece path, not necessarily an authorship credit. AC handle only |
| @whistlegraph | insta.mjs, booted-by.mjs | jeffrey's own GitHub/TikTok handle (not a separate person — included for completeness since it appears in the corpus) |

## Cross-reference summary (handle → git → GitHub)

Same-person mappings confirmed by this sweep:

| Real name | AC handle | Git email(s) | GitHub login |
|-----------|-----------|--------------|--------------|
| Sage Jenson | @mxsage | jenson.sage@gmail.com | mxsage |
| Ida (Pruitt?) | @ida | 81336943+idapruitt@users.noreply.github.com | idapruitt |
| Maya Man | @maya | mayaman26@gmail.com | mayaman |
| Georgica Pettus | @georgica | georgicajpettus@gmail.com, 134235108+ggcajp@users.noreply.github.com | ggcajp |
| Tina Tarighian | (none in disks) | tinatarighianwork@gmail.com, 99256332+ttarigh@users.noreply.github.com | tinatari, ttarigh |
| Miles Peyton | (none in disks) | milespeyton@gmail.com | mileshiroo |
| Esteban Uribe | (none in disks) | estebanuribe@mac.com | estebanuribe |
| Bash Elliott | (none in disks) | bashelliott@gmail.com | rackodo |
| (dan / rcrdlbl) | (none in disks) | dan@tlon.io | rcrdlbl |

AC-handle-only (no commit identity found in this sweep):

- @mollysoda — design collaborator on `selfie.mjs`
- @dreamdealer — referenced as a piece-path namespace in `field.mjs` (likely a published piece-author handle on the AC system rather than a code contributor)

## Methodology notes

- `git shortlog -sne --no-merges` and `git log --no-merges --format='%aN|%aE'` returned identical 25-row author lists across `--all` and `HEAD`; no contributors hide on stale branches.
- No `.mailmap`, `CONTRIBUTORS.md`, or `AUTHORS` file exists in the repo. `package.json` only lists `"author": "Jeffrey Alan Scudder"`.
- Canonical remote is `origin` → `git@knot.aesthetic.computer:aesthetic.computer/core`. The `github` remote (`https://github.com/whistlegraph/aesthetic-computer.git`) is a mirror and is currently behind by 668 commits. Per `~/.ac-agent-memory` the dual-pushurl was deliberately removed after an oven race silently diverged the two.
- AC-handle ripgrep matched 104 lines; after filtering JSDoc tags and generic placeholders, 7 distinct handles surfaced (6 unique people + jeffrey himself).
