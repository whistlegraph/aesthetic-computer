# Whistlegraph Dot Org — artist page

The second AC artist name, sibling to **Aesthetic Dot Computer**. First release:
**lonerclub v4pid (feat. Aesthetic Dot Computer)**, 2026-08-27.

| | |
|---|---|
| Spotify artist | `spotify:artist:04nh3YCovvkIOnefkCi7Hx` |
| Spotify album | `spotify:album:6ocFjxuo0tGamINUijXeTw` |
| UPC / ISRC | 701508062421 / QT6HV2630714 |
| DistroKid album | uuid `61394BC5-FE8A-4839-AB7CE99747305CEC` |
| Record label field | `https://aesthetic.computer` |

## Bio

> Whistlegraph Dot Org makes records out of drawings.
>
> A whistlegraph is a drawing you sing. One continuous mark, made on whatever
> surface is at hand — paper, snow, wet pavement, a sidewalk in chalk — while
> the person drawing it whistles the same shape they are drawing. The drawing
> is the score. It was invented in 2019 and the archive has grown to 291
> confirmed works, all of them at whistlegraph.org.
>
> The records start there. Every piece of the sound is made in the same place
> the drawings live: aesthetic.computer, in the open, where you can type a
> four-character code and perform any graph in the index yourself.
>
> Sibling label to Aesthetic Dot Computer.

Spotify caps the bio at 1500 characters; the above is well under. Same text
serves Apple Music for Artists, Tidal and a YouTube Official Artist Channel.

## Built assets

`python3 bin/build.py --proof` rebuilds everything. Sources live in the
untracked `refs/`; re-export them with `osxphotos` if the folder is empty.

| File | Size | Use |
|---|---|---|
| `wgdo-avatar-3000.jpg` | 3000² | profile image (Spotify min 750²) |
| `wgdo-header-source-2660.jpg` | 2660×1140 | header — "the SOURCE", a drawn score |
| `wgdo-header-sketchbook-2660.jpg` | 2660×1140 | header — sketchbook, inks, iPad |
| `wgdo-header-penbed-2660.jpg` | 2660×1140 | header — monoprint over the keyboard |
| `wgdo-header-sheets-2660.jpg` | 2660×1140 | header — loose sheets on dark wood |
| `wgdo-gallery-*-2048.jpg` | 2048² | gallery, 4 images |
| `../../loner/canvas/lonerclub-v4pid-canvas.mp4` | 720×1280, 8s | Spotify Canvas |

`proofs/` renders each header under Spotify's name overlay, so a crop can be
judged the way a listener sees it rather than as a bare photograph.

## Getting access to the page

Spotify's own claim routes — `artists.spotify.com/c/claim` and the documented
`/c/team/access/artist` — both bounce a Spotify account that already sits on a
team straight back to its existing dashboard ("reach out to that team's
admin"). Verified 2026-08-28 against the Aesthetic Dot Computer team. Two routes
remain:

1. **DistroKid, no review** — <https://distrokid.com/spotify/> grants access
   instantly for any artist in its dropdown. Whistlegraph Dot Org is not listed
   yet: it was submitted as a new artist, so DistroKid holds no Spotify ID for
   it until Spotify reports the minted profile back. Check again a day or two
   after delivery.
2. **artistsupport@spotify.com** — manual, a few days. See
   `../../spotify-for-artists-claim-reply.md`: the Aesthetic Dot Computer claim
   was **rejected twice** before it passed, because the reviewer must be able to
   tie the profile to a public page carrying *both* the contact email *and* the
   matching music. Do not send this one until:
   - [ ] `@whistlegraph` on Instagram publishes a contact email in its bio
   - [ ] that account posts lonerclub v4pid with the same cover art and title
   - [ ] whistlegraph.org carries the release and the same email

Route 2's prerequisites are worth doing regardless — they are what makes the
name findable — but route 1 costs nothing and skips the review entirely.

Canvas upload is blocked behind either route; the file is already cut to spec.
