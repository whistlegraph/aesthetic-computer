# oskiewar on Steam — the marketing model

Written 2026-09-15, the day the appid (5280790) arrived. The reel factory's
own manual is `xbox/live/MARKETING.md`; this file is the Steam-shaped half:
what Steam counts, what it ignores, which of those numbers a script can pull,
and which surfaces only a browser can touch.

## What Steam actually rewards

From Valve's visibility doc (sources in `STEAM.md`):

- **Launch visibility is automatic and unpurchasable.** Every new release
  goes into the New Releases Queue, weighted toward titles with the *fewest*
  views. New & Trending follows if the title "is doing well" — purchases and
  playtime, not page traffic.
- **Store page traffic and conversion rate are explicitly not factors.**
  Neither are wishlists, algorithmically.
- **Wishlists are a mailing list.** Every wishlister gets an email at launch,
  at any discount of 20%+ (8-hour minimum), and when a demo ships. A two-week
  cooldown per app. That is their whole value, and it is a large one: the
  launch email is the only marketing Steam sends on the game's behalf.
- **Tags and language support** are the two page-side levers that change who
  the recommender shows the game to. Wrong tags buy the wrong audience.
- **Update visibility rounds** are unlimited after the launch window — each
  substantive update earns a "Recently Updated" placement.

So the model has three loops, in order of when they run:

1. **Coming-soon → wishlist accumulation.** Outside-Steam traffic (the reel
   factory, three reels a day, already captioned) lands on the coming-soon
   page; the page converts to wishlists; nothing else matters until launch.
   Two levers: the reel captions carry the store URL, and the page itself is
   honest, tagged right, and has a trailer.
2. **Launch → the email + the queue.** Release day converts the wishlist
   list; the queue does its own thing. Lever: pick the day, ship the build
   that the page describes, and post the launch event (the one canvas where
   marketing text is legal).
3. **Updates → visibility rounds.** Every version bump that changes play is
   an update round with an event post. The game ships a version a day on
   the web; Steam updates should be batched into rounds with something to
   say.

## What a script can pull

Valve's partner Web API has **nothing** for editing a store page, posting
events, or reading store traffic — those are the partner site, a browser.
It has exactly two marketing-relevant read endpoints, both on
`IPartnerFinancialsService` and both needing a publisher key with the
Financial permission:

| Endpoint | Grain | Returns |
|---|---|---|
| `GetAppWishlistReporting` | app × GMT day | adds / deletes / purchases, per country, per language |
| `GetDetailedSales` | Pacific day, paged by `highwatermark_id` | every sale row with package, country, discount |

`bin/wishlists.mjs` pulls both into `vault/oskiewar/steam/*.jsonl`. The key
lives in `vault/oskiewar/steam.env` (`STEAM_PUBLISHER_KEY`), minted at
partner.steamgames.com → Users & Permissions → Manage Groups → Create WebAPI
Key, and IP-allowlisted if the pull runs from one box.

**Attribution** is a join, not an API: the reel ledger
(`xbox/live/marketing/ledger.json`, one row per posted reel with day, slot,
segment, and Meta insights) against the wishlist series by day. A reel that
moves wishlists shows up as a day with adds above the segment's baseline;
a segment that never does is a caption problem. That join is the first
report worth writing once the page has been public two weeks — before that
there is no baseline.

## What only a browser can do

| Task | Route |
|---|---|
| Fill and edit the store page (`store-page/fill.md`) | Claude Chrome extension driving partner.steamgames.com; the fill sheet is field-ordered for exactly this |
| Store traffic breakdown, wishlist conversion by email | partner site → Marketing & Visibility → Traffic Breakdown (no API) |
| Post events / announcements (launch, update rounds) | partner site event editor |
| Mark ready for review; set live; press Release | human — each is a deliberate button |
| Set a build live on a branch | partner site SteamPipe page (`steamcmd` can set `SetLive` on upload, but only to branches without a password) |

The extension is the CDP route this repo trusts for a signed-in browser:
the puppet daemon's own CDP path to neo failed on 2026-09-15 and a
hand-rolled CDP script against the user's Chrome is (correctly) refused by
the harness. Keep automation on the extension, keep the fill sheet the source
of truth, and let the human press the three buttons.

## The calendar it implies

- **Now → page approved (3–5 business days):** fill the page from
  `store-page/fill.md`; upload the depots; cut a trailer from the 16:9 lane.
- **Coming-soon live (≥14 days):** reel captions point at the store URL;
  `wishlists.mjs` runs daily; watch the country split — it decides whether
  the launch-day discount is worth a regional price pass.
- **Launch (≥ 2026-10-01):** release button, launch event with the 800×450
  cover, then nothing for two weeks while the queue runs.
- **After:** update rounds on real changes, a 20%+ discount no sooner than
  the email cooldown allows, and the reel factory unchanged.
