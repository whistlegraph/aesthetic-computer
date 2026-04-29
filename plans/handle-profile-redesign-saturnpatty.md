# saturnpatty's `/@handle` profile sketches — analysis

Source: `~/Desktop/saturnpatty/profiles/image0.jpeg`
Email: "profiles" — Mon Feb 16 2026, `bogdanborisov02@gmail.com` → `mail@aesthetic.computer`
Author note: drawn the day after he discovered kidlisp; motivated by *"they only have paintings and i wanted to see all @bash's brushes"*

---

## What's actually on the page

A graph-paper notebook, two pages, pencil. Heavily annotated, exploratory rather than spec'd. The left page is mostly handle-identity exploration; the right page is mostly the body content of a profile.

### Left page

- **Title strip (top):** "designs of profile a.c — use different [...]" + a "name" label. Frames the whole spread as a profile-design study.
- **Handle treatments — many variants of `@sat` / `@sal` side by side:**
  - plain text handle
  - rounded pill / "name plate"
  - filled colored bubble (annotated *"colored"*)
  - bubble with status dot, annotated *"● online"*
  - small caption under the cluster about inclusivity / typeface choice (legibility for many name styles)
- **Navigation row:** `# new search`, plus a dropdown-ish strip annotated *low and 3/3* — pagination or sort indicator across a user's content.
- **Concept diagram (middle):** bubbles connected by arrows — *see → preview cancelling*, *see → brushes*, *see → pieces*, with one node tagged *reusable*. Reads as a transition map: how you move between previewing a piece, opening it, and reusing components from it on the profile.
- **Mood / voice row:** two faces labeled *happy* and *screechy*, paired with small device mockups. Mood as a first-class profile attribute.
- **Bottom strip:** long horizontal layout sketches — phone-shaped frames in a row, plus what looks like "long screen vs ..." — early thinking about responsive layout.

### Right page

- **Codes header (top):** three small framed boxes containing kidlisp-style short codes (e.g. `0M0017 / 1162`, `06 A2 T6`) with a `0.05.10.13` timestamp/cachestamp annotation. *use abstract sound* sits above.
- **Voice/sound caption:** *use signature as reference for voice* — a user's drawn signature feeds their sonic identity.
- **brushes panel:** small framed box labeled *brushes*. Stand-alone section, separate from paintings.
- **pieces panel:** larger box labeled *pieces*, annotated *previews*. Grid of authored pieces with thumbnail previews.
- **Multiplayer cluster:** small device mockups with smiley/heart icons, annotations *in multiplayer ... name*, *PVP or*, *wknd* — multiplayer presence + alternate name in MP context.
- **Live-vs-archive split:** *in-life vs anything* near a feed-card mockup.
- **Action row:** `# $ ! !` — a four-button shelf (likely share / tip / flag / message or similar).
- **Feed mockups:** several card-strip layouts labeled *feed*, *sound*, with a closing tag *#3 set / RC 03*.

---

## The design moves (interpretation)

Seven distinct ideas are doing real work in this sketch:

1. **Handle as designed object, not just text.** He's iterating through several presentations of `@sat`/`@sal` — pill, colored bubble, plain, with online dot. The handle is the user's "name plate," and it should render consistently across AC the way a kidlisp short-code does for a piece.

2. **Profile = composite catalog, not paintings.** The current `/@handle` is paintings-only. He's proposing parallel sections: **paintings**, **brushes**, **pieces**, plus codes/timestamps. Each user becomes browsable across every kind of artifact AC supports, not just one.

3. **Kidlisp short-codes anchor the catalog.** The framed `06 A2 T6` / `0M0017` boxes show that every tile on a profile is addressable via its short code. That's why he says he could only design this *after* discovering kidlisp — the IDs make the per-tile addressing cheap.

4. **Sound + voice as identity.** "use abstract sound" / "use signature as reference for voice" — handles get a sonic signature derived from the user's own drawn signature. Profiles aren't silent.

5. **Mood as a live state.** *happy / screechy* with face icons. Profiles carry a current emotional state, not just static bio.

6. **Multiplayer-aware identity.** Notes about an alternate name *in multiplayer*, plus *PVP or*, *wknd* — the profile knows about MP presence and may surface a different name there.

7. **Live vs archive split.** *in-life vs anything* + the feed cards — a profile has a "live now / recent" surface and a deeper archive, the way `at.aesthetic.computer` already does for the system as a whole.

---

## How this maps to AC today

| Sketch element | Current AC | Gap |
|---|---|---|
| Handle pill + online dot | `/@handle` shows handle as plain text | No persistent identity chrome, no presence |
| Brushes section | Brushes exist but aren't surfaced per-user | The *exact* gap he flagged ("see all @bash's brushes") |
| Pieces grid w/ short codes | Pieces are URL-addressable, kidlisp has short codes | No per-user index of authored pieces |
| Paintings | Already on `/@handle` | Becomes one section among many |
| Mood / voice | Doesn't exist | New surface |
| MP-aware name | Multiplayer rooms exist (`squash.mjs`, `1v1.mjs`) but profile doesn't know | New coupling |
| Live-vs-archive feed | `at.aesthetic.computer` has system-wide live state | Missing per-user equivalent |

His own framing: *"they look like at.aesthetic.computer in a way…"* — i.e. this is `at.` scoped to a single user.

---

## Things I couldn't read

- The handful of words behind *brushes* on the right page (*puebelet*? *bushen*? — unclear).
- The captions inside the small device mockups along the bottom strips.
- The `RC 03` / `#3 set` legend at the bottom-right corner — could be a version label or a palette reference.

If you want a higher-fidelity read, a tighter close-up scan of the right-page top quadrant would resolve most of these.

---

## Where this could go

- **Spec a `/@handle` v2** that adopts sections 2 + 3 first (brushes + pieces with kidlisp short-codes as IDs). Lowest friction, highest user-visible payoff, and matches the explicit ask from the email.
- **Defer mood + voice + MP-aware name** until v2 ships and you can see whether handle-identity-as-object is worth investing in.
- **Reuse `at.aesthetic.computer`'s layout primitives** rather than designing fresh — Bogdan himself flagged the resemblance, and reusing primitives keeps the system coherent.
