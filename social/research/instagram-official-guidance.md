# Instagram Official Creator Guidance — what's documented vs. myth

Reference doc for `/social`. Compiled 2026-07-08 via the deep-research harness
(103 agents, 25 claims adversarially verified, 21 confirmed / 4 killed).
Everything below rests on Instagram's **own first-party documentation** unless flagged.

Sibling: `SCORE.md` (strategy + tooling index), `accounts.json` (stat snapshots).

**One-line takeaway:** there is **no official "ideal number of Reels" and no official posting-cadence number.** Reach is gated by *originality + eligibility + per-viewer retention*, not by how many Reels sit on your profile. For a new account the levers Instagram actually documents are: post original content, stay recommendation-eligible (check Account Status), keep Reels ≤3 min, and let the cold-start test audience do the work (optionally via Trial Reels).

---

## The official sources (first-party)

- **[creators.instagram.com](https://creators.instagram.com/)** — creator hub: Best Practices, growth pages, originality guidelines
- **[Instagram Ranking Explained](https://about.instagram.com/blog/announcements/instagram-ranking-explained)** — per-surface ranking breakdown
- **[Recommendations & Originality](https://creators.instagram.com/blog/recommendations-and-originality)**
- **[Rewarding Original Creators](https://creators.instagram.com/blog/rewarding-original-creators-on-instagram)** + **[Original Content Guidelines](https://creators.instagram.com/original-content-guidelines)**
- **[Trial Reels announcement](https://creators.instagram.com/blog/instagram-trial-reels)** + Mosseri's [Reel](https://www.instagram.com/mosseri/reel/DDZ15g9vs0v/) (2024-12-10)
- **[The latest with Instagram](https://creators.instagram.com/blog/the-latest-with-instagram)** (Jan 2025 update — 3-min Reels)
- **Account Status** — in-app (Settings → Account → Account Status); [announced here](https://about.instagram.com/blog/announcements/instagram-outages-and-account-status)
- **In-app Best Practices hub** — Professional Dashboard → Best Practices (creation/engagement/reach/monetization/guidelines)

---

## DOCUMENTED (high confidence, verbatim-sourced)

**1. Cold-start distribution.** Verbatim: *"every piece of eligible content… is shown to a small audience that we think will enjoy it, regardless of whether they follow the account,"* and *"the top performing set of reels are shown to a slightly wider audience, then the best of these are shown to an even wider group, and so on."* → the mechanism that gives new accounts non-follower reach.

**2. Reels is mostly a non-follower surface.** *"the majority of what you see is from accounts you don't follow."*

**3. Reels ranking factors, in Instagram's stated priority order:**
   1. Viewer's **own recent activity** (reels they liked / saved / reshared / commented on)
   2. Viewer's **interaction history with you**
   3. **Info about the reel** (audio, visuals, popularity)
   4. **Your account's popularity**

**4. Trial Reels — official cold-start tool.** Shows a reel to **non-followers first** (followers don't see it in Feed or Reels tab). Instagram auto-shares to your followers if it performs well **on views within the first 72 hours**; otherwise sharing is a manual choice. Launched Dec 2024 for public accounts with **1,000+ followers**. Built to "depressurize" posting.

**5. Reel length ≤ 3 minutes** eligible for non-follower recommendation (Jan 2025, up from ~90s). Caveat: eligibility ≠ reach — retention still governs actual distribution.

**6. Originality regime (the primary reach-gate):**
   - Must be **original, guideline-compliant, watermark-free** to reach non-followers.
   - **Duplicates suppressed** — only the original is recommended; copies get a label linking to the original creator.
   - **Low-effort edits don't count as original:** borders, watermarks, speed changes, subtitles, or just crediting.
   - **Aggregator penalty:** post others' un-enhanced content **10+ times in 30 days** → removed from recommendation surfaces (rolling window; regained by dropping below). **Existing followers unaffected.**
   - **Expanded April 2026:** from Reels-only → photos + carousels; cross-platform watermarks (TikTok logo) now count as unoriginal.

**7. Account Status** (in-app) reports whether you're **currently eligible** for recommendation and shows a sample of what's disqualifying you. Best single diagnostic for a new/small account — check it before assuming a "shadowban."

---

## ⚠️ MYTH / UNCONFIRMED (did NOT survive verification — do not rely on)

- ❌ **"Watch time is the single most important Reels signal"** — refuted 0–3. Not verifiable from Instagram's own docs. Watch time matters; the "#1 signal" framing is unconfirmed.
- ❌ **"Sends/DM shares weighted 3–5× likes"** — the specific weights appear in **no** official Instagram document. Mosseri has named sends/likes/watch time in *spoken interviews*; the numbers are blog invention.
- ❌ **"Equal / size-neutral distribution for all creators"** — refuted 1–2. Instagram documents the small-audience-first *mechanism*, but does **not** claim account size is neutral.
- ❌ **The "five weighted Feed signals" list** (time spent, comments, likes, reshares, profile taps) — refuted 1–2.

---

## Open questions (not closeable from official docs)

- **Posting cadence:** Instagram publishes **no official number.** The common "3–5 Reels/week" is all third-party. Trial Reels messaging implies they want to *reduce* posting pressure, not prescribe volume.
- **Search/SEO & Collab:** both features exist and are recommended by Instagram, but this pass found no verbatim primary-source claims quantifying their reach effect. Worth a follow-up pass.
- **Mosseri's named reach signals** (sends per reach, likes per reach, watch time) come from AMAs/interviews, **not** written Instagram documentation — attribute them as spoken remarks.

---

## Practical implication for @aesthetic.computer (1k → Trial Reels gate)

- The 1k-follower goal isn't arbitrary — it's the **Trial Reels eligibility threshold**. Below it, cold-start still runs automatically; above it you get the explicit non-followers-first test tool.
- Keep every Reel **original + watermark-free** (no recycled TikTok exports with the logo) — this is the hard gate, and it now covers photos/carousels too (Apr 2026).
- Use **Account Status** to confirm eligibility before diagnosing low reach as anything else.

_Sources: all links above are Instagram first-party. Re-verify before acting — the originality/aggregator regime changed materially in April 2026 and this area moves fast._
