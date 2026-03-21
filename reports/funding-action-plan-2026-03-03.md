# Funding Action Plan: Bring DigitalOcean Back Online
**Date:** March 3, 2026
**Goal:** Raise ~$400 to reinstate suspended DigitalOcean servers (chat, user media, multiplayer)

---

## What's Already Built & Ready

You have a comprehensive funding stack that's fully operational:

| Asset | Status | URL |
|-------|--------|-----|
| **Give page** (Stripe + PayPal) | Live | https://give.aesthetic.computer |
| **GitHub Sponsors** | Live | https://github.com/sponsors/whistlegraph |
| **Emergency mode on site** | Live | Blinking ticker, crying kid face, GIVE button |
| **Email blast tool** | Built, never sent | `node artery/email-blast.mjs` |
| **18,187 Auth0 users** (5,373 verified) | Cached | Vault file from Jan 5 |
| **Unsubscribe system** | Built | HMAC tokens + Netlify function |
| **Shopify store** | Live | shop.aesthetic.computer |
| **Printful merch** | Live | Stickers, mugs |
| **Botce tickets** ($6/ea) | Live | botce.ac |

---

## Actions for TODAY — ranked by speed-to-money

### 1. Send the email blast (highest reach, ready now)

The tool is fully built with unsubscribe compliance. **5,373 verified users** can receive it today.

```bash
# Update the email copy first — reference DO suspension + $400 specifically
# Then:
node artery/email-blast.mjs --preview          # Check the message
node artery/email-blast.mjs --test YOUR_EMAIL   # Test on yourself
node artery/email-blast.mjs --send              # Send to verified users
```

**Suggested updated subject line:**
> "aesthetic.computer servers are down — we need $400 to come back"

**Suggested updated body focus:**
- DigitalOcean suspended servers
- Chat, user media, multiplayer all offline
- $400 brings everything back
- Link to give.aesthetic.computer + GitHub Sponsors
- "even $5 helps"

**Gmail limit:** ~500/day. At 5,373 verified users = ~11 days to reach everyone. Start today, use `--resume` to continue tomorrow.

**If even 2% of 500 people give $5, that's $50 on day 1.** Across 11 days reaching 5.3k users, you could easily hit $400.

### 2. Social media posts (immediate, free)

Post on every platform you have access to right now:

- **Twitter/X** (@aestheticco_mp or personal)
- **Bluesky** (AT Protocol — you have atproto integration)
- **Mastodon**
- **Instagram**

**Template:**
> Our DigitalOcean servers got suspended. Chat, user media, and multiplayer are all down. We need $400 to bring aesthetic.computer back to full capacity.
>
> give.aesthetic.computer
> github.com/sponsors/whistlegraph
>
> Even $5 helps. Thank you.

### 3. GitHub Sponsors — cross-promote

Your `FUNDING.yml` is set up pointing to `whistlegraph`. Make sure:
- [ ] Tiers are configured (suggested: $5, $10, $25/month + one-time)
- [ ] Description mentions the current emergency
- [ ] The repo README or SCORE.md prominently links to sponsors

The "Sponsor" button shows on every repo page — all your GitHub visitors see it.

### 4. Direct asks to people you know

Personal messages convert 10-100x better than broadcast. Think of:
- People who've used AC at workshops
- Creative coding peers
- Former collaborators
- Anyone who's published pieces on AC

A personal text/DM: "Hey, AC's servers got suspended and I need $400 to bring them back. Can you help? give.aesthetic.computer"

### 5. Creative coding communities (today)

Post in communities where AC has natural interest:

| Community | How |
|-----------|-----|
| Processing Foundation Discord | "Open source creative computing platform needs help" |
| p5.js Discord | Similar audience |
| Creative Coding Discord | Share the situation |
| r/creativecoding | Post with context about AC |
| r/generativeart | Cross-post |
| lines.org (Monome) | AC's instrument metaphor resonates here |

---

## This Week — Larger Moves

### 6. Update the email copy for urgency

The current email says generic "help aesthetic.computer stay online." Update it to match the actual emergency:
- Mention DigitalOcean suspension specifically
- State the $400 amount
- Mention what's broken (chat, media, multiplayer)
- Add GitHub Sponsors as a second link alongside give.aesthetic.computer

### 7. Hacker News "Ask HN" or "Show HN"

> "Ask HN: My creative computing platform's servers got suspended. How do small open-source projects handle hosting costs?"

This frames it as a discussion (HN likes that) while making AC visible. Link to the project.

### 8. Grant applications (nearest deadlines)

From the ORCID report — these are approaching:

| Grant | Amount | Deadline |
|-------|--------|----------|
| **NLnet NGI Zero Commons Fund** | €5K-€50K+ | Apr 1, 2026 |
| **Creative Capital Award** | Up to $50K | Apr 2, 2026 |
| **Spencer Foundation Small Grants** | Up to $50K | Apr 15, 2026 |
| **LACMA Art + Tech Lab** | Up to $50K | Apr 22, 2026 |
| **NEA Grants for Arts Projects** | $10K-$100K | Jul 9, 2026 |

These won't solve the $400 today but are the path to sustainability. **NLnet (Apr 1) is 29 days away** and is a strong fit for AC as open-source infrastructure.

---

## Revenue Opportunities Already Built (Underutilized)

| Revenue Stream | Current State | Quick Action |
|----------------|--------------|--------------|
| **KidLisp Keeps** | $0 revenue (free minting) | Add 2-5 ꜩ minting fee |
| **Sticker processing fee** | $1 margin | Raise to $2.50-3.00 |
| **Botce tickets** | $6/ticket | Promote more visibly |
| **Donation perks** | None | Add supporter badges, early access |

---

## The Math: How $400 Gets Raised

| Scenario | How |
|----------|-----|
| 80 people give $5 | Email blast + social media |
| 40 people give $10 | Direct asks + GitHub Sponsors |
| 16 people give $25 | Community supporters |
| 4 people give $100 | Close contacts / patrons |
| Mixed | Most likely: a few big + many small |

**The email blast alone reaching 500 people today at a 3% conversion rate of $10 average = $150.** Repeat for 3-4 days and you're there.

---

## Immediate Checklist

- [ ] Update email-blast.mjs copy for DO emergency ($400, what's broken)
- [ ] Send test email to yourself
- [ ] Send blast to verified users (`--send`)
- [ ] Post on Twitter/X, Bluesky, Mastodon, Instagram
- [ ] DM 5-10 people you know personally
- [ ] Post in 2-3 creative coding communities
- [ ] Verify GitHub Sponsors tiers are configured
- [ ] Check give.aesthetic.computer loads properly with current emergency message

---

*This plan uses infrastructure that's already built. The email tool, give page, Stripe/PayPal, GitHub Sponsors, and emergency site mode are all live. The main gap is that the email blast has never been sent — that's the single highest-impact action to take right now.*
