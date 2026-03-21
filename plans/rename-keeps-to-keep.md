# Rename: keeps.kidlisp.com → keep.kidlisp.com

**Goal:** Rename the subdomain from `keeps.kidlisp.com` to `keep.kidlisp.com` across the entire monorepo. Keep `keeps.kidlisp.com` alive as a 301 redirect.

---

## 0. Infrastructure (manual — do first)

### Cloudflare DNS (kidlisp.com zone)

The `kidlisp.com` zone is on Cloudflare. The existing `keeps` CNAME points to Netlify.

- [ ] **Add CNAME**: `keep.kidlisp.com` → `aesthetic-computer.netlify.app` (proxied)
  - Can use: `node at/scripts/cloudflare-dns.mjs add-subdomain keep aesthetic-computer.netlify.app`
    (script defaults to `aesthetic.computer` zone — need to pass `kidlisp.com` zone or do it in Cloudflare dashboard)
  - Or: Cloudflare dashboard → kidlisp.com → DNS → Add Record → CNAME `keep` → `aesthetic-computer.netlify.app`
- [ ] **Keep old CNAME**: `keeps.kidlisp.com` stays alive (needed for 301 redirect)

### Netlify custom domains

- [ ] **Add domain**: Netlify dashboard → Site → Domain management → Add `keep.kidlisp.com`
- [ ] **SSL**: Netlify auto-provisions Let's Encrypt cert once DNS resolves
- [ ] **Verify**: `curl -I https://keep.kidlisp.com` returns 200 (after DNS propagation)

### Cloudflare SSL mode

- [ ] Confirm SSL mode is "Full (strict)" for kidlisp.com zone — required for Netlify + Cloudflare proxy to work without redirect loops

---

## 1. Netlify routing — `system/netlify.toml`

### Rename existing rules (keeps → keep)

| Line | Current | Action |
|------|---------|--------|
| 566 | `# keeps.kidlisp.com/$code — social crawler…` | Comment → `keep.kidlisp.com` |
| 1375 | `to = "https://keeps.kidlisp.com/"` | → `keep.kidlisp.com` |
| 1408 | `to = "https://keeps.kidlisp.com/"` | → `keep.kidlisp.com` |
| 1450 | `# keeps.kidlisp.com subdomain` | Comment → `keep.kidlisp.com` |
| 1452 | `from = "https://keeps.kidlisp.com"` | → `keep.kidlisp.com` |
| 1457 | `from = "https://keeps.kidlisp.com/api/*"` | → `keep.kidlisp.com` |
| 1462 | `from = "https://keeps.kidlisp.com/technology"` | → `keep.kidlisp.com` |
| 1467 | `from = "https://keeps.kidlisp.com/*"` | → `keep.kidlisp.com` |
| 2272 | `# Local dev: …/keeps.kidlisp.com/` | → `keep.kidlisp.com` |
| 2273 | `from = "/keeps.kidlisp.com/*"` | → `keep.kidlisp.com` |
| 2278 | `from = "/keeps.kidlisp.com"` | → `keep.kidlisp.com` |

### Update path redirects into the new subdomain

| Line | Current | Action |
|------|---------|--------|
| 1374 | `from = "https://kidlisp.com/keeps"` | Keep as-is (old path still redirects) |
| 1375 | `to = "https://keeps.kidlisp.com/"` | → `https://keep.kidlisp.com/` |
| 1407 | `from = "https://www.kidlisp.com/keeps"` | Keep as-is |
| 1408 | `to = "https://keeps.kidlisp.com/"` | → `https://keep.kidlisp.com/` |

### Add backwards-compat 301 redirects (new rules, place BEFORE the keep.kidlisp.com rewrite rules)

```toml
# Legacy keeps.kidlisp.com → keep.kidlisp.com redirect
[[redirects]]
from = "https://keeps.kidlisp.com/*"
to = "https://keep.kidlisp.com/:splat"
status = 301
force = true
[[redirects]]
from = "https://keeps.kidlisp.com"
to = "https://keep.kidlisp.com/"
status = 301
force = true
```

**Order matters**: The 301 redirects for `keeps.kidlisp.com` must come BEFORE the 200 rewrite rules for `keep.kidlisp.com`, otherwise Netlify will try to serve the old domain directly.

---

## 2. Edge function — `system/netlify/edge-functions/keeps-social.js`

| Line | Current | Action |
|------|---------|--------|
| 1 | Comment: `keeps.kidlisp.com/$code` | → `keep.kidlisp.com` |
| 11 | `host.includes('keeps.kidlisp.com')` | → `keep.kidlisp.com` |
| 38 | `` permalink = `https://keeps.kidlisp.com/…` `` | → `keep.kidlisp.com` |

---

## 3. Local dev routing — `system/netlify/functions/index.mjs`

| Line | Current | Action |
|------|---------|--------|
| 181 | Comment: `Serve keeps.kidlisp.com locally` | → `keep.kidlisp.com` |
| 182 | `event.path.startsWith("/keeps.kidlisp.com")` | → `keep.kidlisp.com` |

---

## 4. Frontend HTML — `system/public/kidlisp.com/`

### `keeps.html`
| Line | Current | Action |
|------|---------|--------|
| 3 | `<!-- URL: keeps.kidlisp.com -->` | → `keep.kidlisp.com` |
| 12 | `og:url content="https://keeps.kidlisp.com"` | → `keep.kidlisp.com` |
| 4240 | `hostname === 'keeps.kidlisp.com'` | → `keep.kidlisp.com` |
| 4347 | `` permalink = `https://keeps.kidlisp.com/…` `` | → `keep.kidlisp.com` |
| 5600 | `hostname === 'keeps.kidlisp.com'` | → `keep.kidlisp.com` |
| 5699 | `hostname === 'keeps.kidlisp.com'` | → `keep.kidlisp.com` |

### `buy.html`
| Line | Current | Action |
|------|---------|--------|
| 585 | `href="https://keeps.kidlisp.com"` + inner spans spell "keeps" | → `keep.kidlisp.com` + update spans to spell "keep" (drop one `<span>`) |
| 599 | `KEEPS_URL = 'https://keeps.kidlisp.com'` | → `keep.kidlisp.com` |

### `keeps-tech.html`
| Line | Current | Action |
|------|---------|--------|
| 136 | `← keeps.kidlisp.com` link | → `keep.kidlisp.com` |
| 186 | `keeps.kidlisp.com` link | → `keep.kidlisp.com` |

### `wallet/index.html`
| Line | Current | Action |
|------|---------|--------|
| 247 | `keeps.kidlisp.com, objkt.com` | → `keep.kidlisp.com` |
| 260 | `Back to keeps.kidlisp.com` | → `keep.kidlisp.com` |

---

## 5. Tezos CLI — `tezos/keeps.mjs`

| Line | Current | Action |
|------|---------|--------|
| 110-111 | v11 description + homepage | → `keep.kidlisp.com` |
| 128-129 | v10 description + homepage | → `keep.kidlisp.com` |
| 146-147 | v9 description + homepage | → `keep.kidlisp.com` |
| 163-164 | v8 description + homepage | → `keep.kidlisp.com` |
| 180 | v7 description | → `keep.kidlisp.com` |
| 197 | v6 description | → `keep.kidlisp.com` |
| 214 | v5rc description | → `keep.kidlisp.com/rc` |
| 2231 | fallback homepage | → `keep.kidlisp.com` |
| 4324 | help text example | → `keep.kidlisp.com` |
| 4605 | help text example | → `keep.kidlisp.com` |

---

## 6. Deploy staging — `tezos/deploy-staging.mjs`

| Line | Current | Action |
|------|---------|--------|
| 79 | `description: 'https://keeps.kidlisp.com/rc'` | → `keep.kidlisp.com/rc` |

---

## 7. Wallet extension — `wallet/`

### `extension/manifest.json`
| Line | Current | Action |
|------|---------|--------|
| 35 | `"https://keeps.kidlisp.com/*"` | → `keep.kidlisp.com` |

### `extension/popup/popup.js`
| Line | Current | Action |
|------|---------|--------|
| 135 | `href="https://keeps.kidlisp.com"` | → `keep.kidlisp.com` |

---

## 8. KidLisp CLI — `kidlisp-cli/regenerate-media.mjs`

| Line | Current | Action |
|------|---------|--------|
| 267 | Comment: `keeps.kidlisp.com` | → `keep.kidlisp.com` |
| 385 | Comment: `keeps.kidlisp.com` | → `keep.kidlisp.com` |

---

## 9. Test assertions — `spec/keeps-v6-launch-prep-spec.js`

| Line | Current | Action |
|------|---------|--------|
| 73 | `toContain("description: 'https://keeps.kidlisp.com'")` | → `keep.kidlisp.com` |

---

## 10. Sitemap — `system/public/sitemap.html`

| Line | Current | Action |
|------|---------|--------|
| 1274 | `keeps.kidlisp.com (Mint & Index)` | → `keep.kidlisp.com` |
| 1276 | `keeps.kidlisp.com/` | → `keep.kidlisp.com` |
| 1277 | `keeps.kidlisp.com/api/*` | → `keep.kidlisp.com` |

---

## 11. Docs / TzKT submission — `tezos/TZKT-DAPP-SUBMISSION.md`

| Line | Current | Action |
|------|---------|--------|
| 14 | CTA link `https://keeps.kidlisp.com` | → `keep.kidlisp.com` |
| 41 | App URL `https://keeps.kidlisp.com` | → `keep.kidlisp.com` |

---

## 12. Docs-only (plans, reports, changelogs)

These are historical/reference — update for accuracy but low priority:

- [ ] `plans/keeps-gallery-improvements.md` — 6 refs
- [ ] `plans/keeps-permalink-modals.md` — 11 refs
- [ ] `wallet/PLAN.md` — 7 refs
- [ ] `tezos/V6-LAUNCH-PREP.md` — 1 ref
- [ ] `changelogs/2026-03-18-aesthetic-changes-in-march.md` — 1 ref
- [ ] `reports/2026-03-18-changeloggin-4-all.md` — 1 ref
- [ ] `reports/2026-03-10-keeps-v12-and-25-year-stability-plan.md` — 1 ref
- [ ] `reports/2026-03-12-jeffrey-kidlisp-sessions-analysis.json` — 4 refs (frozen session log, probably skip)

---

## Post-flight (manual)

### On-chain metadata
- [ ] `node tezos/keeps.mjs set-collection-media mainnet --homepage=https://keep.kidlisp.com --description=https://keep.kidlisp.com`
- [ ] Verify on TzKT: `https://tzkt.io/KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB/storage`

### External services
- [ ] **TzKT dApp listing**: Update submission URL if already listed (see `tezos/TZKT-DAPP-SUBMISSION.md`)
- [ ] **Objkt**: Collection homepage updates automatically from on-chain metadata (may take hours)
- [ ] **Auth0**: Check if `keeps.kidlisp.com` is in the allowed callback URLs / allowed origins — add `keep.kidlisp.com` and keep old one temporarily
- [ ] **WalletConnect Cloud**: Project ID `647c41004744244efab53b7c6ef80443` — update allowed domains if keeps.kidlisp.com is registered there

### Verification
- [ ] `curl -I https://keep.kidlisp.com` → 200
- [ ] `curl -I https://keeps.kidlisp.com` → 301 → `keep.kidlisp.com`
- [ ] `curl -I https://keeps.kidlisp.com/\$cow` → 301 → `keep.kidlisp.com/$cow`
- [ ] `curl -A Twitterbot https://keep.kidlisp.com/\$cow` → OG tags with `keep.kidlisp.com`
- [ ] Auth0 login flow works on `keep.kidlisp.com`
- [ ] Beacon wallet connect works on `keep.kidlisp.com`
- [ ] Local dev: `localhost:8888/keep.kidlisp.com/` serves keeps.html

### DNS (permanent)
- [ ] Keep `keeps.kidlisp.com` CNAME alive **indefinitely** — shared links, on-chain metadata on old tokens, search engine indexes all point there
