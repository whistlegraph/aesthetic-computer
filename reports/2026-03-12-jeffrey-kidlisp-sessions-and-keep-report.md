# @jeffrey KidLisp Sessions + Keep Priorities Report (Data-Driven)

Generated: 2026-03-12T01:27:44.286Z

## Scope

- Pull all attributed `@jeffrey` KidLisp rows from Mongo-backed API and analyze source, hits, keep status, and time sequence.
- Reconstruct creation sessions from publish timestamps.
- Cross-reference session windows against monorepo git commit history.
- Identify what is most compelling, what is not yet kept but likely should be, and what anonymous rows are candidates for attribution patching.

## Data Sources

- `GET https://aesthetic.computer/api/store-kidlisp?recent=true&limit=100000&sort=recent&handle=@jeffrey`
- `GET https://aesthetic.computer/api/store-kidlisp?recent=true&limit=100000&sort=hits&handle=@jeffrey`
- `GET https://aesthetic.computer/api/store-kidlisp?recent=true&limit=20000&sort=hits` (global scan for anonymous recovery)
- `git log --since=<first piece - 1 day> --until=<latest piece + 1 day>` for session context

## Assumptions

- Session boundary: new session if gap between publishes is greater than **2 hours**.
- Times shown below are **America/Los_Angeles** for readability.
- “Should keep” ranking is based on family-level traction (hits), recurrence (variants), and feature richness.

## Dataset Snapshot

- Attributed handle: @jeffrey
- Pieces analyzed: **3701**
- Sessions reconstructed: **170**
- Total hits across attributed pieces: **110,384**
- Kept pieces: **27** (0.73%)
- Unique functions/tokens observed: **115**
- Active days: **114**
- Date range: **2025-07-24T06:12:17.631Z** to **2026-03-12T00:33:19.697Z**
- Git commits scanned in range: **4,619**

## Session Structure

- Total publish-active time (sum of session spans): **89.31h**
- Median session span: **0.04h**
- Median pieces/session: **5**
- Average pieces/session: **21.77**
- Sessions with >=50 pieces: **19**
- Sessions with >=100 pieces: **5**

### Theme Mix

| Inferred session focus | Sessions | Pieces | Hits |
|---|---:|---:|---:|
| shape/drawing construction session | 89 | 2792 | 78,899 |
| palette + compositor tuning | 33 | 530 | 11,906 |
| motion + filter/compositor experiments | 27 | 253 | 15,866 |
| remix/embed composition | 11 | 107 | 3,404 |
| mixed exploration | 10 | 19 | 309 |

### Most Used Features (Hit-Weighted)

| Feature | Pieces using it | Weighted hits |
|---|---:|---:|
| ink | 3,077 | 87,159 |
| scroll | 2,439 | 73,091 |
| zoom | 2,009 | 60,669 |
| timing | 1,854 | 53,166 |
| line | 1,152 | 39,543 |
| blur | 1,185 | 35,184 |
| spin | 973 | 32,389 |
| contrast | 582 | 31,020 |
| fade | 711 | 21,382 |
| box | 1,047 | 19,445 |
| circle | 529 | 18,391 |
| flood | 473 | 16,348 |
| repeat | 286 | 14,501 |
| embed | 53 | 11,032 |

## Sessions: What They Were For

### Top Output Sessions (by piece count)

| Session | Start (PT) | End (PT) | Pieces | Hits | Keeps | Dominant ops | Inferred purpose |
|---:|---|---|---:|---:|---:|---|---|
| 160 | 02/27/2026, 22:02 | 02/28/2026, 02:05 | 490 | 5,727 | 0 | ink, scroll, *, zoom | shape/drawing construction session |
| 162 | 02/28/2026, 18:38 | 02/28/2026, 22:39 | 296 | 6,827 | 0 | ink, *, scroll, zoom | shape/drawing construction session |
| 41 | 08/16/2025, 21:42 | 08/16/2025, 23:17 | 179 | 384 | 0 | ink, box, ?, zoom | shape/drawing construction session |
| 94 | 10/20/2025, 15:17 | 10/20/2025, 20:02 | 129 | 4,084 | 2 | ink, line, mask, scroll | shape/drawing construction session |
| 34 | 08/15/2025, 11:29 | 08/15/2025, 13:59 | 104 | 157 | 0 | ink, line, timing, zoom | shape/drawing construction session |
| 44 | 08/17/2025, 18:08 | 08/17/2025, 19:15 | 94 | 2,853 | 1 | ink, ?, scroll, timing | shape/drawing construction session |
| 141 | 01/28/2026, 19:19 | 01/28/2026, 22:07 | 83 | 1,172 | 0 | ink, box, line, scroll | shape/drawing construction session |
| 23 | 08/08/2025, 21:05 | 08/08/2025, 21:23 | 78 | 3,015 | 1 | ink, scroll, timing, blur | palette + compositor tuning |
| 38 | 08/16/2025, 11:15 | 08/16/2025, 12:01 | 77 | 4,197 | 1 | line, flood, zoom, timing | shape/drawing construction session |
| 51 | 08/24/2025, 20:42 | 08/24/2025, 22:49 | 76 | 6,618 | 1 | fade, ink, line, zoom | shape/drawing construction session |

### Top Impact Sessions (by hit volume)

| Session | Start (PT) | Pieces | Hits | Leading outputs | Keep count |
|---:|---|---:|---:|---|---:|
| 58 | 08/30/2025, 00:43 | 8 | 7,797 | $cow (5546), $woww (1935), $i1w (164) | 2 |
| 162 | 02/28/2026, 18:38 | 296 | 6,827 | $a6k (78), $jxo (73), $x8p (69) | 0 |
| 51 | 08/24/2025, 20:42 | 76 | 6,618 | $roz (6518), $wy2 (13), $p2l (4) | 1 |
| 42 | 08/17/2025, 10:09 | 37 | 6,297 | $r2f (4304), $wwi (1738), $wwa (197) | 1 |
| 160 | 02/27/2026, 22:02 | 490 | 5,727 | $d7d (70), $ung (67), $yoz (66) | 0 |
| 45 | 08/18/2025, 19:54 | 52 | 4,971 | $39i (4910), $cif (5), $faic (5) | 1 |
| 38 | 08/16/2025, 11:15 | 77 | 4,197 | $woe (2299), $inc (1809), $law (5) | 1 |
| 94 | 10/20/2025, 15:17 | 129 | 4,084 | $faim (2132), $kl1 (638), $xkq (342) | 2 |
| 149 | 02/13/2026, 15:22 | 20 | 4,075 | $xom (2138), $wcd (163), $4s6 (163) | 1 |
| 134 | 01/20/2026, 10:19 | 9 | 3,413 | $nrb (2493), $d5z (185), $6c4 (168) | 0 |

### Recent Sessions with Monorepo Context

| Session | Start (PT) | Pieces | Hits | Dominant ops | Nearby commit context |
|---:|---|---:|---:|---|---|
| 160 | 02/27/2026, 22:02 | 490 | 5,727 | ink, scroll, *, zoom | ebef1124 perf(kidlisp): reduce aux mic latency with low-latency audio hints |
| 162 | 02/28/2026, 18:38 | 296 | 6,827 | ink, *, scroll, zoom | 0b3d2eb2 fix(kidlisp): skip preview health check when popout is active |
| 154 | 02/23/2026, 21:14 | 73 | 1,319 | ink, down, crawl, right | b0373ac1 feat(fedac): implement all short+medium+initrd roadmap items |
| 165 | 03/04/2026, 14:04 | 21 | 1,387 | ink, line, fade, wipe | c1062404 feat(native): add ac-native sub-second boot runtime |
| 163 | 03/04/2026, 07:18 | 35 | 1,204 | ink, blur, scroll, circle | e62e2e0e fix(kidlisp.com): stage mode — zero margins on Monaco source text |
| 166 | 03/06/2026, 12:39 | 9 | 1,393 | ink, line | 8a859751 feat(kidlisp): Ctrl+]/[ number adjust, stage mode white outline & larger font |
| 170 | 03/11/2026, 17:18 | 68 | 411 | ink, down, timing, crawl | 9a75168e add mail MCP server, keeps market tooling, and misc updates |
| 155 | 02/24/2026, 10:47 | 6 | 663 | ink, circle, once, suck | 9a5050ff Add streaming upload status via SSE track-media-stream endpoint |

## Coolest Piece Pick

**Pick:** `$roz`

- Hits: **6,518**
- Created: **2025-08-25T05:49:46.495Z** (08/24/2025, 22:49 PT)
- Kept: **yes**
- Function richness: **10** distinct ops
- Variant family size: **4**
- Session context: #51 (shape/drawing construction session), 76 outputs in that session

Why this one stands out:
- It is both culturally validated (very high hit count) and formally rich (palette progression + geometry + motion + contrast shaping).
- It captures a full “KidLisp grammar stack” in short source: color orchestration, controlled camera movement, rhythmic modulation, and circular focal structure.
- It already survives as a keep, making it a strong anchor piece for the broader set.

Source:

```lisp
fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)
```

## Not Kept Yet: What Should Probably Be Kept Next

Ranking method: high family traction + repeat exploration + feature richness.

| Priority | Representative | Family hits | Variants | Rep hits | Created (PT) | Why keep it |
|---:|---|---:|---:|---:|---|---|
| 1 | `$weus` | 1,943 | 4 | 1,885 | 10/20/2025, 11:11 | High-signal single with clear visual identity. |
| 2 | `$woe` | 2,302 | 2 | 2,299 | 08/16/2025, 11:20 | Strong audience pull and recurring motif family. |
| 3 | `$nrb` | 3,159 | 5 | 2,493 | 01/20/2026, 12:50 | Strong audience pull and recurring motif family. |
| 4 | `$noc` | 1,901 | 3 | 1,898 | 07/29/2025, 15:20 | High-signal single with clear visual identity. |
| 5 | `$xkq` | 575 | 14 | 342 | 10/20/2025, 18:06 | Large iterative family worth preserving as a lineage marker. |
| 6 | `$1fi` | 1,975 | 2 | 1,974 | 08/13/2025, 08:55 | High-signal single with clear visual identity. |
| 7 | `$zba` | 1,327 | 4 | 1,324 | 07/27/2025, 17:30 | High-signal single with clear visual identity. |
| 8 | `$mtz` | 1,765 | 1 | 1,765 | 12/18/2025, 17:10 | High-signal single with clear visual identity. |

## Anonymous-Era Attribution Recovery

- Anonymous rows found in global pull: **4415**
- Exact source matches to attributed Jeffrey pieces: **4**
- Canonical matches (number/placeholder normalized): **349**
- High token-similarity matches: **665**

### High-Confidence Auto-Patch Set (Exact Source Match)

| Anonymous code | Hits | Matching Jeffrey code | Time delta (h) |
|---|---:|---|---:|
| `$r53` | 26 | `$fm3` | 0 |
| `$faz` | 21 | `$naee` | 0.28 |
| `$q4y` | 1 | `$m5s` | 0 |
| `$8ed` | 1 | `$mzm` | 0 |

### High-Impact Medium-Confidence Candidates (Manual Review)

| Anonymous code | Hits | Matched Jeffrey code | Match type | Notes |
|---|---:|---|---|---|
| `$bop` | 11,597 | `$pi5` | canonical | Content-equivalent pattern; verify lineage before patch. |
| `$ceo` | 5,757 | `$puf` | canonical | Content-equivalent pattern; verify lineage before patch. |
| `$4bb` | 4,834 | `$qwn` | canonical | Content-equivalent pattern; verify lineage before patch. |
| `$otoc` | 2,653 | `$eys` | canonical | Content-equivalent pattern; verify lineage before patch. |
| `$bels` | 2,580 | `$bils` | canonical + time-close | Strong timing alignment. |

### Time-Close Canonical Matches (Great for Batch Review)

| Anonymous code | Hits | Jeffrey match | Time delta (h) |
|---|---:|---|---:|
| `$bels` | 2580 | `$bils` | 0.72 |
| `$qwj` | 73 | `$2rv` | 0.01 |
| `$iyb` | 70 | `$ncly` | 4.17 |
| `$yjh` | 66 | `$okt` | 0 |
| `$764` | 65 | `$2rv` | 0.01 |
| `$xg3` | 61 | `$89z` | 0 |
| `$yos` | 57 | `$2rv` | 0.01 |
| `$hy5` | 50 | `$ung` | 0 |
| `$b7e` | 47 | `$t2f` | 0 |
| `$ta5` | 46 | `$hjz` | 0.01 |
| `$jcs` | 45 | `$89z` | 0.01 |
| `$kh5` | 45 | `$2rv` | 0.01 |
| `$kau` | 45 | `$4c3` | 0 |
| `$vq9` | 42 | `$okt` | 0 |
| `$98k` | 42 | `$vgg` | 0 |

## Recommendation: Attribution Patch Workflow

1. Patch exact matches first (safe auto-apply).
2. For canonical matches, approve only if at least one condition holds: time-close, same motif family in same session period, or manual source check.
3. Store patch provenance in document metadata (`attributionPatchedAt`, `attributionPatchedBy`, `attributionEvidence`).
4. Run post-patch diff report: newly attributed piece count, added hits to profile totals, and top recovered classics.

## Report Artifacts

- Session analysis JSON: `2026-03-12-jeffrey-kidlisp-sessions-analysis.json`
- Attribution candidates JSON: `2026-03-12-jeffrey-kidlisp-attribution-candidates.json`
- Attribution candidates with thumb/still URLs: `2026-03-12-jeffrey-kidlisp-attribution-candidates-with-media.json`
- Canonical thumb/still gallery: `2026-03-12-jeffrey-kidlisp-canonical-thumb-gallery.md`
- This markdown report is derived from these analysis artifacts.
