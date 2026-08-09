# CultureHub LA residency acceptance

Working packet for the September 16–25, 2026 residency.

**The program is two works, billed *Whistlegraph presents*:** the expanded live
spatial version of *Special Sign* (six salvaged laptops, AC Native, notepat) and
*MacNeoPolitan* (three MacBook Neos — indigo, citrus, blush — each played
through Menu Band). Decided August 9, 2026, replacing the earlier single-work
plan. The contrast between salvaged and new hardware is the pitch; keep both
halves.

*MacNeoPolitan* runs **Menu Band**, not AC Native. AC Native is x86_64 UEFI only
(`fedac/native/REPORTS.md`) and cannot boot on Apple Silicon. Menu Band is a
shipping universal macOS app, so no port is required — but no copy should ever
claim AC Native runs on Apple hardware.

## Deliverables

- [x] Resident Artist LOA downloaded
- [x] Artist-page copy drafted
- [x] Event-page copy drafted
- [x] Headshot prepared
- [x] New 3:2 spatial-project image prepared; final selection awaits Jeffrey’s approval
- [x] Technical rider drafted
- [x] Residency schedule drafted
- [x] Stale LOA references reported; offer to re-sign sent
- [ ] CultureHub sends a corrected LOA or confirms the intended language
- [x] Payee confirmed as Jeffrey Scudder
- [x] Individual W-9 mailing address confirmed
- [x] Individual taxpayer identification received privately
- [x] Official IRS Form W-9 completed and signed for Jeffrey Scudder
- [x] Official six-page W-9 correction returned August 3, 2026; CultureHub was
  told to discard the earlier substitute-format attachment
- [x] LOA signed as received and returned August 3, 2026
- [x] Zelle payment contact returned privately August 3, 2026
- [ ] Event dates and equipment inventory confirmed
- [x] Program expanded to two works and billed *Whistlegraph presents*
- [x] Artist-page and event-page copy rewritten for the two-work program
- [x] Embargoed announcement, social, and listing copy drafted (`ANNOUNCE.md`)
- [x] Poster and event-image art direction written (`POSTER.md`)
- [x] Two-work event image generated and alt-texted (`gens/program.png`);
  square and portrait crops still to do
- [x] Portrait chosen: the green-laptop shot, not the plain blue-background one
- [ ] Third MacBook Neo (blush) purchased
- [ ] `MacNeoPolitan` scored and rehearsed on all three machines
- [x] Tech rider rewritten for the two-work program: three MacBook Neos on Menu
  Band, the NTP-clock sync path and its failure mode, guest Wi-Fi for the
  workshop, nine power positions, changeover time, and the `neo` memory risk
- [ ] Decide before load-out: does `neo` perform from a clean boot, or does a
  fourth machine stand by?

Artist-page materials are due August 11, 2026. The working technical deadline is
August 16, one month before the residency begins. Payment details were supplied
privately and must not be committed. Do not use the Aesthetic Inc. W-9: the LOA
and payee are Jeffrey Scudder.

## Embargo

Do not announce publicly before CultureHub's formal resident announcement in
late August or early September 2026.

## Files

- `ARTIST-PAGE.md` — ready-to-send artist copy and image metadata
- `EVENT-PAGE.md` — public-program copy; dates await CultureHub confirmation
- `ANNOUNCE.md` — embargoed press blurb, social posts, and calendar listings
- `POSTER.md` — art direction for the event image and poster
- `brief/` — 7-page internal program brief PDF with the media contact sheet;
  `build.py` + `prep-media.sh` regenerate it (headless Chrome, no LaTeX needed)
- `TECH-RIDER.md` — signal, space, projection, network, and documentation needs
- `SCHEDULE.md` — working production calendar
- `REPLY-DRAFT.md` — text of the August 3 return email
- `assets/` — 3:2 animated headshot, still fallback, and project-image drafts
- `packet/` — reproducible LaTeX/PDF artist-page, event-page, technical-rider,
  and internal send-manifest outputs with their approved visual assets
- `private/` — ignored LOA, W-9, signed forms, and payment information

The valid tax attachment is the completed official IRS Form W-9. The earlier
one-page substitute-format PDF is retained only as an audit record and must not
be reused.
