# Deadlines

The public board at `https://papers.aesthetic.computer/deadlines/` serves reviewed new-media opportunities. Its JSON catalog, RSS feed and calendar are static assets on the existing Papers host.

Edit `system/public/papers.aesthetic.computer/deadlines/opportunities.json`, then run:

```sh
node utilities/deadlines/build.mjs
node --test utilities/deadlines/core.test.mjs
node utilities/deadlines/build.mjs --check
node utilities/deadlines/check-browser.mjs https://papers.aesthetic.computer/deadlines/
```

Commit the catalog and generated `index.html`, `feed.xml` and `calendar.ics` together. Publish through the repository's standard lith deployment. `template.html` owns the page shell; `core.mjs` shares validation and formatting between the browser and builder.

Use Exa to discover candidates, then verify the official call before adding one. Keep stable IDs across edits. Record the review date, fee, funding, eligibility, requirements, material rights or travel conditions, and source. Unknown fees never count as free. Private application notes, draft locations and applicant statuses do not belong in this catalog; the schema rejects unapproved fields and local paths. No search service runs in the visitor's browser, and no automatic scouting schedule is configured.

Only set `deadline.at` when the organizer's date, time and zone establish an unambiguous instant. Otherwise set it to `null`, retain the stated date, and explain the uncertainty in `deadline.note`. Calendar exports make these all-day entries marked “check time.” Date-only entries stay visible through the source date in every time zone, then archive; this is a display grace period, not an extension. A deadline can have `rolling: true` for rolling review with a final closing date. Use `deadline: null` for an open-ended call.

The browser refreshes expiry on load and when the page becomes visible. Exact cutoff times display in the visitor's time zone. RSS retains publication dates and stable permalinks; calendar events retain stable UIDs, so corrections update subscriptions instead of duplicating events. Closed entries remain in the source catalog and are reachable in Archive. The generated HTML is a dated fallback when JavaScript or the catalog request is unavailable.

The former personal tracker is archived privately in the adjacent vault under `grants/deadlines-personal/`.
