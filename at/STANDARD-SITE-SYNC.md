# Standard.site Mirror Sync

Shallow-copy selected `computer.aesthetic.*` ATProto records into `site.standard.document`.

## Recommended First-Wave Sources

- `computer.aesthetic.paper` → high-fidelity long-form records (`papers.aesthetic.computer`)
- `computer.aesthetic.news` → headline/body documents (`news.aesthetic.computer`)
- `computer.aesthetic.piece` → canonical piece pages (`aesthetic.computer/<slug>`)

These are the default sources in the script.

## Script

`at/scripts/atproto/backfill-standard-site-documents.mjs`

This script:

- Logs into each user repo with existing ATProto credentials
- Reads selected source records
- Creates `site.standard.document` records with mapped fields
- Deduplicates by `sourceAtUri` (stored on target records)
- Never modifies source records (shallow copy)

## Usage

```bash
# Dry run (recommended first)
npm run at:standard:dry

# Live sync (default sources: paper,news,piece)
npm run at:standard:sync

# Single user
node at/scripts/atproto/backfill-standard-site-documents.mjs --dry-run --user @jeffrey

# Custom sources
node at/scripts/atproto/backfill-standard-site-documents.mjs --dry-run --sources=paper,news,piece,kidlisp

# Limit records per source per user
node at/scripts/atproto/backfill-standard-site-documents.mjs --dry-run --limit 25

# Limit number of users processed (staged rollout)
node at/scripts/atproto/backfill-standard-site-documents.mjs --dry-run --user-limit 10
```

## Notes

- Target collection: `site.standard.document`
- Required target fields are always populated: `site`, `title`, `publishedAt`
- Optional mappings include `path`, `description`, `textContent`, and `tags`
