# Private photo platter

Read-only search over Jeffrey's local iCloud Photos library for private creative
context and source-image curation. The code is public; the catalog, thumbnails,
paths, and selections remain local.

```bash
node slab/bin/photo-platter.mjs status
node slab/bin/photo-platter.mjs search "ocean sunset" --limit 12
node slab/bin/photo-platter.mjs sheet "ocean sunset" --limit 12
node slab/bin/photo-platter.mjs songs --limit 12
node slab/bin/photo-platter.mjs approval --limit 6
```

The default state root is:

```text
~/Library/Application Support/Aesthetic Computer/photo-platter/
```

Search reads `Photos.sqlite` and `database/search/psi.sqlite` directly in
read-only mode. It uses Apple Photos' local scene labels, user titles,
descriptions, keywords, activities, and visual-quality scores. It excludes
videos, screenshots, hidden/deleted assets, OCR, faces and people names, and
precise location metadata. Contact sheets are square-crop previews; no photo is
modified. `approval` makes one private PDF per released song plus a combined
review book. That lane keeps direct captures and camera-transfer imports,
removes repeated photo-days and pixel-identical assets, and excludes images
saved from communication apps. It selects for saturation and contrast, and
allows either no detected person or only the library's local “me” cluster;
unverified people/portrait/swimming labels are rejected.

The MCP front door is `slab/bin/photo-mcp.mjs`. `photo_search` returns compact
metadata, `photo_sheet` creates an inspectable local contact sheet, and
`photo_song_candidates` applies [`pop/photo-cover-briefs.json`](../../pop/photo-cover-briefs.json).
An image only enters model context when one of these tools is deliberately
called and its returned path is inspected.
