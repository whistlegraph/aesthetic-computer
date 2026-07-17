# Social Software media index

Generated catalog for the reel source corpus. `index.html` is the browsable contact sheet; `manifest.json` and `manifest.csv` carry the same asset records and blank editorial fields for `canonicalWork`, `status`, and `notes`.

Sources remain in `/Users/jas/Downloads`. Only lightweight JPEG thumbnails are stored here. ZIP photographs are identified by both archive filename and member path. SHA-256 hashes identify exact duplicates.

Rebuild from the current Downloads corpus:

```sh
node marketing/sosoft-reel/index/generate-index.mjs
```
