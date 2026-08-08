# Sleuth

Sleuth is the research platter's source-and-search index: a compact record of
where a lead can be searched, which mechanism reaches it, what the browser or
collection skill contributes, and what evidence should be retained.

This is an index, not a crawler. Search results are leads; a source is only
confirmed after opening the primary page or file and recording its stable URL,
identifier, title, creator, date, access state, and the claim it supports.

## Routing

1. Start with `sources.json` and choose the narrowest source that can answer
   the question.
2. Search the source directly when a general web search is incomplete. This is
   essential for sparse video pages, old archives, and private collections.
3. Use browser inspection for pages whose useful state is visual or interactive.
4. Keep private-vault paths and tokens out of published notes. Record the
   public citation and a local provenance note instead.
5. Add the stable identifier to the working paper bibliography or evidence log.

## Source index

| Source | Mechanism | Skill / collection move | Evidence to keep |
| --- | --- | --- | --- |
| Vimeo | Direct URL/ID, Vimeo search, uploader pages, web search fallback | `browser-use:browser`; inspect title, uploader, description, player metadata, and numeric ID | canonical Vimeo URL, video ID, uploader, duration, capture date |
| YouTube | YouTube search, Data API when authorized, channel/playlist pages | YouTube pipeline plus `browser-use:browser` for Studio/player state | watch URL or video ID, channel, title, upload date |
| Internet Archive | Advanced Search, item pages, files/metadata API | browser + `ffprobe`/OCR/transcription for media | item identifier, file name, checksum or archive URL |
| Dropbox vault | Dropbox API search and temporary links; never publish credentials | vault read, filename variants, media metadata, local inspection | Dropbox path, file name, size/date, local provenance; no token |
| Web archives | Wayback/CDX and page snapshots | browser; compare capture dates and linked media | archived URL, capture timestamp, original URL |
| Scholarly indexes | Crossref, OpenAlex, Semantic Scholar, library catalogs | DOI/title/author disambiguation, then primary-source retrieval | DOI or catalog ID, edition, publisher, access date |
| Institutional archives | University, museum, festival, and venue pages | browser; event-program and embedded-media inspection | institution URL, event date, venue, speaker, media link |
| Papers platter | `paper_list`, `paper_find`, `paper_read`, local manifests | consult SCORE, sub-platter README/manifest, prior bibliography | consulted files, revision/date, provenance gap |

## Video-specific rule

Do not infer identity from a generic title. For a video such as a page titled
“GOODIEPAL”, search by the numeric Vimeo ID, uploader, related event, and local
archive filenames (`Goodiepal.mp4`, `lecture`, `radical`, venue names). A short
clip and a longer source should be linked as separate evidence items.

## Browser and collection skills

- `browser-use:browser` is the visual/browser inspection route for live pages,
  local previews, screenshots, and interactive state.
- The web search connector is a discovery route, not a completeness guarantee.
- Dropbox and other vault connectors are private collection routes; they may
  locate evidence but do not authorize publishing private material.
- Media inspection uses the repository QoS shims (`ffprobe`, `ffmpeg`) and
  preserves the original file path or stable remote identifier in notes.

See [`sources.json`](sources.json) for machine-readable routing data.
