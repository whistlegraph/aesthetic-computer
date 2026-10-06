# Whistlegraph in Roblox

Design study, 5 October 2026. The PDF records the baseline before implementation. Subsequent work renamed the app to [`apple/whistlegraph`](../../apple/whistlegraph) and added a Ware picker and private [`Roblox Room prototype`](../../roblox/rooms/README.md). Local validation is recorded with that implementation; Roblox play and mobile handoff remain unverified. The paper's source hashes and historical internal name describe its original inspection.

Build `whistlegraph-roblox.tex` through Paper MCP (`paper_build`, absolute source path), then run `paper_figure_table_qa_check`, inspect every page and diagram crop, and record the current PDF hash in `aesthetic-eye.json` before running `paper_aesthetic_eye_check`.

Build inside the Aesthetic Computer repository: the shared paper layout, source attachment style, and title fonts are repository dependencies. No bibliography processor is required; `references.tex` is the typeset bibliography and `references.bib` contains the corresponding machine-readable entries.

`api-audit.json` records endpoint-level boundaries. `evidence.json` records source hashes, public-read evidence and the scope of local tests. `example-room.json` illustrates the proposed data format; no runtime consumes it. `notes.md` records the research consultation and limitations.

The PDF embeds one source ZIP. The bundle contains text sources and the explicit audit records; it excludes private archives, credentials, participant data, downloaded documentation, fonts, and generated PDFs. Shared layout dependencies are resolved from the repository rather than duplicated into this directory.

No game, asset, account configuration, paper-site entry, or public release is changed by this study.
