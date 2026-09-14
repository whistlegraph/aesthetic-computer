# Consultation and evidence

Date: 2026-09-13. Base repository revision: a594f6f9968b78d5ee9ca85ae41f90c377aa372b; working tree contains unrelated ongoing work. This paper and robloxplorer are local additions, not deployed releases.

Consulted before outlining: papers/SCORE.md; https://papers.aesthetic.computer/ (rendered public index); papers/whistlegraph-platter/README.md and its Dropbox manifest index; papers/arxiv-stack-summer-26/stack-summer-26.tex and references.bib; papers/arxiv-ac/ac.tex; papers/arxiv-whistlegraph/whistlegraph.tex and references.bib; papers/arxiv-notepat/references.bib; papers/arxiv-identity/identity.tex. Private archived media were not retrieved or included. The sub-platter's manifest is dropbox-manifest.json rather than manifest.json.

Primary code inspected: toolchain/robloxplorer/{robloxplorer.mjs,robloxplorer.test.mjs,campaign.json,README.md}; system/netlify/functions/{get-painting.mjs,painting-metadata.mjs,piece-log.mjs,print.js}; at/lexicons/computer/aesthetic/painting.json; system/backend/painting-atproto.mjs; lith/server.mjs; session-server/{session.mjs,arena-manager.mjs}; system/public/aesthetic.computer/disks/notepat.mjs; oven/kidlisp-mini/{render.mjs,bundle.mjs}; toolchain/assets/sync-index.mjs; toolchain/instagram/reel-app.mjs; toolchain/youtube/yt.mjs; slab/bin/paper-mcp.mjs.

The four robloxplorer focused tests passed earlier in this conversation. The live publishing branch was mocked, not exercised against Roblox. Account setup separately verified an authenticated GET of Sky Pool's universe. evidence.json records fresh bounded GET probes for this paper; only public metadata and capability status are retained. No raw headers, secret values, private game source, player records, or browser state are included.

Official documentation was consulted through Creator Hub HTML and its advertised /docs/en-us/*.md equivalents on 2026-09-13. The Luau feature Markdown says SavePlaceAsync can persist changes, while the linked engine method reference is sparse. This remains documented but untested; the paper recommends whole-place publishing as the first reproducible release path. Quota descriptions in the generated API reference disagree within the same page; numerical throughput promises are deliberately omitted.

The current Assets guide does not include classic clothing in its content upload types. That is a limitation of the verified route, not a proof that all legacy clothing APIs are unavailable. Clothing creation and sale automation remain unverified. Marketplace policy and fees were read from their current pages; seller eligibility was not inspected and no fees were paid. Roblox's archived place-ci-cd-demo is cited only as a workflow precedent, not a source for current limits.

No artwork is exported by this paper. Its product table is a proposal, not evidence of created Roblox goods. Broad operator key permissions were explicitly requested by the user and were not changed during the study.

Source bundle: paper text, bibliography, reference list, and these consultation notes. The separate evidence JSON remains beside the source but is not embedded in the ZIP. No generated PDF or private data is bundled.
