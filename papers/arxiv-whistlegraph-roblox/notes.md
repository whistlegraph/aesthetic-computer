# Consultation and scope

Date: 5 October 2026. This paper follows the user's proposal to connect Whistlegraph's maker app to a Roblox game that exhibits and plays users' creations, including editing while playing. It is a design study with an API audit, not a report of a shipped integration.

Consulted the repository's SCORE.md and ENVIRONMENT.md; papers/SCORE.md; papers/AESTHETIC-EYE.md; papers/sleuth/README.md and sources.json; the public index at https://papers.aesthetic.computer/; papers/whistlegraph-platter/README.md and the top-level structure of its manifest; the Whistlegraph paper and bibliography; the Robloxplorer paper, bibliography, notes and evidence; and the Pieces Not Programs source. Private archive files were not retrieved. The public index was consulted through its rendered HTML text; no browser screenshot of it is claimed.

Inspected the public whistlegraph.app landing page, the internally named Walkieware Swift shell and web engine, the piece-version ledger, the shared thread backend, and the Whistlegraph mint/pack path. The app directory and plist still use Walkieware. The paper identifies that internal name rather than claiming an installed Whistlegraph binary was tested. The iPhone app was not launched in this study.

Inspected Robloxplorer, the Arena release evidence, Notepat evidence, the asset bridge and its server reader, and the release-message subscriber. Evidence hashes in evidence.json identify the consulted contents independently of unrelated working-tree edits. The new paper does not change those components.

Official Creator Hub pages and their advertised Markdown equivalents supplied the API audit. The Studio MCP guide carries last_updated 2026-10-02T21:48:56Z. Endpoint-specific authentication was inspected: the Places upload operation lists API-key authorization, despite generic reference-page prose mentioning OAuth. The Engine Instances guide is a beta collaborative-editing interface, not a generic public-server control channel. The HttpService reference explicitly limits CreateWebStreamClient to Studio. The Luau task model says physics does not run, embedded scripts do not automatically start, and data-model changes cannot persist; the prior Robloxplorer paper's older save-path discussion is therefore not reused as a current capability claim.

The sources.json working download log was retained temporarily outside the paper. This deliverable contains bibliography URLs and snapshot hashes, not copied documentation. Source wordings are paraphrased; product architecture, revision protocol, evaluation targets, and rollout sequence are proposals.

Four Robloxplorer tests were rerun and passed. Their authenticated upload is mocked. Public GET requests confirmed the account and public Sky Pool listing. The publishing credential was not configured in this checkout, and the Studio MCP binary was absent at its documented path. No authenticated API calls, game creation, configuration changes, paid uploads, Studio MCP executions, or publication took place. The separate current public-manifest probe and its result are recorded in evidence.json; the September bridge ledger remains historical evidence only.

The study does not claim all players can authorize a third-party app: current Roblox OAuth documentation requires a 13+ account. It does not propose alternate pairing as a workaround for that restriction. The first user study is scoped to consenting adults until the intended audience and permissions are resolved.

No participant measurements or revenue estimates are supplied. The three-second median and ten-second 95th-percentile patch-application objectives exclude model generation and asset moderation. They are explicit proposed acceptance targets, not observations.

The embedded source bundle includes only the listed text artifacts. It excludes the public documentation downloads, the private archive manifest, credentials, raw prompts/recordings, unrelated working-tree changes, fonts, and generated PDFs. The repository's shared LaTeX style and font paths remain build dependencies.
