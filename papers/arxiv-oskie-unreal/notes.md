# Oskie Files in Unreal Engine — provenance

Written November 6, 2026, at @jeffrey's request for an implementation map and public paper. This is a design, not an implemented Unreal plugin or a benchmark report. Publication means the Aesthetic.Computer papers site, not submission to arXiv.org.

Consultation before drafting:

- papers/SCORE.md, VOICE.md, QUALITY-RUBRIC.md, and AESTHETIC-EYE.md.
- Public Platter at https://papers.aesthetic.computer/, including a rendered browser view on November 6.
- papers/prose-platter/manifest.md for the plain register.
- arxiv-oskiewar/oskiewar.tex, including its inline bibliography; arxiv-oskiewar-store/store.tex for the distinction between running a native game and distributing it.
- arxiv-harness-loop/harness-loop.tex and references.bib for the current paper format and evidence limits; arxiv-keymaps/references.bib for the wider project context. These contextual readings are not used as proof of Unreal capability.
- Public core source: xbox/ENGINE.md; xbox/native-bios/QuickJsEngine.cpp; xbox/live/scene3d.mjs; system/public/oskie/format.mjs, engine-runtime.mjs, index.html; docs/oskie/README.md.
- Official QuickJS documentation and Epic documentation listed in references.bib, accessed November 6. Epic pages identify themselves as Unreal Engine 5.8 documentation; no Unreal target was installed or compiled for this study. The implementation must pin its own tested version.

Reference core revision: f568c690186a4ffca0ced1c9e19133abebe68038. The current working tree is shared; named paths and the recorded revision define the source baseline rather than unrelated local edits.

The engine release discussed is build 462. Engine JavaScript SHA-256: e36e0079cae32e9ed03c34d171a2c039ee779929b382e2a6e11967b53b498e4b. Public engine artifact: https://aesthetic.computer/oskie/engine/oskiewar.js. Runtime release identifier: 1ad4971476cfc921fc115d7d3fd97207a6804cd521ab187ae1a087c4fa90ed75. These identifiers are provenance, not a promise that the current native Xbox build uses the same release.

Archived reference bundle: system/public/oskie/examples/island-crisis-engine-455.oskie, SHA-256 024ff2e2669e883a545bf8e9242b50ac8cbec68531046719d6c49113a43e8ff1. The paper does not redistribute its source or claim an unchanged native run. Browser dependencies must be audited before use as an Unreal acceptance fixture.

The existing native QuickJS host creates a runtime with memory and stack limits and evaluates the engine using JS_EVAL_TYPE_GLOBAL. A bundle-local ES module registry and Unreal resource adapter still need implementation. The public web validator currently strips unrecognized manifest fields, so capability declarations cannot be added only to authoring prompts.

All stages and all numerical acceptance thresholds in this paper are proposed. The 600-tick trace, display rates, aspect ratios, and one-pixel text tolerance are test specifications, not reported measurements. No Unreal plugin files, performance data, or successful Unreal game runs exist as outputs of this writing task.

The diagrams are original vector implementation maps in the TeX source. No decorative cover, private correspondence, private implementation source, account data, or raw tool payloads are included. The narrow bibliography documents source code and vendor APIs; demographic proportions are not inferred for organizational authors. Unrelated citations were not added to satisfy a numerical target.

Rebuild from the repository using the papers source-bundle generator, XeLaTeX, BibTeX, then two more XeLaTeX passes. The shared style and title font resolve from the repository; the attached text bundle is for inspection and editing, not a standalone distribution of those assets. Run Figure-Table-QA-Check and refresh Aesthetic Eye after any rebuild.
