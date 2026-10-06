# fuser-gun

Scout public Instagram posts for a Fuser ICP brief. Exa public search is the default;
Business Discovery is optional. No Fuser credits, board builds, outreach or Instagram writes.
Node builtins only.

```sh
node fuser-gun/ig-gun.mjs scout areaware
node fuser-gun/ig-gun.mjs brief areaware --product "Cache Box" --rank 001
node fuser-gun/ig-gun.mjs rank areaware
```

`scout` saves indexed evidence, attributable post links, available thumbnails and rankings
under `out/<handle>/`. It rejects other accounts merely mentioning the target. Search is
incomplete and can be stale: follower counts, engagement, post dates and exact media types
remain unknown. Thumbnail links can expire. A visual check must confirm the account,
product, complete crop and absence of people before generation.

`brief` writes `brief.md`, `script.txt`, `email.md`, `board-spec.json`, `icp-queue.line`,
`provenance.json` and dossier-shaped `data/`. These are proposals, not completed work.
Missing reference images and unverified product guesses are recorded. A fresh successful
scout removes drafts derived from the previous scout; regenerate them with `brief`.

## Sources

Exa uses its [keyless, rate-limited MCP endpoint](https://exa.ai/docs/get-started/exa-mcp).
No token or account setup is needed for this route. Search errors never substitute fixtures.

```sh
node fuser-gun/ig-gun.mjs scout areaware --limit 12 --top 4
node fuser-gun/ig-gun.mjs scout areaware --query 'Areaware Instagram Cache Box product'
node fuser-gun/ig-gun.mjs scout areaware --no-media
node fuser-gun/ig-gun.mjs scout areaware --search-results results.json
```

Saved search input uses `{ "provider": "exa", "query": "...", "retrieved_at": "...",
"results": [{ "url": "...", "title": "...", "text": "...", "image": "..." }] }`.
The source may be another public search tool; preserve its name and retrieved evidence.
A target handle in a post URL, indexed author or title establishes a search candidate.
Search metadata is not independent confirmation of authorship.

`--source graph` uses Facebook Login Business Discovery. It reads
`<PREFIX>_FB_IG_USER_ID` and `<PREFIX>_FB_TOKEN` from the environment or
`vault/<account>/instagram.env`; aliases are aesthetic, whistlegraph, oskiewar and menuband.
`--as <alias>` chooses the reader. Existing Instagram Login tokens do not supply this edge.
An explicit `--host instagram` remains available for diagnostics. Credentials are never
printed or saved in output. Graph setup is unnecessary for Exa scouting.

`--fixture` explicitly selects the offline Areaware or Olivia Rodrigo demo. Fixture captions
are authored examples, not real post captions; permalinks and counts are absent.
The Olivia fixture ranks merchandise above portrait/concert examples.

## Evidence and handoff

- `search.json` and `search-excluded.json`: retrieved results and rejection reasons.
- `profile.json`, `media.json`, `ranking.json`, `api.json`: normalized data, ranking and source.
- `media-index.json`, `media.csv`: download status, source links, dimensions and hashes.
- `provenance.json`: the brief's reference and remaining visual check.

The board spec follows the existing ICP lane: credited reference, sibling image, blank
carrier, views, Meshy model, staged shot and Kling push-in. The generated budget is an
estimate; the worker reads live prices before generation. Keep lettering separate from the
object and do not use people or minors as generation inputs. Public images remain their
owners' work; retain attribution. `out/` is ignored by git.

Model selection starts with the default **most-used** order at
[Fuser Models](https://fuser.studio/models), as Hirad confirmed through Jeffrey on
October 5, 2026. The worker refreshes the catalog before building, then selects compatible
models by reference fidelity, output needs and live price. Record the observed family
order, check date, exact variant and reason in the spec. Family popularity does not rank
its variants; the new-models section is separate. Image specs explicitly select Nano
Banana Pro instead of inheriting the node's default model.

Brief flags: `--hero <rank>`, `--product "Name"`, `--voice stock|jeffrey`, `--rank <n>`.
`IG_GUN_OUT` selects a separate output directory for isolated checks.

```sh
node --test fuser-gun/public-search.test.mjs
node fuser-gun/ig-gun.mjs scout areaware --fixture --no-media
```

The public-search tests check attribution, unknown metadata, source provenance, rate-limit
failure and refusal to replace live results with fixtures.
