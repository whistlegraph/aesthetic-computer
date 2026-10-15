# Private products

Product implementations live in the separate private `aesthetic-computer-vault`
repository under `products/`. Public AC retains its runtime, KidLisp, creative
libraries, shared identities, media formats, and client protocols.

| Product | Private implementation |
| --- | --- |
| sotce-net | Diary, photo editor, membership application |
| choir | Native app, site, API and curator |
| mime | MIME and the retiring Whistlegraph app, their services and release tools |
| oskiewar | Game, native app shells, game services and authoring tools |
| nopaint | No Paint / NeoPaint web and native apps, inference, billing and product experiments |

`private-products.json` is the path inventory. Whistlegraph's public art archive
is separate from the app. Historical public commits remain in public Git.

No Paint's shared painting runtime, brush libraries, document formats and ACUI
remain in core, along with the classic web export and public gallery. Its new
application, native shells, Construct project and cloud services belong to the
private product. The public site keeps the No Paint name and `nopaint.art` URL.

## Local work

Clone the private vault at `aesthetic-computer-vault/`, then run:

```sh
node toolchain/private-products.mjs link
node toolchain/private-products.mjs check
```

Each private file keeps its original path through an ignored relative symlink.
Edit and test through those paths as before. Commit product changes in the
vault; commit shared changes in AC. The product directories contain ignored
links back to shared dependencies, including installed `node_modules`.

`guard` rejects private paths in the public index, including forced additions.
It runs in the repository pre-commit hook and public CI. On a new seat, install
the pre-commit hook individually; enabling every hook also enables deployment
hooks. The generated Walkieware directory is ignored, not imported as source.

## Releases

Public discovery is curated in `shared/public-products.mjs`: product purpose,
home page, access requirements, and explicitly supported public interfaces.
MIME's anonymous public feed is listed; Sotce Net and Choir advertise their
landing pages without enumerating member APIs. The retiring Whistlegraph app
points to MIME. `network.json`, `/sitemap`, and the public API reference consume
these descriptions. Product `llms.txt` files describe usage without offering
private implementation source.

`/sitemap` does not scan the backend handler registry. Its piece list and
`llms-full.txt` reuse `/docs`' published-piece selection; private inventory
paths are excluded even when deployed as ordinary files. Run the three
discovery generators after changing public entries. Their `--check` modes and
`utilities/public-discovery.test.mjs` run in public CI without the vault.

Discovery is separate from authorization. Lith's operational stats, errors,
requests and traffic require AC admin authorization, and Silo sends its login
token. Before rollout, set a randomly generated `LITH_OPERATIONS_TOKEN` in
Lith's private `system/.env` and restart Lith. The SSH diagnostics collector
loads that same environment and uses this credential for read-only operational
routes; missing credentials produce an unavailable-coverage result. Never put
this token in browser configuration or public manifests. Product handlers
retain their own account and membership checks.
Browser-delivered HTML and JavaScript remain downloadable; private source
ownership does not conceal code that runs in a visitor's browser.

Lith and the session server ship a source-only bundle from a clean vault
revision recorded on its upstream branch. Their preflight also requires the
public deployment revision to contain this inventory and exclude private
source. Both revisions must therefore be committed and pushed before cutover.

The deploy transport snapshots installed product files before resetting public
Git. Rollback restores those files with the previous public commit. Private
source is installed as ordinary files; production needs no vault checkout or
development symlinks. `.commit-ref` and `.products-ref` identify the pair.

```sh
node toolchain/private-products.mjs pack /tmp/products.tgz
# On a prepared checkout, extract the trusted bundle into a temporary directory:
node toolchain/private-products.mjs install /tmp/extracted-products
```

Only inventory-owned files enter a bundle. Credentials, client work, dependency
links and migration records are excluded. `pack --worktree` creates a
`local-test` bundle for isolated testing; do not deploy it.

MIME's release script verifies both served revisions and composes native source
from both repositories. Oskiewar stamps and commits its game changes in the
vault. Oven's `deploy.sh` uploads the private bundle for native and reel build
checkouts; jobs record its private revision. The older `deploy.fish` delegates
to that transport. Native builds and remote deployment still require their
normal release validation.

## First cutover

Prepare the two commits separately. Stage `products/` and the encrypted Lith
environment in the private vault; stage only the inventory-owned removals and
reviewed integration/discovery changes in core. Leave unrelated working-tree
changes out. Use `AC_NO_AUTO_DEPLOY=1` for preparation commits: the post-commit
hook honors it for every deployment path, including Feed and AT.

Before publishing, verify the vault repository is private, run the source
boundary checks, and inspect both diffs. Push the private revision first, then
the public cutover. A `local-test` product bundle is only a rehearsal artifact;
the deploy must build a fresh bundle from the committed, pushed vault revision.

For the release window:

1. Confirm the private Lith environment has `LITH_OPERATIONS_TOKEN`, retains
   the production key set, and passes `node lith/verify-env.mjs aesthetic-computer-vault/lith/.env`.
   The encrypted vault copy must include the new token; decrypt it on the
   releasing seat if using another machine.
2. Run `node toolchain/private-products.mjs release-check HEAD` from the
   reviewed public revision. Set `AC_PRODUCTS_REVISION` to the selected vault
   commit for the release commands so it cannot drift underneath them.
3. Run `ac-deploy` from the reviewed, pushed `main`. Environment preflight runs
   before source changes. The printed rollback directory preserves the prior
   products, public revision (`core.before`), environment (`lith.env.before`)
   and Caddy configuration (`Caddyfile.before`). Keep it through verification.
4. After the knot-to-GitHub mirror contains that same public revision, deploy
   the session server with the same private revision. Update the Silo dashboard
   and proxy together so their admin requests carry authorization.
5. Check `.commit-ref` and `.products-ref`, all five product homepages, Choir's
   `/llms.txt`, the public sitemap/API directory and MIME's documented feed.
   Operational routes must reject anonymous requests while admin and service
   diagnostics continue working. Exercise signed-in flows with a test account.

If verification fails, restore the prior public commit and the saved product
snapshot together using the staged `private-products.mjs install` tool. Restore
the saved environment and Caddy configuration, regenerate precompressed assets,
reload Caddy and restart Lith, then verify the prior public revision and product
pages. An install failure already restores the source pair automatically;
failures later in deployment still require this operator rollback.

DNS delegation repair is independent of this source cutover. Moving source
does not require changing the products' domains or exposing their member APIs.

## Migration record

`products/migration.json` in the vault records the public parent commit, each
file's original SHA-256 and mode, and Oskiewar's prior revision count. It includes
uncommitted work present at migration. Later compatibility edits are ordinary
vault changes; the receipt remains the record of the original bytes.

The local migration stages public deletions so ignored links cannot be
mistaken for public source. It makes no commits. To undo a migration before
further edits, first preserve both repositories' current diffs, copy canonical
files over their compatibility links, then restore only those paths in the
public index. Never reset either whole working tree to undo this change.

Existing license notices and the imported ISC license are retained in the
vault. Moving a file does not remove its public history or prior notices.
