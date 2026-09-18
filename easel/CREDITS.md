# Braincells

The bottom-right selector shows a braincell balance for AC hosted inference, or
Claude/Codex when using that provider's own account. It opens provider selection,
model settings, and Stripe checkout at `pay.aesthetic.computer`.

The initial pack is **$5 USD for 1,000,000 braincells**, shared across supported
hosted models. Purchased braincells have no daily reset or expiry. The free daily
allowance is used first. Claude/Codex subscription usage is separate.

Usage is input/output tokens, with cached reads weighted at 0.1 and cache writes
at 1.25, multiplied by the model's braincell rate. Current rates: Luna 1×, GLM 3×,
Qwen/DeepSeek 2×, Sonnet 15×, Opus 25×, GPT-5.4 13×. Long-context upper bounds at
272,000 tokens use Luna 2× or GPT-5.4 25×. Rates are AC's tariff, not an assertion
of exact provider cost; checked against the [OpenRouter catalog](https://openrouter.ai/api/v1/models)
on September 17, 2026. A request starting within the free allowance remains free.
Image-generation purchases are not included in this release.

`/api/easel-checkout` creates authenticated checkouts and receives signed Stripe
webhooks. The stable AC user ID owns the wallet, not a mutable handle. Server-side
pack, amount, currency, mode, and payment-status checks precede fulfillment. A
single Mongo conditional update grants each Checkout Session once. Return-page
navigation never grants credit. Aesel reconciles pending checkouts after returning
and on its regular poll, using a server-to-server Stripe lookup.

Paid inference reserves a conservative upper bound before contacting OpenRouter
and returns the unused portion when usage arrives. Concurrent calls cannot spend
the same balance. Large contexts can require more available braincells than their
final metered charge. Partial/full Stripe refunds revoke proportional braincells;
spent refunds become wallet debt. No card data or Stripe keys enter the renderer.

Production configuration: `AC_CREDITS_CHECKOUT_ENABLED=true` and a dedicated
`STRIPE_AC_CREDITS_WEBHOOK_SECRET`, plus the existing Stripe private key. Provision
once with `easel/scripts/provision-credits.mjs` on Lith. This registers completed,
async-success, and refund events and writes a private systemd environment file.
Re-deploys preserve that environment file. Existing `luna-1m-v1` checkout IDs remain
valid and grant the same quantity of shared braincells.

Validation: Stripe sandbox card checkout completed; its signed completion replayed
five times granted one pack. Mongo concurrency tests use the isolated
`ac-credit-wallets-test` collection and remove their test accounts. Production
checkout creation succeeds; unauthenticated requests and forged signatures fail.
No live payment was made by the agent.

Operational limits: an abrupt server crash during a paid request can leave a
reservation in `ac-credit-wallets.holds`; reconcile those against provider usage
before returning credit. Disputes require operator review.

## Live preview and local updates

Source saves flow through `/run` → Redis → session-server code channel → the
running AC piece. The embedded preview joins the channel directly, so a new piece
works before public publication. Publishing persists the source in the background;
it never reloads the page. Identical live pushes and in-flight publishes coalesce.
Screenshot capture accepts a bare piece route only when the Electron guest was
explicitly bound to that channel on its initial trusted navigation.

**Restart Agent** checkpoints and replaces the PTY without closing the window or
preview. It carries successful push/publication identities across this in-process
handoff. **Reload Interface** reloads renderer assets within the native window;
this recreates the embedded preview. This installed development build reads UI
assets from `Resources/easel/desktop-ui`, avoiding Electron's cached archive offsets.
Use `desktop/scripts/install-dev-ui.sh` to copy subsequent UI edits there. Main-
process and binary changes still use **Restart App**. An agent restart waits for
active requests and uploads to finish.
