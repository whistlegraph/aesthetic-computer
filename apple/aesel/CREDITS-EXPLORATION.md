# Aesthetic Computer credits

Superseded product recommendation: use **AC balance** in money with an **Add funds** action. See [AC-BALANCE.md](AC-BALANCE.md) for the current definition. The credit denomination and coin treatment below are historical exploration, not the selected product contract.

Research and proposed contract, September 17, 2026. No credits, purchases, wallets, or tokens have been created by this work.

Start with **AC credits**: non-transferable usage credit attached to an AC account, shared across Aesel and other participating AC tools. Keep the free allowance. Offer Luna by default and Opus as an explicit, more expensive choice. Tez can later buy credits on the web; it need not become the unit users spend on every prompt.

| Direction | User experience | Engineering and product consequence |
| --- | --- | --- |
| AC credits, recommended first | Sign in, receive a small allowance, optionally buy more, see each turn's charge | Server ledger; familiar iOS consumable purchase; no wallet required |
| Tez funds the same credits | Connect a wallet on the web, accept a time-limited quote, pay once | Adds chain verification, exchange-rate quotes, treasury and refund operations; inference still uses the same ledger |
| Tez balance or transferable AC token | Hold or transfer a market-priced asset and spend it | Wallet onboarding, custody or signing, variable purchasing power, app-review and jurisdiction-specific obligations; poor first dependency for making a picture |

“Coins” could be the visual treatment, but call the balance **credits** and disclose its price. Do not promise appreciation, redemption for cash, staking yield, or resale. Transfers, creator payouts, tipping, and Keeps ownership remain separate product decisions.

Existing foundations

- `system/backend/ai-budget.mjs` tracks a daily per-handle token bucket in Mongo `ai-usage`. It checks before inference, increments after inference, and returns an unspent-looking balance marked `unknown` when lookup fails. aesel’s endpoint already rejects that unknown balance; callers must make that decision explicitly. This is useful subsidized-usage telemetry, not a purchased-balance ledger: concurrent calls can overspend, writes can be lost, and tokens from different models have different prices.
- `system/netlify/functions/easel-inference.mjs` and `ask.js` already call that budget module. Integrate metering here on the server, not in the phone's JavaScript.
- `system/netlify/functions/news-toll.mjs` demonstrates authenticated Stripe checkout and signed webhooks. Reuse infrastructure conventions, but implement dedicated credit products and deduplication.
- `tezos/contracts.json`, `tezos/keeps.mjs`, and the Keeps contracts provide an existing Tezos operational context. Keeps are art tokens, not an inference-credit system. Use a dedicated payment receiver rather than borrowing minter or contract-admin keys.

Proposed money contract

Use a disclosed reference denomination: **1 credit = US$0.01 of AC service value**, with 1,000 integer subunits per credit. This is a proposed service denomination, not cash redemption or a promised launch price. Store exact local purchase currency, amount, tax treatment, product, and credited units separately; regional StoreKit prices can differ. Show decimal credits rather than rounding every tiny Luna call to a whole credit.

Purchased credits do not expire. Free promotional grants are separately identified and may reset under clearly disclosed rules. The paid balance survives reinstall and device changes through AC login. A handle is a display name; the ledger belongs to the stable AC account ID, with an audited account-merge procedure.

Illustrative provider costs, verified against the [OpenRouter model catalog](https://openrouter.ai/api/v1/models):

| Exact route | Input / output per million tokens | 10,000 input + 2,000 output |
| --- | --- | --- |
| `openai/gpt-5.6-luna` | $0.20 / $1.20 | $0.0044 |
| `anthropic/claude-opus-5` | $5 / $25 | $0.10 |

These are baseline provider costs, not retail turn prices. Cache writes/reads, long-context pricing, reasoning/output accounting, tools, retries, and multiple agent steps change the bill. Luna's catalog includes a higher price tier above 272,000 prompt tokens. Version the full rate card; never price all input or all models as one token bucket. Set the retail multiplier only after measuring provider costs, store/payment fees, taxes, support, and the free allowance.

In Aesel, show selected and actually served model, estimated range, and a user-controlled maximum per turn. Show the settled charge afterward. Switching to Opus never silently spends the whole balance. No automatic premium fallback. Local tools using a person's own subscription do not consume AC inference credits unless a separate AC service charge is clearly offered.

Ledger and request contract

1. Authenticate; accept an idempotent `turnId`, chosen model, and maximum credit spend. The server fixes the pricing version and allowed model. Do not trust client cost or token counts.
2. Atomically reserve available units before calling the provider. The turn cap covers all model/tool steps, not only its first request. Bound context, output, retries, parallelism, and step count to that reservation.
3. Settle once using server-observed usage and the agreed retail rate. Release the unused reservation. A user cancel still costs provider work already performed; disclose this. Do not charge for a failed-to-start call. Give service-failure refunds through explicit compensating ledger entries.
4. If completion or provider usage is uncertain, retain a bounded pending reservation and reconcile provider request IDs. A sweeper must not simply release an expired reservation while its worker can still generate a bill: cancel/fence the worker first. Resolve unprovable usage under a published customer-friendly policy.
5. Record immutable entries for purchase, grant, reserve, settle, release, refund, and reversal. Deduplicate purchase IDs independently of webhook event IDs. A repeated HTTP request returns its original result, never a second debit.

Suggested Mongo collections: `credit-accounts` (available/reserved projection and version), `credit-ledger` (signed integer deltas and references), `credit-turns` (reservation state, actual model/provider ID, pricing version, usage/cost), and `credit-purchases` (verified payment provenance). Unique indexes cover account plus idempotency key, provider transaction, and terminal settlement. Use transactions for ledger+balance changes; verify deployment supports them before coding. A projection must be reproducible from ledger entries.

Paid inference fails closed if a reservation cannot be confirmed. Retain a separately capped free lane if desired. Add per-account and global concurrency/spend limits, verified-account grant limits, and chargeback holds. Refunds that exceed unspent purchased credit create a recorded debt/hold, not a silently erased charge or negative spendable balance. Store usage metadata and IDs, not prompts, API keys, or wallet secrets in the ledger.

Purchase routes

For iOS and Mac App Store, recommend consumable IAP packs. Apple requires IAP for digital functionality, prohibits expiring purchased credits, and allows multiplatform access under its stated conditions. External-purchase links have US-storefront and other regional exceptions; these do not constitute blanket permission for an embedded crypto unlock flow. Explain shared credits and inference to review. [Apple payment guidelines §§3.1.1–3.1.5](https://developer.apple.com/app-store/review/guidelines/#business), [consumable types](https://developer.apple.com/help/app-store-connect/reference/in-app-purchases-and-subscriptions/in-app-purchase-types/).

Use verified Apple-signed transactions and bind a server-issued UUID to the AC account through `appAccountToken`; validate bundle, environment, product, quantity, and transaction uniqueness. Finish the transaction only after durable credit delivery. Test account switching, pending purchases, duplicate notifications, refunds, reinstall, and balances on another device. Server balance recovery is essential; do not assume StoreKit will restore consumed packs. [Apple server integration](https://developer.apple.com/videos/play/wwdc2025/249/), [account token](https://developer.apple.com/documentation/appstoreserverapi/appaccounttoken).

Web checkout can use Stripe. Credit only a verified successful payment, including delayed-payment handling; never credit from a success redirect. Verify signatures over the raw body, acknowledge durable processing, and tolerate repeated/out-of-order notifications. [Stripe webhooks](https://docs.stripe.com/webhooks).

For a later tez pilot, quote a fixed credit amount and exact integer mutez amount with an expiry and exchange-rate source. Map each payment to one authenticated invoice: do not identify purchasers solely by similar amounts or a pasted operation hash. Verify the network, receiver, payer binding, amount, successful operation/content index, and finality before crediting; deduplicate the operation. Pending wallet submission is not payment. Under/overpayments and expired quotes need explicit recovery paths.

Tez is Tezos's native currency; fees are paid in tez. Estimate each transaction rather than advertising a permanent fee. Tenderbake provides deterministic finality after two additional blocks; wait for chain evidence rather than a fixed timer. Keep inference off-chain, with no prompt text or AC identity in payment metadata. Exchange-rate volatility and treasury conversion remain operational responsibilities. [Tezos glossary](https://docs.tezos.com/overview/glossary), [transaction estimation](https://docs.tezos.com/tutorials/build-your-first-app/sending-transactions), [consensus specification](https://octez.tezos.com/docs/alpha/consensus.html).

Rollout and decisions

1. Measure real Luna/Opus per-turn cost with shadow accounting; keep existing users' access intact. Build the ledger with test grants and concurrency/crash/replay tests.
2. Add balance, model choice, turn cap, and receipts in Aesel. Pilot free credits, then sandbox StoreKit and Stripe, including refunds and interrupted settlement.
3. Choose retail packs and margin from evidence. Submit the purchase flow with review notes before selling credits in the app. Roll out paid packs behind explicit server eligibility and spend caps.
4. Evaluate a web-only tez top-up pilot after fiat operations reconcile reliably. A transferable AC token is a separate proposal, not required infrastructure.

Product choices still open: credit name/art; free daily or first-use allowance; retail prices and margin; whether Opus is available to every funded account; whether credits eventually cover images/audio/rendering; initial storefronts; and whether tez demand merits the extra payment operations. Recommended first scope: **AC credits for remote inference, Luna default, explicit Opus, purchased units without expiry, no transfer/cash-out, StoreKit plus web Stripe**.
