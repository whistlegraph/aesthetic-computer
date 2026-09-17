# AC balance

Product definition · September 17, 2026

**AC balance is prepaid account credit for paid Aesthetic.Computer services. Add funds once, then pay for usage from the same account across aesel and participating AC apps.**

The public name is **AC balance**. The purchase action is **Add funds**. Show money directly; there is no separate coin, point, or credit-to-dollar conversion to learn. “Account credit” describes the prepaid funds, not a new denomination.

This defines the proposed product. Paid billing is not live, and this document does not authorize charging existing users. It supersedes the invented-unit recommendation in [CREDITS-EXPLORATION.md](CREDITS-EXPLORATION.md). The comparative research is in [What to Call a Balance](../../papers/arxiv-ac-credits/ac-credits.pdf).

## What the user sees

| Place | Label or behavior |
| --- | --- |
| Account menu | **AC balance · US$5.00** |
| Purchase action | **Add funds** |
| Balance detail | **Available**, **Pending usage**, **History** |
| Before paid work | Selected model, estimated cost, and **Maximum spend** |
| After paid work | Actual charge and remaining available balance |
| Insufficient funds | **Add funds to continue**; preserve the draft |
| Payment awaiting confirmation | **Payment pending**; do not increase available funds yet |
| Usage awaiting settlement | **Pending usage**; show the reserved amount and request |

The amounts above are interface examples, not pack prices. Use **US$** where the currency could be ambiguous. VoiceOver should read monetary values and model names in full. Balance is accessible from the account menu behind the top-right handle; keep the personalized handle colors and the Aesel home action.

The user can inspect a request's estimate and maximum before spending. A maximum applies to the whole turn, including its permitted tool/model steps and retries. Saving a default maximum is allowed; raising it requires an explicit user action. Show the actual served model in the receipt. A premium model cannot silently replace the chosen model.

## Value and precision

The initial service denomination is **USD**: US$1 of AC balance buys US$1 of services at the published AC tariff. This states service value, not a cash-withdrawal promise. It does not promise that the checkout total equals delivered balance: taxes, regional purchase currency, and any platform-specific offer must be itemized before purchase.

Keep internal amounts as integer millionths of a US dollar: `1 USD = 1_000_000 microUSD`. Never use floating-point values for ledger arithmetic. Calculate fractional metered charges at higher precision and round once per settled turn to the nearest microUSD, half up. Do not round each token, stream event, or tool step into a separate charge.

The compact balance shows two decimal places rounded down so it never overstates available funds. For a positive balance below one cent, show **< US$0.01**. Balance detail and usage receipts expose up to six decimal places, trimming trailing zeros; a nonzero charge must never appear as US$0.00. All arithmetic uses the full stored amount.

Retail pricing is separate from provider cost. Each turn pins a server-owned tariff version before reservation. Tariffs specify rates, billable dimensions, and any disclosed minimum; there is no implicit minimum charge. Price changes apply to future requests. They do not reduce the monetary face value of funds already added.

## Ownership and scope

- The balance belongs to the stable AC account ID, not its handle, device, or Apple account. Changing a handle or reinstalling does not lose funds.
- The same signed-in account sees its eligible balance and history on iPhone, Mac, and web. Do not create a second balance per app or per model. Any required storefront restriction must be disclosed before purchase.
- Initial paid scope is remote aesel inference. Other AC services join only with an explicit price and consent to spend. Ordinary browsing and publishing do not become paid merely because an account has funds.
- Purchased funds do not expire. They are not transferable between users, withdrawable on demand, or a creator-earnings balance. Purchase refunds remain a separate process.
- Optional free allowance is shown separately from paid funds, with any expiry/reset stated. Use eligible free allowance first, then paid funds only when paid use is enabled by the user. Within free grants, consume the earliest expiry first.
- Anonymous exploration may use a separately bounded free allowance. Adding paid funds requires AC sign-in so ownership and cross-device recovery are unambiguous.
- Local work using the user's own provider subscription does not debit AC balance unless a distinct AC service charge is explicitly offered and accepted.

## Add funds

The purchase screen states the monetary service value delivered, checkout currency and total, tax treatment, and payment method. A purchase increases available funds only after server verification and durable delivery. A success page, client callback, or pending payment is not proof of payment.

The intended launch routes are web checkout and native in-app purchase. Their implementation must satisfy the applicable storefront and payment-provider requirements at launch. Purchased value is recorded in the shared account ledger, with the original currency, payment amount, tax data, product, and provider transaction retained separately.

Grant each provider transaction once even when notifications repeat. Bind payment ownership to the signed-in AC account before checkout; switching accounts during payment must not move the purchase to the new account. Provide recovery for interrupted purchase delivery.

**No automatic top-up in the first version.** Reaching the limit stops paid work and preserves the draft. A saved payment method is not consent to replenish the balance.

Tez is outside the initial release. A later tez payment could fund this same monetary balance through a disclosed conversion quote; it would not rename the balance or require users to hold an AC token.

## Spend and settle

1. Authenticate the account. Accept a unique request ID, selected model, and authorized maximum. Resolve the tariff and enforce server model, output, tool-step, retry, and concurrency limits.
2. Atomically reserve the authorized amount from eligible available funds. If there is not enough, stop before starting paid provider work. Concurrent requests cannot reserve the same funds.
3. Run only within the reservation. If continuing requires more, stop and ask for a higher maximum before further paid work. Provider overruns caused by AC's enforcement failure are AC's expense, not an unapproved user debit.
4. Settle exactly once from server-observed usage under the pinned tariff. Convert the consumed reservation into a charge and release the unused portion to its original funding source.
5. Return a receipt containing the request, actual model, tariff version, metered usage, settled amount, and resulting balance. Replaying the request returns its existing state or result, never a second debit.

Available funds exclude active reservations. Pending usage is reserved money, not a second charge. The total remaining value is available plus reserved; unsettled work must not look both spent and available.

Use one atomic ledger/projection mutation for every reserve, settle, release, or funding change. The append-only ledger is the source of truth; cached balances must rebuild from it. Use unique keys for requests, provider payments, and terminal settlements. Paid work fails closed if reservation state cannot be confirmed.

## Cancellation, failure, and refunds

| Event | Balance treatment |
| --- | --- |
| Request fails before provider work starts | Release the whole reservation; no charge |
| User cancels after work starts | Charge verified work already performed, within the cap; release the rest |
| AC/provider failure prevents delivery of a usable result | Refund the affected turn with a compensating entry; AC absorbs its provider cost |
| Output arrives but the user dislikes it | Normal usage charge; no automatic refund solely for preference |
| Worker disappears or usage cannot yet be proved | Mark pending, fence/cancel further work, and reconcile |
| Unresolved usage after 24 hours | After fencing the worker, release the customer's reservation; later provider cost is AC's expense |
| Verified payment refund or reversal | Reverse the corresponding funding once; preserve the original ledger history |

The 24-hour target is a customer-facing settlement commitment for implementation, not an existing operational guarantee. A timeout alone cannot free a reservation while its worker can still spend.

Purchase refunds use the original payment route and the applicable purchase rules. If a reversal exceeds remaining funds, record the shortfall separately and put paid usage on hold for review. Never display a negative spendable balance or silently debit another payment method. Keep promotional and purchased provenance through all adjustments.

## Records and rollout

Store payment/request IDs, account reference, model, usage quantities, amounts, funding provenance, tariff version, and state transitions. Do not put prompts, generated content, API keys, or wallet secrets in the financial ledger. History should explain each top-up, charge, release, and refund without requiring access to private creative content.

Implementation sequence:

1. Shadow-meter usage without charging users; measure actual costs and establish tariffs.
2. Build the ledger and exercise concurrent reservations, request replays, worker crashes, cancellation, and reconciliation using test funds.
3. Add balance, spend limits, and receipts to aesel; verify one account across iPhone, Mac, and web.
4. Verify sandbox purchases, duplicate/out-of-order notifications, account switching, refunds, and interrupted delivery.
5. Publish final tariffs, purchase offers, free-allowance terms, and supported storefronts before enabling paid access.

Still to set from measured costs: retail tariffs, top-up amounts, free-allowance size, initial regions, and maximum account/turn limits. These do not change the core definition: **add money to AC balance; spend it on explicitly priced AC services.**
