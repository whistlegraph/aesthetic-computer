# Monthly handle colors

After creating a handle, non-supporters can open the color editor or continue
to their starting piece. Existing supporters skip the offer. Billing lookup
failures never block signup.

Anyone can preview colors. Saving requires an active monthly AC subscription
at any amount. The server matches the authenticated, verified email to Stripe
customers and accepts monthly `Give` products, including legacy products, or
products marked `ac_support=monthly`. Sotce subscriptions do not qualify.
Saved colors remain visible after support ends; subsequent edits require
active support.

The support checkout authenticates the account and uses its verified email.
It returns to `/handle`, where a local preview is retained for up to 24 hours.
The existing payment minimums still apply: $1 USD or 5 DKK per month.

Signup completes before the optional offer. Existing anonymous funnel events
add `support_shown` and `support_clicked`, honoring the same privacy opt-outs.
Checkout and subscription metadata record only the reviewed placement
(`signup` or `handle-colors`), without a handle or signup-attempt identifier.
Paid starts and renewals must be measured from Stripe billing records, not
offer clicks or checkout returns.
