# caPUR client loop

Last updated: 2026-07-24

This is a secret-free handoff for Jeffrey's work with Artur. Do not place ATH
private keys, passwords, tokens, or copied environment values in this folder.

## Verified context

- The client app is referred to as caPUR and is live at `capur.app`; Artur also
  configured `www.capur.app`.
- The application repository is referred to in the thread as `fitness-app` and
  is deployed with Vercel.
- Work reported complete on 2026-07-14 included local HTTPS, dashboard polish,
  a green LED-style live-session timer, removal of debug panels, splash/logo
  fixes, automated login tests, English and Spanish sign-in/dashboard tutorial
  videos, and the logo material studies in this directory.
- Earlier reported work included web push notifications, an admin notification
  sender, a shared responsive page shell, and live-session timer styling.
- Artur's latest concrete design feedback is that the dashboard and live timer
  still need surface work. He specifically called out the admin dashboard being
  compressed on laptop, squashed explanatory text, and the need for desktop
  content to use the available width while remaining phone-friendly.
- Artur also wants to discuss the timer model, dashboard, and user-data
  architecture. Timer notes should account for supersets, dropsets, and
  circuits.

## Awaiting from Artur

- Confirmed ATH provider requirements and exact success/cancel URLs
- The secure location/account/environment holding ATH credentials (not the
  credential values)

The earlier iMessage index exposed attachment markers but not file contents or
filenames. Artur later identified the GitHub repository, and the promised files
were inspected there read-only on 2026-07-24:

- `CAPUR_BILL.md`
- `WORKLOG.md`
- `CaPUR_ATH_Movil_Integration_TODO.pdf`

These remain in the private `babylon-detective/fitness-app` repository. Do not
copy the bill into this public monorepo.

## Current blocker

- On 2026-07-24, Artur reported that the client will not share API keys online.
- Artur considers the ATH handoff blocked until they meet in person.
- Artur told the client he has a trip on Tuesday. The thread does not yet state
  a confirmed meeting time, so do not assume the handoff will happen before the
  trip.
- No credential request or workaround should be sent automatically. The useful
  next input is a confirmed meeting time and, during the meeting, client-owned
  configuration of the secret values.

## Intake findings

### Client TODO PDF

- The two-page English PDF renders cleanly with no clipping or broken layout.
- It gives the client the production webhook URL,
  `https://www.capur.app/api/payments/ath-webhook`, and asks for business
  identity, public/private tokens, sandbox access, callback URLs, and KYC.
- It currently says the client supplies tokens to the developer. Because the
  client refuses online key sharing, the handoff wording should be simplified
  to client-operated secret entry during the in-person meeting.
- It describes signature verification and subscription activation as future
  developer work; those capabilities do not exist yet.

### Bill and work log

- Both use the same 99-commit, 38-active-day, approximately 97-hour estimate.
- The hours are inferred from commit density and scope, not contemporaneous
  time records. Present them as an estimate, not measured labor.
- The bill lists a paid contractor cost but does not include that line in its
  subtotal or markup calculations. Artur must decide whether it is reimbursable
  and then make the arithmetic internally consistent.
- The proposed operational-expense allocation and markup should be agreed with
  the client before presenting the document as an invoice.
- `WORKLOG.md` says it was generated from git history on 2026-06-26 despite
  containing work through 2026-07-21. Correct or explain the footer date.

### ATH implementation reality

- A live GET to the documented webhook URL returned HTTP 200 on 2026-07-24.
- The deployed handler is only an acknowledgement stub. It accepts GET/POST
  and returns `{ received: true }`; it does not verify a signature, validate a
  transaction, prevent replays, or activate a subscription.
- The frontend checkout service constructs a sample URL locally and has no ATH
  SDK dependency. The UI itself labels this a checkout scaffold.
- Naming is inconsistent:
  - PDF: `VITE_ATH_PUBLIC_TOKEN`, `ATH_PRIVATE_TOKEN`
  - security doc: `VITE_ATH_MOVIL_*`, `ATH_MOVIL_CLIENT_ID`,
    `ATH_MOVIL_CLIENT_SECRET`, `ATH_MOVIL_WEBHOOK_SECRET`
  - current frontend config uses merchant email, environment, and an enable
    flag rather than a public token
- Route naming is also inconsistent: the PDF and code use
  `/api/payments/ath-webhook`, while the security checklist proposes
  `/api/payments/webhook` plus a separate `/api/payments/create-checkout`.
- Before key handoff, choose one server-side contract from the actual ATH
  Business documentation: credential names, signature algorithm and headers,
  webhook event schema, checkout creation endpoint, success/cancel behavior,
  sandbox availability, and replay/idempotency rules. Do not infer these from
  the current scaffold.

## Intake checklist

When the files arrive:

1. Preserve the originals in a dated intake subdirectory.
2. Inspect the PDF visually and extract its action items without changing the
   client's stated requirements.
3. Compare the TODO, work log, and bill for scope/date/amount consistency;
   flag discrepancies rather than guessing.
4. Produce a short Jeffrey-facing summary: completed work, client actions,
   developer actions, blockers, and the next demo target.
5. Keep financial documents private and avoid committing them unless Jeffrey
   explicitly requests that.

## ATH handoff boundary

- Treat the webhook URL as configuration and confirm its exact deployed route
  before asking the client to add it.
- Keep the ATH private key in the deployment platform's secret environment, not
  in source, Markdown, screenshots, or iMessage.
- Prefer an account role/invite or a client-operated screen share for account
  configuration; do not collect the client's reusable password.
- Record only secret names, ownership, target environment, and verification
  status in this handoff.
- Do not claim the ATH integration works until a real or documented sandbox
  callback has been verified end to end.
