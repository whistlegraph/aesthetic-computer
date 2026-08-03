# PostHog

PostHog is additive product analytics. It does not replace the existing
operational or traffic systems.

| System            | Ownership                                                                                                        |
| ----------------- | ---------------------------------------------------------------------------------------------------------------- |
| Lith + Silo/Mongo | Boot health, piece-run logs, errors, bundle performance, piece-hit counters, access logs, and storage operations |
| Google Analytics  | Existing broad site traffic                                                                                      |
| PostHog           | Minimized journeys, funnels, cohorts, and experiments                                                            |

The browser integration is inert until Lith receives `POSTHOG_PROJECT_TOKEN`.
`POSTHOG_API_HOST` may be `https://us.i.posthog.com` (default) or
`https://eu.i.posthog.com`. The project token is public browser configuration;
personal API keys and OAuth tokens never belong in the repository or HTML.
Set `POSTHOG_SERVER_ENDPOINT_EVENTS=true` separately to enable anonymized,
batched endpoint-volume events. This second switch makes the higher-volume
server stream an explicit rollout decision.

Initial capture is deliberately narrow:

| Event                   | Properties                                                                    |
| ----------------------- | ----------------------------------------------------------------------------- |
| `$pageview`             | `ac_route`, with query, hash, published handles, and source removed           |
| `ac_piece_opened`       | built-in `piece` or `null`, `piece_kind`, minimized `route`                   |
| `$identify`             | Auth0 `sub`; public `handle` when available; never email                      |
| `ac endpoint completed` | `endpoint`, method/status/latency buckets, analytics class, aggregate `count` |

Autocapture, session replay, exception capture, performance capture, heatmaps,
surveys, and product tours are off. Embedded, packed, local, and preview/icon
renders do not initialize PostHog. Do Not Track is respected.

Start with a journey from `$pageview` to `ac_piece_opened`, then break down by
`ac_route`, `piece_kind`, and built-in `piece`. Validate that published pieces
have `piece = null` before adding any dashboard or experiment.

The server event is an anonymous aggregate, not a person event. It uses one
Lith-level distinct ID, disables person profiles and GeoIP, and combines equal
dimensions into ten-second batches. Never use it for unique-user counts or
person funnels; sum its `count` property for request volume.

## Endpoint map

Run the inventory from the repository root:

```sh
node toolchain/analytics/posthog-inventory.mjs > /tmp/ac-posthog-inventory.json
```

The current tree contains 165 function source files, resolving to 164 unique
names: 160 statically detected handlers and four helpers or scripts. This is
not the same as the number of public routes. Lith also provides aliases, media
routes, host rewrites, operational routes, static files, and piece/index
fallbacks. `lithRouteFamilies` in the generated JSON maps those surfaces.

Every function is classified by `shared/posthog-policy.mjs`:

| Policy                           | PostHog treatment                           |
| -------------------------------- | ------------------------------------------- |
| `minimized-browser-or-aggregate` | Browser journey and/or bounded server count |
| `aggregate-status-only`          | Bounded server count only                   |
| `inventory-only`                 | Mapped for context; emits no event          |
| `existing-lith-silo-only`        | Remains in Lith/Silo/Mongo                  |
| `disabled` / `review-required`   | Fails the inventory test until reviewed     |

The server count contains only function name, HTTP method, status class,
duration bucket, analytics class, and count. It never reads or sends path,
query, request or response body, IP, user agent, user ID, authorization header,
raw error, or stack. Messaging, MCP, local-machine, admin, and existing
operational telemetry classes do not emit endpoint events.

Adding a function source requires an analytics classification. Run:

```sh
node --test \
  system/tests/product-analytics.test.mjs \
  system/tests/posthog-server.test.mjs \
  system/tests/posthog-inventory.test.mjs
```

The inventory test fails when the source/handler counts change or any function
lands in `review-required`. Update the reviewed policy and counts together.

## Validation

Before enabling the server switch, configure only the browser token and verify:

1. Production `$pageview` events contain minimized `ac_route` values.
2. `/@handle/...`, `/$code`, and prompt-source routes become `/@published`,
   `/$code`, and `/prompt`; no query or hash survives.
3. Published pieces have `piece = null`.
4. Identified profiles contain Auth0 `sub` and optional public handle, never email.
5. Replay, autocapture, exception, performance, survey, and tour data remain absent.

Then enable `POSTHOG_SERVER_ENDPOINT_EVENTS=true` and verify `ac endpoint
completed` has only the documented properties. A useful HogQL request-volume
check is:

```sql
select
  properties.endpoint as endpoint,
  sum(toInt64(properties.count)) as requests
from events
where event = 'ac endpoint completed'
group by endpoint
order by requests desc
```

For product behavior, create a funnel from `$pageview` to `ac_piece_opened` and
break it down by `piece_kind`. Endpoint aggregates answer system-usage volume;
Lith/Silo answer failures and diagnostics; neither should be substituted for
the other.

Rollback requires no code or data migration: unset `POSTHOG_PROJECT_TOKEN` to
disable browser and server analytics, or unset only
`POSTHOG_SERVER_ENDPOINT_EVENTS` to retain browser journeys.

## Product context

- The front door is a prompt-driven creative computer. `imnew` registers,
  verification establishes the account, `handle` claims a public identity, and
  a piece name opens a program.
- The browser runtime lives in `system/public/aesthetic.computer`; pieces are in
  `disks/`; Lith serves the site and API; Silo is the data and storage console.
- Built-in, published, and KidLisp pieces are different content classes. Raw
  source, prompts, paintings, chat, and private account fields are content, not
  analytics properties.
- Local tools include Slab/host tooling and the prox, fleet, paper, frame,
  chat, DM, mail, and calendar MCP surfaces. Their existence and public product
  role are useful context. Prompt/session content, contacts, messages, mail,
  calendars, fleet host details, secrets, and local files are not analytics
  inputs.

Any local-tool analytics must be a separate opt-in proposal with a minimized
schema such as `{ tool_family, operation_class, outcome, duration_bucket }`.
Never include arguments, file paths, hostnames, handles, recipients, message
content, artifact contents, or raw errors by default.

## Self-driving

PostHog's setup command is:

```sh
npx @posthog/wizard self-driving
```

Do not run it unattended. It can connect GitHub, enable replay and error
tracking, configure signal sources, and schedule scouts. Before activation:

1. Verify production events and the privacy contract above.
2. Decide whether AI data processing is acceptable.
3. Resolve repository routing: Tangled is authoritative and GitHub is currently
   documented as a read-only mirror, while PostHog requires a writable GitHub
   repository to open PRs.
4. Grant only the selected GitHub repository, require human review and merge,
   and leave deployments outside PostHog.
5. Review every proposed signal source. Replay, error capture, support, Slack,
   and local MCP content stay off until each has its own privacy review.

Self-driving creates a branch and PR for an actionable report; a human still
reviews and merges it. Start with one manually reviewed report, not broad
autonomous production mutation.
