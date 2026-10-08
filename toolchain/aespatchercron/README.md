# aespatchercron

Read Aesthetic Network analytics, recent AC navigation, Lith/boot diagnostics
and public user reports, investigate a reproducible defect, and prepare an
independently reviewed draft PR.
An empty queue is a valid result.

```sh
slab/bin/aespatchercron init
slab/bin/aespatchercron collect
slab/bin/aespatchercron scan
slab/bin/aespatchercron status
slab/bin/aespatchercron tick
```

`init` creates owner-only configuration, evidence and task state under
`~/.local/share/aespatchercron`. It never overwrites an existing config.
`--home DIR` selects a separate state directory. `tick` collects and admits
hypotheses, then investigates at most one queued task. It stops at review.
It invokes the installed Codex CLI and uses that account's inference quota.
Neither initialization nor collection invokes a model.

No cron job is installed. `cron.example` is an operator-installed schedule;
replace its executable paths first. The defaults permit two new tasks per UTC
day, four active tasks, four changed files and 200 changed lines. One process
owns the state lock, with a 15-minute worker limit. A crashed process leaves
`lock/owner.json`: check that its PID has ended before removing the lock.

## Evidence and scope

The collector sends read-only Mongo aggregation code over the existing Lith
SSH transport. No remote file or deployment changes are needed. Configuration
uses the AC Lith key, host and checkout; it never reads Fuser credentials or
client analytics. Missing credentials and unsupported queries fail visibly.

* `piece-runs`: built-in JavaScript piece loads and error-bearing runs,
  including a retained error followed by `status: complete`.
* `piece-runs` paths: adjacent recorded loads in one boot, at most 30 minutes
  apart. Known automation is excluded; unmarked automation may remain.
* `account-activity` paths: adjacent `piece_opened` events in one authenticated
  AC runtime, ordered by sequence. Identities remain inside Mongo.
* `network-visits`: non-automated studio page visits, interaction and engagement
  by property and coarse surface. Client properties are excluded.
* `boots`: the actual collection behind `/api/boot-log`, grouped by public
  landing piece. Retained error payloads and logged error events count even
  after a successful completion. Unfinished boots alone do not prove failure.
* `lith-journal`: bounded recent `lith.service` journal entries, recognizing
  function exception prefixes on the server. Raw lines never leave Lith.
* `lith-errors`: Lith's bounded in-memory error ring, fetched over loopback and
  reduced to function counts before transport. It resets when Lith restarts.
* `chat-clock` and `chat-system`: bounded recent public, nondeleted, nonmuted
  messages; only selected malfunction reports become private evidence.
* PostHog: read-only, AC-bound aggregate piece events and endpoint status counts
  when a separate read credential is configured. No exception-tracking claim.

The two path sources overlap and must not be summed as unique journeys or
people. They represent adjacent observed events, not complete browsing
histories or cross-site attribution. Private/unknown pieces become barriers
where present; unrecorded loads cannot be reconstructed. Visit snapshots
cannot supply paths. The source model is documented in
[`../analytics/VISITS.md`](../analytics/VISITS.md).

Operational sources return public built-in JS slugs, reviewed studio
properties/surfaces, repository function names and counts. Repeated-error
candidates require the configured threshold (at least three observations);
diagnostic counters may include smaller groups. No account/session/boot identity, raw error,
log, stack, IP, UA, query string, mail, contact, fleet detail or MCP payload
is exported. Client-only build/report/media functions are excluded; the
unrelated `/api/reports` stores private false.work build reports and is never
read here. Source failures and truncation remain explicit, never zero activity.

Candidate rules combine repeated error-bearing piece runs/boots, endpoint
errors with one eligible piece reference in repository source, and explicit
public malfunction reports tied to a supported piece. One user report can
nominate an investigation; it still must reproduce before a patch exists.
Incoming/outgoing paths guide reproduction. Popularity or low engagement alone
never proves a bug. Unmapped or ambiguous endpoint/core failures and PostHog
5xx aggregates remain manual-review diagnostics. Referencing an endpoint in
source is a clue, not proof of causality or permission to change its backend.
KidLisp, published pieces and native apps have no automatic patch worker yet.

Public report intake looks for English/Danish malfunction language and a
known piece reference. Clock-chat app/audio/radio complaints can nominate
`laer-klokken`. Everyday conversation, resolved issues and ungrounded complaints
do not automatically become work. The rules are conservative and can miss
reports; this is not a claim to understand every message. Defaults examine the
last 24 hours, at most 200 qualifying messages per public channel and 2,000
journal entries. Set `collection.chatLimit` up to 500 and `logLimit` up to 5,000.

Selected excerpts are stored separately in owner-only `privateReports`, with
sender/identity columns excluded, URL/email/handle/address/number redaction,
a source-text hash and bounded timestamp. The worker receives at most three
selected excerpts as explicitly untrusted data. Instruction-like or ambiguous
reports require manual review; embedded commands/links cannot authorize an
action. There are no replies or reporter outreach. Public PR prose and patch
files are checked for copied six-word report phrases, then independently
reviewed for privacy; the phrase check is not a complete PII classifier.
Neither excerpts nor other collected evidence are sent to PostHog.

`latest.json` keeps the original aggregate `report` plus supplementary
`signals.coverage`, `metrics`, `leads` and `privateReports`. CLI output omits
excerpts. `scan FILE.json` can import a saved evidence envelope or a minimized
aggregate report; it applies the same schema and admission gates. `scan`
reports the number of manual-review diagnostics as well as worker candidates.
Collector provenance hashes all collection/validation modules and the exact
remote program sent to Lith.

## PostHog read access

Set `posthog.projectId` and `posthog.organizationId` in private `config.json`,
and supply the AC read key through `AESPATCHER_POSTHOG_READ_KEY`. Use a key
scoped to the selected project's metadata and query reads (`project:read`,
`query:read`; see [PostHog queries](https://posthog.com/docs/api/queries)). The
adapter checks the selected project, organization and token against AC's
production browser configuration before sending aggregate queries. It accepts
only the official US/EU cloud hosts, rejects redirects, and never borrows a
generic or Fuser credential. A capture token cannot authorize these reads.

The queries count known built-in piece open/interaction/prompt-success events
and allowlisted endpoint status aggregates. They request no identities,
recordings, raw events or URLs. Counts are event context, not people or a
conversion funnel. Missing read credentials/project binding are reported as
unavailable; `posthog.enabled: false` explicitly disables the source. No
capture, analytics instrumentation or write operation is performed.

One stable task ID groups each source/piece hypothesis, not each unique bug;
public reports also bind their private evidence reference. Only one active
investigation per piece is admitted, even when multiple sources agree.
It is deduplicated for the lifetime of its state record. Reopening a resolved
route with a fresh task/branch identity is not automated in this version;
repeated regressions need an operator to investigate separately. Retain old
evidence and PR identities rather than deleting state to force another PR.

## Investigation and review

```sh
slab/bin/aespatchercron prepare TASK_ID
slab/bin/aespatchercron work TASK_ID
slab/bin/aespatchercron validate TASK_ID
slab/bin/aespatchercron review TASK_ID /path/to/review.json
slab/bin/aespatchercron publish TASK_ID
```

`prepare` verifies the AC knot origin, fetches `main`, and creates a dedicated
branch/worktree. It copies no edits from the primary checkout. The host's git
performance guard remains active; a deferred worktree creation must wait for
resources to recover. No guard, git hook or signing policy is bypassed.

`work` runs Codex with its workspace sandbox, network disabled for generated
commands, and personal configuration/MCP connections excluded. The brief
requires reading the score, HAND and existing helpers before tracing a route
and reproducing the failure. It forbids commit, push, deployment, contact,
credential acquisition and nested agents. An unproven problem returns IDLE.
Only the implicated piece and relevant Node regression tests may change;
core, backend/auth/database/payment, deployment, dependency, fleet and client
changes are rejected. Account/commerce pieces need separate authorization.
Known sensitive leaves (including `prompt`, `handle`, `give` and account
deletion) are excluded before admission. Registration/deletion, auth,
credentials, personal data, checkout/payment/minting and publishing permission
flows remain off-limits inside other pieces too. A telemetry allowlist is not
a complete safety classification; the independent review must check scope.

Validation executes generated tests in macOS `sandbox-exec` with network,
private-home reads and source writes denied, a clean environment and temporary
scratch space. Linux is currently unsupported for this step and fails closed.
The exact original piece must fail the named `AES_REPRO:TASK_ID` assertion;
the patch must pass. Missing modules or environment failures do not qualify.
No raw test output is published. Tests receive neither SSH nor API credentials.

The packet records source hashes, exact base revision, changed-file digest,
test receipts, reuse findings and HAND observations. The checker is grounded
in [`../../HAND.md`](../../HAND.md), *The Hand and the Loop*, and inspected
2022 `help.mjs` / 2023 `num.mjs` revisions. Counts of guards, comments and lines
are approximate mechanical observations, not an automatic aesthetic score.
A human or independent reviewing agent must judge one-mind knowability,
idiom names, why-comments, boundary guards, leaf size, justified infrastructure
and reuse. The investigating worker's self-review does not satisfy this stage.
The runner validates the attestation shape, not the reviewer's identity or
truthfulness; its operator is responsible for an actually independent review.

Example `review.json` shape (replace every explanatory value with findings):

```json
{
  "base": "base commit from review-packet.json",
  "digest": "exact digest from review-packet.json",
  "decision": "approve",
  "kind": "human",
  "reviewer": "reviewer name",
  "userImpact": "Observed trigger and resulting user-visible behavior.",
  "reproduction": "Evidence that the same assertion fails before and passes after.",
  "privacy": "The title/problem/cause/reviewer text is safe for a public PR.",
  "scope": "The patch touches no protected account, permission, private-data or commerce flow.",
  "hand": {
    "mind": "Describe the module's purpose and state boundary.",
    "names": "Explain how changed names fit the local idiom.",
    "comments": "Identify the necessary reasons retained in comments.",
    "guards": "Explain the boundary that earns each added guard.",
    "leaves": "Explain why the piece remains a small lifecycle leaf.",
    "reuse": "Explain the existing helper contracts considered."
  },
  "reuse": [{
    "id": "idioms",
    "sourceHash": "current hash from the reuse report",
    "decision": "considered",
    "reason": "Explain why this existing helper fits or does not fit."
  }]
}
```

Any changed source, base, HEAD, untracked patch file, staged content or stale
selected reuse entry invalidates publication. The committed tree is checked
independently of the working tree. A stopped worker retains its task and
failure for inspection; nothing automatically retries an unreviewed patch.

## Reuse map

```sh
slab/bin/aespatchercron reuse
slab/bin/aespatchercron reuse numbers
slab/bin/aespatchercron reuse-add /path/to/finding.json
slab/bin/aespatchercron hand HEAD
```

The initial map contains 12 source-anchored findings: piece lifecycle, numeric
and idiom helpers, visit/account policy, piece signals, existing reports,
transport, PostHog boundary and HAND. It is a curated starting point, not a
complete dependency index. `reuse-add` accepts `{ "id", "file", "anchor",
"finding" }`, checks that the source anchor exists at HEAD, and records its
commit, source hash and line. Semantic correctness still requires reading the
source. Reads flag drift; they never silently refresh a finding. Re-read and
explicitly update it when the source changes. Workers consume this map and
reviewers must account for at least one relevant current entry.

## Draft hosting

AC's canonical host is `knot.aesthetic.computer` / Tangled. The default is
`hosting.kind: "unconfigured"`: this implementation does not claim Tangled
draft support without a verified draft API. No GitHub fallback is automatic.

A real GitHub adapter is included for an explicitly selected writable AC
review destination. Only after that destination is authorized, set:

```json
{
  "kind": "github",
  "remote": "github",
  "repository": "whistlegraph/aesthetic-computer",
  "writableReviewDestination": true
}
```

The adapter checks both hosts' `main` against the reviewed base, commits the
exact patch, pushes its isolated branch and calls `gh pr create --draft`.
It checks the resulting draft flag, base and commit. Stable task markers and
branch queries reconcile an uncertain create without submitting a duplicate.
It never merges or deploys. If the commit succeeds but saving its receipt is
interrupted, publication fails closed; inspect HEAD and record/revalidate the
commit before retrying. `publish` alone performs commit/push/PR side effects.

## Verification

```sh
node --test toolchain/aespatchercron/test/*.test.mjs
```

Tests cover privacy/schema rejection, actual account event names, route
barriers, source failures, queue bounds/deduplication, private state/locking,
reuse drift, dirty-checkout isolation, real sandbox restrictions, before/after
assertions, protected paths, stale reviews, hidden staged changes, independent
commit-tree hashes and remote draft deduplication. Supplemental checks cover
English/Danish report routing, moderation filters, redaction, instruction-like
reports, public quotation rejection, real boot schema, bounded log coverage,
client exclusion and source failures. PostHog tests cover project binding,
aggregate query shape, missing read permissions and response validation.
The worktree integration
test explicitly skips when the host performance guard returns exit 75; it
does not bypass the guard. No test invokes a model or creates a remote PR.
