# ATProto Integration Plan

## Overview
- **Goal:** Connect aesthetic computer (AC) clients, services, and media pipeline to the AT Protocol (ATProto) so creative works, identity, and live interactions flow across both ecosystems.
- **Success criteria:**
  - AC users can authenticate with an ATProto account (DID/handle) and publish select outputs as signed records.
  - AC services ingest ATProto records for discovery, remixing, and notification overlays inside AC.
  - Core schemas, infra, and tooling are documented and reproducible for self-hosters.

## Architectural context
- AC stack spans browser client (KidLisp runtime & pieces), `session-server` (Fastify, WebSocket, Redis/Jamsocket), native daemon & overlay tooling, plus shared utilities.
- Publishing flows today target AC-managed storage (stickers, tapes, prompts) with AC handles.
- ATProto architecture centers on per-user Personal Data Servers (PDS), AppView ingestion services, Lexicon-defined JSON records, OAuth for app auth, and firehose/relay event streams.

## Integration pillars
1. **Identity bridge** – map AC handles to ATProto DIDs, manage OAuth flows, and sync profile data.
2. **Publishing pipeline** – export AC media + metadata as ATProto records/blobs under AC-owned Lexicon namespaces.
3. **Discovery & notifications** – run an AppView ingestion path inside `session-server` (or companion service) to surface ATProto activity inside AC UI and overlays.
4. **Developer ergonomics** – provide shared libraries and KidLisp primitives so pieces can interact with ATProto data.
5. **Governance & moderation** – align AC filters/moderation signals with ATProto collections for cross-network safety.

## Roadmap snapshot

| Phase | Duration | Outcomes |
| --- | --- | --- |
| 0. Research & schema design | 2 weeks | Lexicon drafts for creative pieces, token storage policy, integration architecture doc |
| 1. Infrastructure bootstrap | 3–4 weeks | Dev PDS/AppView, OAuth login in `session-server`, firehose ingester prototype |
| 2. Client feature spike | 3 weeks | Share-to-ATProto command, Atmosphere feed prototype, overlay notifications |
| 3. Media & content pipeline | 4 weeks | Blob upload workflow, content hashing, moderation hooks |
| 4. Identity & verification | 2 weeks | Handle linking UI, profile sync |
| 5. Scaling & federation | ongoing | DB hardening, relay partnership, packaging for self-hosters |
| 6. Beta launch & iteration | ongoing | Pilot rollout, telemetry, schema revisions |

## Detailed workstreams

### 0. Research & schema design
- Audit current AC user identity, publishing endpoints, and asset storage requirements.
- Register reverse-DNS namespace (e.g., `computer.aesthetic`) for Lexicon IDs.
- Draft Lexicon records for: painting artifacts, KidLisp programs, performance logs, remix references, moderation signals.
- Document token storage & privacy considerations (encrypted at rest, renewal cadence).

### 1. Infrastructure bootstrap
- Stand up dev PDS using self-host guide or Bluesky sandbox; configure AppView co-located with `session-server`.
- Integrate `@atproto/oauth-client-node` into `session-server` Fastify routes; persist tokens (Redis + encrypted persistence) and DIDs per AC account.
- Build ingestion worker using `@atproto/sync` firehose filter on AC namespaces; write to SQLite/Postgres (start with SQLite for prototyping).
- Expose internal API to return aggregated piece records to AC client.

### 2. Client feature spike
- Add KidLisp command (`share piece`) to trigger OAuth, export current piece/session, and publish ATProto record.
- Create “Atmosphere feed” prompt view powered by AppView data.
- Extend `ac-event-daemon` to subscribe to ingestion events and display notifications (mentions, remixes, follows).

### 3. Media & content pipeline
- Implement blob-upload helper (handles PNG/GIF/video) with progress, error handling, and deduplication via content hashing.
- Embed canonical asset URI (AC CDN) and optional ATProto blob ref in published records.
- Reuse `shared/filter.mjs` profanity + safety checks prior to publishing.
- Support optimistic updates so published records appear instantly in AC feeds.

### 4. Identity & verification
- Build UI for linking AC handle to ATProto handle (DNS challenge, e-mail fallback).
- Sync ATProto profile record fields (displayName, avatar, description) into AC profile views.
- Allow optional automatic updates when ATProto profile changes.

### 5. Scaling & federation
- Evaluate migrating ingestion store to Postgres; ensure idempotent upserts and backlog replay.
- Decide whether to run dedicated relay for AC namespaces or partner with existing relays.
- Package integration as optional module for self-hosted AC deployments (install scripts, env vars, docs).

### 6. Beta launch & iteration
- Recruit power users to test sharing pipeline and Atmosphere feed.
- Track metrics (share rate, ingestion latency, feed engagement, OAuth errors).
- Iterate on Lexicon schemas (version fields, backward compatibility) based on feedback.
- Prepare public documentation & marketing (blog, docs site section).

## Deliverables checklist
- `docs/atproto-integration.md` high-level architecture & onboarding guide.
- `spec/lexicons/computer.aesthetic.*.json` schema files + generated TS types.
- `shared/atproto.mjs` (or similar) shared helper module for API calls & validation.
- `session-server` Fastify routes + tests for OAuth, publishing proxy, ingestion API.
- KidLisp command implementations + client UI components for feed/notifications.
- Deployment artifacts: PDS/AppView configs, relay scripts, env templates.

## Risks & mitigations
- **Security of OAuth tokens** → encrypt at rest, rotate regularly, limit scopes, store server-side only.
- **Content moderation mismatch** → embed moderation signals in records; sync with ATProto takedown lists.
- **Storage costs for media blobs** → hybrid approach (blobs for small assets, external URLs for large media).
- **Protocol evolution** → track ATProto spec updates; version Lexicons; add feature flags around beta endpoints.
- **User experience complexity** → provide clear onboarding UI, fallback flow for AC-only accounts.

## Dependencies & coordination
- Access to ATProto sandbox or production PDS.
- Legal review for cross-network data sharing.
- DevOps support for deploying AppView/relay services.
- Coordination with ATProto community for namespace registration and schema visibility.

## Metrics
- % of active AC users who link ATProto accounts.
- Median publish-to-visibility latency across Atmosphere feed.
- Number of ATProto-originated pieces consumed/remixed inside AC.
- OAuth error/abandonment rate.
- Moderation turnaround time for cross-network content.

## Open questions
1. Which creative artifacts should be first-class ATProto records (paintings, music, text, KidLisp scripts)?
2. Where should canonical media live long-term (PDS blobs vs AC CDN vs hybrid)?
3. Do we need offline desktop sync (native daemon) for heavy media publishing MVP?
4. What governance model do we adopt for Lexicon evolution (AC-run vs community RFC)?
5. How does cross-app attribution/remix tracking feed back into AC reputation systems?
