# Analytics

## PostHog account boundary

The Aesthetic Computer PostHog connection is for Aesthetic Computer only.
Other organizations require their own authorization and connection.

Aesthetic Computer PostHog complements existing Lith, Silo, Mongo, Google
Analytics, MCP, and local-tool telemetry; it does not replace those operational
channels. Do not send prompt or session content, chats, mail, contacts, fleet
host details, local files, secrets, or raw MCP payloads to PostHog by default.
Describe those systems as product context without exporting their private data.
New local-tool or MCP analytics must use an explicit, minimized, opt-in event
schema.

For traffic reports, see [VISITS.md](VISITS.md).
