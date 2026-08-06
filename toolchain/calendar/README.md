# AesthetiCal → Google Calendar

One-way, API-native mirror from the signed-in AesthetiCal account to a
secondary Google calendar named **AesthetiCal** on `mail@aesthetic.computer`.

Google's Calendar API cannot subscribe to an arbitrary ICS URL. This tool
creates a native secondary Google calendar and mirrors events through the
Events API instead. Mirrored events carry their AesthetiCal UID and revision in
private extended properties, so repeated syncs update in place and deleted
AesthetiCal events are removed without touching manually-created Google events.

```bash
node toolchain/calendar/gcal-sync.mjs auth   # one-time Google OAuth consent
node toolchain/calendar/gcal-sync.mjs enable-api # one-time project API enable
node toolchain/calendar/gcal-sync.mjs sync   # reconcile immediately
node toolchain/calendar/gcal-sync.mjs status
toolchain/calendar/install.sh                # sync every five minutes
```

The existing Desktop OAuth client is reused from
`aesthetic-computer-vault/youtube/client.json`. Calendar-specific credentials
and state live in the vault at:

```
aesthetic-computer-vault/calendar/google-token.json
aesthetic-computer-vault/calendar/state.json
```

The sync is intentionally one-way. Google edits are overwritten only when the
corresponding AesthetiCal revision changes; Google-only events are untouched.
