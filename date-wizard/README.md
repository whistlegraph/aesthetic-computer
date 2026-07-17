# DateWizard

A native macOS AppKit pop-up that shows your upcoming **appointments agenda** and lets you schedule
against the [AesthetiCal](https://aesthetic.computer) backend. Your editable
*aesthetical* events sit next to your Google calendar(s), overlaid read-only.

Sibling to `wave-wizard/`, `clip-wizard/`, `juke-wizard/` — same conventions:
a Swift Package executable, AppKit (`NSApplication` + `AppDelegate` + `NSWindow`
+ custom-drawn `NSView` subclasses). **Not SwiftUI.**

## Build & run

```bash
cd date-wizard
swift build          # compiles the DateWizard executable
swift run DateWizard # build + launch the window
```

Requires macOS 12+ and a Swift 5.9 toolchain.

## Sign in (shared AC session)

DateWizard signs you in **natively, in-app** — no terminal, no CLI. It shares
the same `~/.ac-token` the rest of the AC suite uses, so one sign-in serves
`ac-os`, the other wizards, and the Electron tray.

On first run (or after the token expires and a request comes back `401`), it
shows a **sign-in screen**:

- **Sign in** runs AC's OAuth2 + PKCE browser flow directly (`ACLogin.swift`):
  it spins up a localhost loopback callback on port `44233`, opens your browser
  to `hi.aesthetic.computer`, then on the redirect exchanges the code, fetches
  your `@handle`, and writes `~/.ac-token` itself — the same token the
  `ac-login` CLI would write. The app proceeds automatically the instant the
  token lands (via the shared session file-watch).
- **Use ac-login CLI instead** is a fallback that launches `ac-login` in
  Terminal — only needed if the native flow can't bind its loopback port.

The token file is JSON written either by the native flow or the AC stack:

```
~/.ac-token   # { "access_token": "<jwt>", "refresh_token": "...", "expires_at": <ms-epoch> }
```

`access_token` is sent as `Authorization: Bearer …` on every `/api/cal` call.
The token is treated as expired when the file is missing, unparseable, or
`expires_at` is in the past — at which point you're prompted to sign in again.

## Using the agenda

- The main window is one chronological, scrollable list covering the next 14 days.
- **+ Event** opens the editor sheet.
- **Aesthetical events** draw with a solid fill (green = public, accent =
  private) and are **editable** — click one to edit or delete it.
- **Google / connected-calendar events** draw dashed with a source-color dot
  and are **read-only** — clicking shows details, no edit.
- Former week/day drill-down views are retired; Today and legacy day launch
  actions open the same reliable upcoming agenda.

## Connect a Google calendar

Click **Connect Google…** and paste your Google *secret address in iCal format*
URL plus a label:

> Google Calendar → Settings → *your calendar* → **Integrate calendar** →
> **Secret address in iCal format**

This `POST /api/cal {feed:{url,label}}` subscribes the calendar; its events
then overlay your week live and read-only.

## Backend contract

All authed calls send `Authorization: Bearer <token>`. Base
`https://aesthetic.computer`.

| Method | Path | Purpose |
|--------|------|---------|
| GET    | `/api/cal?from=&to=` | your editable events in range |
| POST   | `/api/cal` `{title,start,end,allDay?,note?,visibility?}` | create |
| PUT    | `/api/cal` `{uid,...}` | update |
| DELETE | `/api/cal?uid=` | delete |
| GET    | `/api/cal?feedEvents=1&from=&to=` | read-only Google overlay |
| GET    | `/api/cal?feeds=1` | list connected calendars |
| POST   | `/api/cal` `{feed:{url,label}}` | connect a calendar |
| DELETE | `/api/cal?feedId=` | disconnect a calendar |

Times are ISO-8601 UTC strings.

## Layout

```
Sources/DateWizard/
  main.swift             NSApplication bootstrap
  AppDelegate.swift      window + WizardController; ensure auth then load week
  WizardController.swift owns state (agenda, events, token); refresh/editor/sign-in screen
  AgendaView.swift       chronological, scrollable upcoming appointments list
  WeekView.swift         deprecated grid implementation retained for shared event types
  EventEditor.swift      native sheet: Title/Start/End/Note/privacy + Add/Save/Delete
  CalAPI.swift           URLSession networking + Codable models
  Auth.swift             reads the shared AC session token (~/.ac-token) from ac-login
```

## Notes / TODO

- Feed management is connect-only in v1. The backend supports
  `DELETE /api/cal?feedId=<id>` and `GET /api/cal?feeds=1`; `CalAPI` already
  wires `removeFeed`/`fetchFeeds`, but there is no UI to list/disconnect feeds
  yet. **TODO:** add a feeds manager sheet.
- All-day events render as a slim band at the top of their column; recurring
  events (`rrule`) are drawn only on their first occurrence (the backend
  returns base events, not expanded instances). **TODO:** expand `rrule`.
- The editor always writes `allDay:false`. **TODO:** surface an all-day toggle.
- No dock icon assets bundle (unlike wave-wizard); the executable uses the
  default app icon. **TODO (optional):** add an Assets bundle + DockIcon.
