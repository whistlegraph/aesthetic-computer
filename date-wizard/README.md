# DateWizard

A native macOS AppKit pop-up that shows your **week** and lets you schedule
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

DateWizard uses your shared AC session from `ac-login` (`~/.ac-token`) — the
same login `ac-os` uses. Run `ac-login` once to sign in.

On first run (or after the token expires and a request comes back `401`), it
shows a **sign-in screen**:

- **Sign in (run ac-login)** launches `ac-login` in Terminal so you can
  authenticate without leaving the app.
- It then auto-polls `~/.ac-token` every ~2s and proceeds automatically once a
  valid token appears. (Or click **I've signed in** to check immediately.)

The token file is JSON written by the AC stack:

```
~/.ac-token   # { "access_token": "<jwt>", "refresh_token": "...", "expires_at": <ms-epoch> }
```

`access_token` is sent as `Authorization: Bearer …` on every `/api/cal` call.
The token is treated as expired when the file is missing, unparseable, or
`expires_at` is in the past — at which point you're prompted to run `ac-login`
again.

## Using the week

- **‹ / ›** page weeks; **Today** jumps back to the current week.
- **Refresh** reloads both editable + connected-calendar events.
- **+ Event** (or clicking an empty slot) opens the editor sheet.
- **Aesthetical events** draw with a solid fill (green = public, accent =
  private) and are **editable** — click one to edit or delete it.
- **Google / connected-calendar events** draw dashed with a source-color dot
  and are **read-only** — clicking shows details, no edit.
- A red **now** line marks the current time in today's column.
- A small **legend** along the bottom maps fill colors to sources.

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
  WizardController.swift owns state (week, events, token); paging/refresh/editor/sign-in screen
  WeekView.swift         custom NSView: 7-column Sun→Sat grid, hour rows, now line, legend
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
