# choir components

Use these shared pieces rather than writing new one-off styles, so every screen
looks and behaves the same. jeffrey (2026-10-12) asked for "a proper styled
component library in the app, so we can ensure correctness from here on".
When a screen needs something new, add it here first, then use it.

## Color and type (`Sources/Theme.swift`)

| Token | Use |
|---|---|
| `Theme.desk` `#E2DBCC` | the paper behind everything (also choir.ac `--desk` and the launch screen) |
| `Theme.paper` | cards and bubbles |
| `Theme.ink` | text and lines |
| `Theme.head` | the red accent: destructive actions and the voice-note progress |
| `Theme.type(size, bold:)` | the app's typewriter face |

Logo art comes only from `marketing/choir/logo/brand.py`. See `marketing/choir/logo/BRAND.md`.

## Buttons (`Theme.swift`)

| Style | Where |
|---|---|
| `SendStyle(ready:)` | Send and Reply: plain bold type, ink pill when ready, never faded |
| `CuteDoorStyle(new:, small:)` | Log in / I'm new, and other big friendly doors |
| `CircleChipStyle` | the round glass buttons beside the text box (mic, +) |
| `GlassChipStyle(on:)` | small toggles and filters |
| `DeskButtonStyle` | quiet desk-style buttons |
| `ConfirmButtonStyle(kind:)` (`Components.swift`) | the two buttons in a `.confirm` card |

## Pieces (`Sources/Components.swift` and friends)

| Component | Use |
|---|---|
| `.confirm($flag, "Title?", detail:, action:) { … }` | Anything irreversible, like deleting a message. Never use `confirmationDialog` from a long-press menu: it reopens as a second menu. |
| `.reportDialog($flag, post:)` (`Moderation.swift`) | Reporting a message. Attach it to its own view (`.background(Color.clear.reportDialog…)`), never stacked with another dialog on the same view. |
| `TermsSheet` (`Moderation.swift`) | Agreeing to choir.ac/terms once per version |
| `SingingMark(height:, energy:)` | the logo, breathing |
| `FlowerSpinner(size:)` | any loading state, in place of `ProgressView` in choir screens |
| `Hand("HandStar")` and others (`Hand.swift`) | Fía's hand-drawn glyphs |
| `PresenceLine`, `GreetingLine` (`RoomView.swift`) | the room's status lines |

## Rules

- **One dialog per view.** SwiftUI shows only one `alert`/`confirmationDialog`
  per view. A second one goes on its own `.background(Color.clear…)` view.
- **No new colors or fonts inline.** Add a token to `Theme` if one is missing.
- **Loading means the flower.** Use `FlowerSpinner`, not a bare `ProgressView`.
- **Irreversible means `.confirm`**, with a one-line `detail` that says what happens.
