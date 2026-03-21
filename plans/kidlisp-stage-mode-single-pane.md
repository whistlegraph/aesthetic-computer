# KidLisp Stage Mode (Single-Pane Live Coding)

**Status:** Planning  
**Created:** 2026-03-02

## Naming Recommendation

Use **Stage Mode**.

- `live mode` is clear but generic (can also mean live reload).
- `concert mode` is fun but less obvious for new users.
- `stage mode` reads as performance-focused and still simple in settings UI.

Suggested label in UI: `Stage (Live Coding)`

## Goal

Add a global editor setting for kidlisp.com that switches from normal authoring layout to a performance layout:

1. Hide Console and Learn surfaces.
2. Combine Monaco + AC output into one single “stage pane.”
3. Toggle from the existing Preferences modal.
4. Persist globally (localStorage) and restore on load.

## Product Behavior

### Studio Mode (default)
- Current layout and behavior unchanged.

### Stage Mode (new)
- Hide bottom row (`#bottom-row`) entirely (reference + console panels).
- Hide Learn/Console tab affordances in case mobile/tab state leaks.
- Replace 4-panel/split interaction with one stage pane:
  - AC output is full-bleed background.
  - Monaco is overlaid in the same pane (left-anchored, semi-opaque, performance-readable).
- Keep Play/Stop and preview title controls accessible.
- Keep mode switch reversible without page reload.

## Technical Approach

### 1. Persisted global setting
- New key: `kidlisp-ui-mode` with values: `studio | stage`.
- Add helpers:
  - `getUiMode()`
  - `setUiMode(mode)`
  - `applyUiMode(mode)`

### 2. Preferences modal integration
- Extend Preferences modal with a new **Workspace** section:
  - `Studio`
  - `Stage (Live Coding)`
- On click:
  - persist mode,
  - call `applyUiMode`,
  - refresh modal active state.

### 3. Layout orchestration
- In `applyUiMode('stage')`:
  - add `body.stage-mode`,
  - short-circuit Split.js layout flow (`initSplits`) to avoid gutters/resizers,
  - enforce single-pane DOM structure with preview + Monaco overlay in same container.
- In `applyUiMode('studio')`:
  - remove `body.stage-mode`,
  - restore standard split layout (`initSplits()`),
  - restore tab visibility and panel behavior.

### 4. CSS mode layer
- Add scoped CSS rules under `body.stage-mode`:
  - hide `#bottom-row`, `#center-square`, and gutters,
  - make `#top-row` fill viewport height,
  - make preview full pane,
  - position editor container as overlay with controlled width/opacity/shadow,
  - mobile-specific simplification (full-width editor overlay toggle).

### 5. Tab/content guardrails
- In tab activation helpers, no-op console/guide activation while in `stage` mode.
- Ensure initial tab boot logic does not force Learn/Console visible when stage mode is active.

## File Touchpoints

- `system/public/kidlisp.com/index.html`
  - Preferences modal construction and handlers
  - Split/layout initialization (`initSplits`)
  - Tab switching setup (guide/console/list/etc.)
  - Inline mode CSS additions (`body.stage-mode`)
- Optional cleanup follow-up:
  - `system/public/kidlisp.com/css/layout.css`
  - `system/public/kidlisp.com/css/mobile.css`

## Implementation Phases

### Phase 1: State + toggle plumbing
- [ ] Add mode state + localStorage persistence.
- [ ] Add Workspace section to Preferences modal.
- [ ] Wire toggle handlers and active-state rendering.

### Phase 2: Stage layout mode
- [ ] Add `body.stage-mode` styles.
- [ ] Implement single-pane composition (preview + Monaco overlay).
- [ ] Disable Split.js/gutter behaviors in stage mode.

### Phase 3: UX hardening
- [ ] Guard tab activation (Learn/Console hidden/inert in stage mode).
- [ ] Ensure resize/orientation changes preserve mode correctly.
- [ ] Ensure editor `layout()` is called after mode switch.

### Phase 4: QA pass
- [ ] Desktop: toggle back/forth repeatedly without layout drift.
- [ ] Mobile: no broken tabs/panels when stage mode is on.
- [ ] Playback: play/pause/stop still works in stage mode.
- [ ] Persistence: refresh keeps selected mode.

## Acceptance Criteria

1. User can enable Stage mode from Preferences without reload.
2. Console and Learn surfaces are hidden in Stage mode.
3. Monaco and AC output appear in one combined pane.
4. Mode persists across refresh.
5. Switching back to Studio fully restores normal split layout.

## Risks / Notes

- `index.html` is large and stateful; mode transitions should avoid destructive DOM rebuilds where possible.
- Existing mobile merge behavior (console into reference tab) can conflict with stage assumptions; enforce explicit stage guard in mobile setup.
- Keep this as a layout mode, not a theme token, to avoid coupling with light/dark theme logic.

