# Ant Score Entry

This is the ant-local score entry point.

- Main score and task list source: `../SCORE.md`
- Ant mindset and rules source: `./mindset-and-rules.md`

Runtime note:
- `ants/colony.fish` composes both sources (ant mindset/rules first, then main score/tasks) when building the ant prompt.

---

## Current Tasks

> **The Queen ([@jeffrey](https://prompt.ac/@jeffrey)) maintains this list. Agents pick from it.**

### Agent Memory Logging (automatic — @jeffrey's sessions)

When @jeffrey is working, prompts are logged to local encrypted agent memory via `memory/hook.mjs` (`~/.ac-agent-memory`).

### Tier 1: Safe & Small (ant-appropriate)

- [ ] Run `npm test` and fix any failing tests (one at a time)
- [ ] Find and fix lint warnings in `system/public/aesthetic.computer/disks/*.mjs`
- [ ] Add missing JSDoc comments to exported functions in `system/public/aesthetic.computer/lib/`
- [ ] Check `package.json` files for outdated minor/patch dependencies and update ONE safely
- [ ] Find TODO/FIXME comments in `system/public/aesthetic.computer/lib/` and resolve simple ones

### Tier 2: Slightly Braver

- [ ] Add a small test for any untested utility function in `shared/`
- [ ] Improve error messages in KidLisp interpreter for common mistakes
- [ ] Find dead code (unused exports/functions) and remove it with confidence

### Tier 3: Need Coordination

- [ ] Performance improvements (profile first, discuss approach)
- [ ] New features (propose in `chat` or GitHub issues)
- [ ] Architectural changes (needs queen approval)
