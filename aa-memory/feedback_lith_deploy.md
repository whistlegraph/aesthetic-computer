---
name: Always deploy to lith after compush
description: After every git push, run fish lith/deploy.fish to deploy to production
type: feedback
---

Always run `fish lith/deploy.fish` after pushing to git (compush). The user expects the full compush flow to include both the git push AND the lith deployment in one go.

**Why:** The user was surprised I didn't deploy automatically — lith is the production server and a push without deploy leaves the live site stale.

**How to apply:** Any time the user says "compush" or asks to commit+push, finish by running `fish lith/deploy.fish` unless they explicitly say otherwise.
