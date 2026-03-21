# Patch Command - Admin PR Agent Spawner

## Overview

The `patch` command is an admin-only feature that allows authorized users (specifically @jeffrey) to spawn GitHub Copilot coding agents directly from the aesthetic.computer web client.

## Architecture

### Client-Side (prompt.mjs)
- **Command**: `patch <prompt/instruction>`
- **Location**: After other admin commands in the command handler
- **Authentication**: Requires login
- **Feedback**: Uses the existing notice/flash system for status updates

### Server-Side (netlify/functions/patch.mjs)
- **Endpoint**: `POST /api/patch`
- **Authentication**: Uses Auth0 via `authorize()` from authorization.mjs
- **Admin Check**: Uses `hasAdmin()` which validates:
  - User is logged in
  - Email is verified  
  - Handle is "jeffrey"
  - User sub matches `ADMIN_SUB` environment variable

### GitHub Integration
- Uses `@octokit/rest` to interact with GitHub API
- Creates an issue in the whistlegraph/aesthetic-computer repository
- Labels: `copilot`, `auto-patch`
- Dispatches a `copilot-patch` repository event for agent triggering

## Environment Variables Required

| Variable | Description |
|----------|-------------|
| `GITHUB_COPILOT_TOKEN` | GitHub PAT with repo and workflow permissions |
| `ADMIN_SUB` | Auth0 sub ID for the admin user |

## Usage

```
patch fix the typo in README.md
patch add a new color palette to the wand piece
patch update the chat component to handle emoji better
```

## Security Model

1. **Authentication Layer**: Auth0 bearer token required
2. **Email Verification**: User's email must be verified
3. **Handle Whitelist**: Only @jeffrey can execute
4. **Sub Validation**: User's Auth0 sub must match ADMIN_SUB env var

This multi-layer security ensures that even if someone obtains a valid auth token, they cannot spawn PR agents without being the designated admin user.

## Response Format

### Success
```json
{
  "success": true,
  "message": "PR agent spawned successfully",
  "data": {
    "jobId": "issue-123",
    "status": "queued",
    "url": "https://github.com/whistlegraph/aesthetic-computer/issues/123"
  }
}
```

### Error (Unauthorized)
```json
{
  "success": false,
  "message": "Admin privileges required. Only @jeffrey can use this command."
}
```

## Implementation Date
2026-02-01

## Files Changed
- `system/netlify/functions/patch.mjs` - New netlify function
- `system/public/aesthetic.computer/disks/prompt.mjs` - Added `patch` command handler
- `system/package.json` - Added `@octokit/rest` dependency
