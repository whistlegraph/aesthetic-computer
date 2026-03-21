# Git Commit Status Indicator for Prompt Curtain

## Overview
Add a visual indicator to the prompt.mjs curtain UI showing whether the deployed site is on the latest git commit or behind, with the commit hash displayed.

## Visual Design
- **Location**: Below "X HANDLES SET" text on the login curtain
- **Font**: MatrixChunky8 (same as handles counter)
- **Colors**:
  - 🟢 Green (`[0, 255, 0]`) = Site is up-to-date with latest commit
  - 🟠 Orange (`[255, 165, 0]`) = Site is behind (shows how many commits)
- **Format Examples**:
  - `✓ 37da247` (current)
  - `↑ 2 behind (37da247)` (behind by 2 commits)

---

## Implementation Steps

### 1. Create Build-Time Version File
**File**: `system/public/version.json` (generated at build time)

Add to `netlify.toml` build command:
```toml
command = "rm -f public/index.html && echo '{\"commit\":\"'$COMMIT_REF'\",\"timestamp\":\"'$(date -u +%Y-%m-%dT%H:%M:%SZ)'\"}' > public/version.json"
```

This creates a file like:
```json
{"commit":"37da2475abc123...","timestamp":"2026-01-11T22:34:06Z"}
```

### 2. Create Netlify Function: `/api/version`
**File**: `system/netlify/functions/version.mjs`

```javascript
// Fetches latest commit from GitHub and compares to deployed version
export default async (request) => {
  const deployedCommit = process.env.COMMIT_REF || "unknown";
  
  try {
    // Fetch latest commit from GitHub (public, no auth needed)
    const res = await fetch(
      "https://api.github.com/repos/whistlegraph/aesthetic-computer/commits?per_page=50",
      { headers: { "User-Agent": "aesthetic-computer" } }
    );
    const commits = await res.json();
    const latestCommit = commits[0]?.sha;
    
    // Find how many commits behind
    let behindBy = 0;
    if (deployedCommit !== "unknown" && latestCommit) {
      const idx = commits.findIndex(c => c.sha.startsWith(deployedCommit.slice(0, 7)));
      behindBy = idx === -1 ? 50 : idx; // -1 means very old
    }
    
    const status = behindBy === 0 ? "current" : "behind";
    
    return Response.json({
      deployed: deployedCommit.slice(0, 7),
      latest: latestCommit?.slice(0, 7),
      status,
      behindBy,
      timestamp: new Date().toISOString()
    });
  } catch (e) {
    return Response.json({ 
      deployed: deployedCommit.slice(0, 7), 
      status: "unknown",
      error: e.message 
    });
  }
};

export const config = { path: "/api/version" };
```

### 3. Add Redirect in `netlify.toml`
```toml
[[redirects]]
from = "/api/version"
to = "/.netlify/functions/version"
status = 200
```

### 4. Update `prompt.mjs`

#### A. Add State Variables (~line 230)
```javascript
let versionInfo = null; // { deployed, latest, status, behindBy }
```

#### B. Fetch in `boot()` (~line 640, after handles fetch)
```javascript
// Fetch commit/version status
const fetchVersion = async () => {
  try {
    const res = await fetch("/api/version");
    versionInfo = await res.json();
    needsPaint();
  } catch (e) { 
    console.warn("Could not fetch version info:", e); 
  }
};
fetchVersion();
// Refresh every 5 minutes
setInterval(fetchVersion, 5 * 60 * 1000);
```

#### C. Render in `paint()` (~line 6081, after handles text)
```javascript
// Git commit status indicator
if (versionInfo && screen.height >= 120) {
  const versionY = handlesY + 12;
  let versionText, versionColor;
  
  if (versionInfo.status === "current") {
    versionColor = [0, 255, 0]; // Green
    versionText = `✓ ${versionInfo.deployed}`;
  } else if (versionInfo.status === "behind") {
    versionColor = [255, 165, 0]; // Orange
    versionText = `↑ ${versionInfo.behindBy} behind (${versionInfo.deployed})`;
  } else {
    versionColor = [128, 128, 128]; // Gray for unknown
    versionText = `? ${versionInfo.deployed || "unknown"}`;
  }
  
  ink(versionColor).write(
    versionText,
    { center: "x", y: versionY },
    undefined, undefined, false, "MatrixChunky8"
  );
}
```

---

## File Changes Summary

| File | Change |
|------|--------|
| `system/netlify.toml` | Update build command to generate version.json; add redirect |
| `system/netlify/functions/version.mjs` | New function (create) |
| `system/public/aesthetic.computer/disks/prompt.mjs` | Add state var, boot fetch, paint render |

---

## Technical Notes

### GitHub API
- **Endpoint**: `GET https://api.github.com/repos/whistlegraph/aesthetic-computer/commits`
- **Rate limit**: 60 requests/hour unauthenticated (sufficient with 5-min polling + server-side caching)
- **No auth required** for public repos

### Netlify Environment Variables
- `COMMIT_REF` - Full SHA of the commit being deployed (available at build/function time)
- `BUILD_ID` - Unique build identifier

### Caching Strategy
The Netlify function can add cache headers to reduce GitHub API calls:
```javascript
return new Response(JSON.stringify(data), {
  headers: {
    "Content-Type": "application/json",
    "Cache-Control": "public, max-age=60" // Cache for 1 minute
  }
});
```

---

## Future Enhancements
- Click on commit hash to open GitHub commit page
- Show commit message preview on hover
- Add "Update available" notification badge
- Track deploy history / show last N deploys
