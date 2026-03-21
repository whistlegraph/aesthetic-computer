# Unified Build System Plan for builds.false.work

## Current Build Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                    Dev Container (Linux)                           │
│                                                                    │
│  ┌─────────────────────┐   SSH to host.docker.internal             │
│  │ windows/             │ ─────────────────────────────────┐       │
│  │ remote-update-and-  │                                   │       │
│  │ build.fish          │                                   ▼       │
│  └─────────────────────┘                        ┌──────────────────┐
│                                                 │ Windows Host PC  │
│  ┌─────────────────────┐   SSH to Mac mini     │ (your machine)   │
│  │ unreal-builder/     │ ─────────────────────┐└──────────────────┘
│  │ remote-mac-build.   │                       │
│  │ fish                │                       ▼
│  └─────────────────────┘            ┌──────────────────┐
│                                     │ Mac Mini         │
│  ┌─────────────────────┐            │ (remote)         │
│  │ unreal-builder/     │ ───────────└──────────────────┘
│  │ remote-ios-build.   │   SSH to Mac mini
│  │ fish                │
│  └─────────────────────┘
└────────────────────────────────────────────────────────────────────┘
```

---

## Problem Analysis

### Current State
The builds.false.work page is currently managed via **direct HTML manipulation** using AWK scripts that insert new build entries. This causes several issues:

1. **Layout Jank**: Each of the 3 platform-specific build scripts (Mac, Windows, iOS) modifies `index.html` directly via `update-builds-page.fish`, inserting new HTML blocks with AWK. This creates:
   - Duplicated "Previous" headers
   - Inconsistent date groupings
   - Broken layout when builds from different platforms are added sequentially

2. **Three Separate Build Scripts**:
   - `unreal-builder/scripts/remote-mac-build.fish` - Builds on Mac mini via SSH
   - `unreal-builder/scripts/remote-ios-build.fish` - Builds iOS on Mac mini via SSH  
   - `windows/remote-update-and-build.fish` - SSH to local Windows host (`host.docker.internal`)

3. **HTML Update Mechanism Issues**:
   - `unreal-builder/scripts/shared/update-builds-page.fish` uses AWK to inject HTML
   - Each build inserts its own date header and "Previous" section
   - No deduplication or sorting logic
   - Results in stacked redundant headers (visible in current `index.html`)

### Evidence of Layout Jank
Looking at `system/public/builds.false.work/index.html`:
```html
<!-- Multiple redundant "Previous" headers stacked: -->
<h3 class="date-header" style="color: #888; ...">Previous</h3>
  <h3 class="date-header" style="color: #fa0; ...">Today</h3>
```

---

## Proposed Solution

### Architecture: MongoDB-Driven Builds Page

Instead of a static JSON file (which could be fetched publicly), we'll store builds in MongoDB behind authenticated Netlify functions. This provides:
- **Real security** - builds data only accessible after password verification
- **Proper database** - querying, filtering, retention policies built-in
- **Consistency** - same MongoDB instance used by rest of aesthetic-computer

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    MongoDB (aesthetic database)                              │
│                                                                              │
│  Collection: false.work-builds                                               │
│  ┌─────────────────────────────────────────────────────────────────────────┐ │
│  │ {                                                                        │ │
│  │   "_id": ObjectId("..."),                                               │ │
│  │   "platform": "ios",                                                    │ │
│  │   "version": "2025.11.26-1019",                                        │ │
│  │   "timestamp": ISODate("2025-11-26T10:19:00Z"),                        │ │
│  │   "sizeMB": 879,                                                        │ │
│  │   "level": "L_VerticalSlice_Demo",                                     │ │
│  │   "ueVersion": "UE_5.6",                                               │ │
│  │   "downloadUrl": "https://...",                                         │ │
│  │   "logUrl": "https://...",                                              │ │
│  │   "changelist": "CL#689",                                               │ │
│  │   "buildType": "device"                                                 │ │
│  │ }                                                                        │ │
│  └─────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                              │
          ┌───────────────────┴───────────────────┐
          │                                       │
          ▼                                       ▼
┌─────────────────────────────────┐   ┌─────────────────────────────────┐
│ Netlify Function:               │   │ Netlify Function:               │
│ get-builds.mjs                  │   │ register-build.mjs              │
│                                 │   │                                 │
│ - Requires password in header   │   │ - Requires API key              │
│ - Returns builds as JSON        │   │ - Inserts new build             │
│ - Supports filtering by platform│   │ - Called by build scripts       │
└─────────────────────────────────┘   └─────────────────────────────────┘
          │                                       ▲
          ▼                                       │
┌─────────────────────────────────┐   ┌─────────────────────────────────┐
│ index.html (Static Shell)       │   │ Build Scripts                   │
│                                 │   │                                 │
│ - Authentication UI             │   │ - remote-mac-build.fish         │
│ - After auth, fetches builds    │   │ - remote-ios-build.fish         │
│ - JavaScript renders cards      │   │ - remote-update-and-build.fish  │
│ - Sorts by timestamp            │   │                                 │
│ - Groups by date                │   │ All call register-build.mjs     │
└─────────────────────────────────┘   └─────────────────────────────────┘
```

### Benefits
1. **Real security** - Builds data requires authentication, not publicly fetchable
2. **No more layout jank** - JavaScript renders consistently every time
3. **Simpler scripts** - Build scripts call a Netlify function, no AWK/git needed
4. **Proper sorting** - MongoDB sorts by timestamp
5. **Proper grouping** - Date headers generated dynamically with deduplication
6. **Platform filtering** - Could easily add filter buttons (show Windows only, etc.)
7. **Query capabilities** - Find builds by platform, date range, etc.
8. **Retention policy** - MongoDB TTL indexes can auto-delete old builds
9. **No git commits for builds** - Build registration doesn't touch the repo

---

## Implementation Plan

### Phase 1: Create MongoDB Collection and Netlify Functions

**Collection**: `false.work-builds` in the `aesthetic` database

**Schema**:
```javascript
{
  _id: ObjectId,
  platform: "windows" | "mac" | "ios",
  version: "2025.11.26-1019",        // Build version string
  timestamp: ISODate,                 // When build completed
  sizeMB: Number,                     // File size in MB
  level: String,                      // Start level (e.g., "L_VerticalSlice_Demo")
  ueVersion: String,                  // Unreal Engine version
  downloadUrl: String,                // CDN download URL
  logUrl: String | null,              // Build log URL (optional)
  changelist: String | null,          // Perforce CL (optional)
  buildType: String | null            // "device" | "simulator" for iOS
}
```

**Index**: `{ timestamp: -1 }` for efficient sorting

---

### Phase 2: Create Netlify Functions

**File**: `system/netlify/functions/get-builds.mjs`

```javascript
import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";
const BUILDS_PASSWORD = process.env.BUILDS_PASSWORD;

export default async (req, context) => {
  // Verify password from Authorization header
  const authHeader = req.headers.get("Authorization");
  const password = authHeader?.replace("Bearer ", "");
  
  if (password !== BUILDS_PASSWORD) {
    return new Response(JSON.stringify({ error: "Unauthorized" }), {
      status: 401,
      headers: { "Content-Type": "application/json" }
    });
  }

  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  
  try {
    await client.connect();
    const db = client.db(MONGODB_NAME);
    const builds = db.collection("false.work-builds");
    
    // Get query params for filtering
    const url = new URL(req.url);
    const platform = url.searchParams.get("platform");
    const limit = parseInt(url.searchParams.get("limit")) || 50;
    
    const query = platform ? { platform } : {};
    
    const results = await builds
      .find(query)
      .sort({ timestamp: -1 })
      .limit(limit)
      .toArray();
    
    return new Response(JSON.stringify({ builds: results }), {
      status: 200,
      headers: { "Content-Type": "application/json" }
    });
  } finally {
    await client.close();
  }
};
```

**File**: `system/netlify/functions/register-build.mjs`

```javascript
import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";
const BUILDS_API_KEY = process.env.BUILDS_API_KEY;  // Separate key for build scripts

export default async (req, context) => {
  if (req.method !== "POST") {
    return new Response(JSON.stringify({ error: "Method not allowed" }), {
      status: 405,
      headers: { "Content-Type": "application/json" }
    });
  }

  // Verify API key
  const apiKey = req.headers.get("X-API-Key");
  if (apiKey !== BUILDS_API_KEY) {
    return new Response(JSON.stringify({ error: "Unauthorized" }), {
      status: 401,
      headers: { "Content-Type": "application/json" }
    });
  }

  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  
  try {
    const build = await req.json();
    
    // Validate required fields
    const required = ["platform", "version", "timestamp", "sizeMB", "downloadUrl"];
    for (const field of required) {
      if (!build[field]) {
        return new Response(JSON.stringify({ error: `Missing required field: ${field}` }), {
          status: 400,
          headers: { "Content-Type": "application/json" }
        });
      }
    }
    
    await client.connect();
    const db = client.db(MONGODB_NAME);
    const builds = db.collection("false.work-builds");
    
    // Convert timestamp string to Date
    build.timestamp = new Date(build.timestamp);
    
    // Check for duplicate (same platform + version)
    const existing = await builds.findOne({ 
      platform: build.platform, 
      version: build.version 
    });
    
    if (existing) {
      return new Response(JSON.stringify({ 
        error: "Build already exists",
        existingId: existing._id 
      }), {
        status: 409,
        headers: { "Content-Type": "application/json" }
      });
    }
    
    const result = await builds.insertOne(build);
    
    return new Response(JSON.stringify({ 
      success: true,
      insertedId: result.insertedId,
      build 
    }), {
      status: 201,
      headers: { "Content-Type": "application/json" }
    });
  } catch (error) {
    return new Response(JSON.stringify({ error: error.message }), {
      status: 500,
      headers: { "Content-Type": "application/json" }
    });
  } finally {
    await client.close();
  }
};
```

---

### Phase 3: Update index.html to Fetch from MongoDB

Replace the static build list with JavaScript that:
1. Shows password prompt (existing behavior)
2. After auth, stores password in memory (not localStorage for security)
3. Fetches builds from `/.netlify/functions/get-builds` with password in header
4. Renders platform-appropriate cards
5. Handles iOS OTA install links

---

### Phase 4: Create Shared Build Registration Script

**File**: `false.work/unreal-builder/scripts/shared/register-build.fish`

```fish
#!/usr/bin/env fish
# Register a new build via Netlify function (stores in MongoDB)
# Usage: register-build.fish PLATFORM VERSION TIMESTAMP SIZE_MB DOWNLOAD_URL [LEVEL] [UE_VERSION] [LOG_URL] [CHANGELIST] [BUILD_TYPE]

function register_build
    set platform $argv[1]
    set version $argv[2]
    set timestamp $argv[3]
    set size_mb $argv[4]
    set download_url $argv[5]
    set level $argv[6]
    set ue_version $argv[7]
    set log_url $argv[8]
    set changelist $argv[9]
    set build_type $argv[10]
    
    # Load API key from vault
    set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault
    set API_KEY (cat $VAULT_DIR/false.work/builds-api-key.txt 2>/dev/null)
    
    if test -z "$API_KEY"
        echo "❌ BUILDS_API_KEY not found in vault"
        return 1
    end
    
    # Build JSON payload
    set json_payload (printf '{
        "platform": "%s",
        "version": "%s",
        "timestamp": "%s",
        "sizeMB": %s,
        "downloadUrl": "%s",
        "level": %s,
        "ueVersion": %s,
        "logUrl": %s,
        "changelist": %s,
        "buildType": %s
    }' \
        "$platform" \
        "$version" \
        "$timestamp" \
        "$size_mb" \
        "$download_url" \
        (test -n "$level" && echo "\"$level\"" || echo "null") \
        (test -n "$ue_version" && echo "\"$ue_version\"" || echo "null") \
        (test -n "$log_url" && echo "\"$log_url\"" || echo "null") \
        (test -n "$changelist" && echo "\"$changelist\"" || echo "null") \
        (test -n "$build_type" && echo "\"$build_type\"" || echo "null"))
    
    # Call Netlify function
    set response (curl -s -w "\n%{http_code}" \
        -X POST \
        -H "Content-Type: application/json" \
        -H "X-API-Key: $API_KEY" \
        -d "$json_payload" \
        "https://aesthetic.computer/.netlify/functions/register-build")
    
    set http_code (echo "$response" | tail -1)
    set body (echo "$response" | sed '$d')
    
    if test "$http_code" = "201"
        echo "✅ Build registered: $platform $version"
        return 0
    else if test "$http_code" = "409"
        echo "⚠️  Build already exists: $platform $version"
        return 0
    else
        echo "❌ Failed to register build (HTTP $http_code): $body"
        return 1
    end
end
```

### Phase 4: Update Platform Build Scripts

Replace AWK-based HTML updates with calls to `register-build.fish`:

**remote-mac-build.fish** changes:
```fish
# OLD:
source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/update-builds-page.fish
update_builds_page "mac" "$build_version" ...
git add system/public/builds.false.work/index.html
git commit -m "Add SpiderLily Mac build $build_version"
git push

# NEW:
source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/register-build.fish
register_build "mac" "$build_version" "$iso_timestamp" "$file_size" "$download_url" "$start_level" "$ue_version" "$log_url"
# No git commit needed! Build is registered in MongoDB
```

---

### Phase 5: Unified Build Controller Script 🎯

**File**: `false.work/unreal-builder/scripts/build.fish`

A single entry point that can:
1. Detect the host platform automatically
2. Accept platform as argument
3. SSH to appropriate build machine
4. Execute the build
5. Register the build in JSON
6. Upload and deploy

```fish
#!/usr/bin/env fish
# Unified SpiderLily Build Controller
# Usage:
#   ./build.fish              # Interactive - prompts for platform
#   ./build.fish windows      # Build Windows
#   ./build.fish mac          # Build Mac
#   ./build.fish ios          # Build iOS (device)
#   ./build.fish ios-sim      # Build iOS Simulator
#   ./build.fish all          # Build all platforms

set script_dir (dirname (status filename))

function detect_host_platform
    if test (uname) = "Darwin"
        echo "mac"
    else if test -n "$WSLENV" -o -d /mnt/c/Windows
        echo "windows"
    else
        echo "linux"  # Dev container
    end
end

function show_menu
    echo "╔════════════════════════════════════════╗"
    echo "║   SpiderLily Build Controller          ║"
    echo "╠════════════════════════════════════════╣"
    echo "║  1) 🪟  Windows                        ║"
    echo "║  2) 🍎  Mac                            ║"
    echo "║  3) 📱  iOS (Device)                   ║"
    echo "║  4) 📱  iOS (Simulator)                ║"
    echo "║  5) 🌐  All Platforms                  ║"
    echo "║  q) Exit                               ║"
    echo "╚════════════════════════════════════════╝"
    echo ""
    echo -n "Select platform: "
end

function build_windows
    echo "🪟 Starting Windows build..."
    # Always runs via SSH to local Windows host from devcontainer
    fish /workspaces/aesthetic-computer/windows/remote-update-and-build.fish
end

function build_mac
    echo "🍎 Starting Mac build..."
    fish "$script_dir/remote-mac-build.fish"
end

function build_ios
    set build_type $argv[1]
    echo "📱 Starting iOS build ($build_type)..."
    fish "$script_dir/remote-ios-build.fish" $build_type
end

# Main
set platform $argv[1]

if test -z "$platform"
    # Interactive mode
    show_menu
    read choice
    switch $choice
        case 1 windows
            set platform "windows"
        case 2 mac
            set platform "mac"
        case 3 ios
            set platform "ios"
        case 4 ios-sim
            set platform "ios-sim"
        case 5 all
            set platform "all"
        case q
            exit 0
        case '*'
            echo "Invalid choice"
            exit 1
    end
end

switch $platform
    case windows
        build_windows
    case mac
        build_mac
    case ios
        build_ios "device"
    case ios-sim
        build_ios "simulator"
    case all
        echo "Building all platforms..."
        build_windows
        build_mac
        build_ios "device"
    case '*'
        echo "Unknown platform: $platform"
        echo "Valid options: windows, mac, ios, ios-sim, all"
        exit 1
end
```

---

## File Changes Summary

| File | Action | Description |
|------|--------|-------------|
| `system/netlify/functions/get-builds.mjs` | **CREATE** | Netlify function to fetch builds from MongoDB |
| `system/netlify/functions/register-build.mjs` | **CREATE** | Netlify function to insert new builds |
| `system/public/builds.false.work/index.html` | **MODIFY** | Replace static HTML with MongoDB-driven rendering |
| `false.work/unreal-builder/scripts/shared/register-build.fish` | **CREATE** | New shared function to register builds via API |
| `false.work/unreal-builder/scripts/shared/update-builds-page.fish` | **DELETE** | Remove old AWK-based HTML updater |
| `false.work/unreal-builder/scripts/remote-mac-build.fish` | **MODIFY** | Use `register-build.fish`, remove git commit |
| `false.work/unreal-builder/scripts/remote-ios-build.fish` | **MODIFY** | Use `register-build.fish`, remove git commit |
| `windows/remote-update-and-build.fish` | **MODIFY** | Use `register-build.fish`, remove git commit |
| `false.work/unreal-builder/scripts/windows-build-and-upload.ps1` | **DELETE** | Old GCP VM script (no longer used) |
| `false.work/unreal-builder/scripts/build.fish` | **CREATE** | Unified build controller |
| `aesthetic-computer-vault/false.work/builds-api-key.txt` | **CREATE** | API key for build registration |

---

## Migration Steps

1. **Create `BUILDS_API_KEY`** env var in Netlify and save to vault
2. **Create Netlify functions** (`get-builds.mjs`, `register-build.mjs`)
3. **Create MongoDB collection** `false.work-builds` with index on `timestamp`
4. **Migrate existing builds** from current HTML to MongoDB (one-time script)
5. **Update `index.html`** to fetch and render from API
6. **Create `register-build.fish`** shared function
7. **Update Mac build script** to use new registration (remove git commit)
8. **Update iOS build script** to use new registration (remove git commit)
9. **Update Windows build script** to use new registration (remove git commit)
10. **Create unified `build.fish`** controller
11. **Test each platform individually**
12. **Remove old `update-builds-page.fish`**

---

## Future Enhancements

### Platform Filter UI
```html
<div class="platform-filters">
  <button data-platform="all" class="active">All</button>
  <button data-platform="windows">🪟 Windows</button>
  <button data-platform="mac">🍎 Mac</button>
  <button data-platform="ios">📱 iOS</button>
</div>
```

### Build Retention Policy
- MongoDB TTL index can auto-delete builds older than X days
- Or keep last N builds per platform via aggregation

### Webhook Integration
- GitHub Actions could trigger builds
- Discord notifications on new builds

### Admin Functions
- `delete-build.mjs` - Remove a build by ID
- `update-build.mjs` - Edit build metadata

---

## Environment Variables

Add to Netlify:
- `BUILDS_API_KEY` - Secret key for build scripts to register builds

Already exists:
- `BUILDS_PASSWORD` - Password for web UI access
- `MONGODB_CONNECTION_STRING` - MongoDB connection
- `MONGODB_NAME` - Database name ("aesthetic")

---

## Security Model

| Actor | Access Method | What They Can Do |
|-------|---------------|------------------|
| Web UI User | Password → `get-builds.mjs` | View builds list |
| Build Script | API Key → `register-build.mjs` | Add new builds |
| Public | None | Cannot access builds data |

The builds data is **never publicly accessible** - it always requires either the password (for viewing) or the API key (for writing).

---

## Dependencies

- **MongoDB** - Already used by aesthetic-computer
- **Netlify Functions** - Already deployed
- **curl** - For calling API from build scripts (available everywhere)
- **fish shell** - Already used in the project

---

## Questions to Resolve

1. How many historical builds should be retained? (TTL index or manual cleanup)
2. Should we add a build status field (success/failed/in-progress)?
3. Do we want to add build notes/changelogs?
4. Should we support building from Mac directly (local mode) or always from devcontainer?
5. Should failed builds also be registered (with a status field)?
