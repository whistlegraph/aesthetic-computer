# Scripts Architecture

## ✅ Final Organization (November 2024)

After investigating script placement and dependency resolution, we arrived at this clean architecture:

### Location: `scripts/` (workspace root)

**Why root level?**
- Logical separation: deployment code in `system/`, operational tools in `scripts/`
- Clear organization: not buried in implementation directories
- Consistent with existing `at/scripts/` pattern

### Dependency Strategy: Shared Dependencies

**Problem**: Scripts need packages like `@atproto/api`, `mongodb`, etc. that are installed in `system/node_modules/`

**Solution**: Install same packages at root level
- Added to root `package.json` dependencies
- Small duplication (fine architecturally)
- Node.js walks up from `scripts/atproto/` → `scripts/` → root → finds `node_modules/`
- Same packages in `system/` for Netlify functions (they need bundled deps)

**Dependencies shared**:
```json
{
  "@atproto/api": "^0.17.2",
  "@aws-sdk/client-s3": "^3.896.0", 
  "mongodb": "^6.20.0",
  "dotenv": "^16.4.7",
  "node-fetch": "^3.3.2"
}
```

### Import Paths

Scripts use relative paths to access backend modules:

```javascript
// From scripts/atproto/*.mjs
import { connect } from "../../system/backend/database.mjs";
import { createMediaRecord } from "../../system/backend/media-atproto.mjs";

// From scripts/recovery/*.mjs  
import { connect } from "../../system/backend/database.mjs";
import { userIDFromHandle } from "../../system/backend/authorization.mjs";
```

**Path breakdown**: `../../system/backend/`
- `../` - up from `scripts/atproto/` to `scripts/`
- `../` - up from `scripts/` to workspace root
- `system/backend/` - into backend module directory

### Environment Variables

Scripts load `.env` from `system/.env`:

```javascript
import { config } from 'dotenv';
config({ path: './system/.env' });  // Relative to workspace root
```

**Must run from workspace root** so `./system/.env` resolves correctly.

## 🏗️ Directory Structure

```
/workspaces/aesthetic-computer/
├── scripts/                    # Operational scripts (NEW!)
│   ├── README.md              # Omakase menu for LLM
│   ├── SETUP.md               # Setup instructions
│   ├── ARCHITECTURE.md        # This file
│   ├── atproto/               # ATProto sync/backfill tools
│   │   ├── sync-atproto.mjs
│   │   ├── backfill-kidlisp-rkeys.mjs
│   │   └── ...
│   └── recovery/              # Data recovery tools
│       ├── recover-deleted-tapes.mjs
│       └── check-tape-zips-in-spaces.mjs
│
├── system/                     # Deployment code
│   ├── backend/               # Reusable modules
│   │   ├── database.mjs
│   │   ├── media-atproto.mjs
│   │   ├── authorization.mjs
│   │   └── ...
│   ├── netlify/
│   │   └── functions/         # Netlify edge functions
│   ├── .env                   # Secrets (in .gitignore)
│   ├── node_modules/          # System dependencies
│   └── package.json
│
├── at/scripts/atproto/        # External AT Protocol tools
│   └── ...                    # Federation, lexicon publishing, etc.
│
├── node_modules/              # Root dependencies (includes script deps)
└── package.json               # Root packages (includes script deps)
```

## 🎯 Design Principles

1. **Separation of Concerns**
   - `system/` = deployment infrastructure (Netlify functions, edge compute)
   - `scripts/` = operational tools (maintenance, sync, recovery)
   - `at/scripts/` = external protocol tools (federation with other PDS)

2. **Shared Dependencies**
   - Accept small duplication for cleaner architecture
   - Root level for scripts, system level for functions
   - Both can use same versions (kept in sync)

3. **Backend Modules as Library**
   - `system/backend/*.mjs` = reusable utilities
   - Used by: Netlify functions, operational scripts, AT tools
   - Never create duplicate implementations

4. **LLM-Friendly Documentation**
   - `README.md` = "Omakase menu" of available scripts
   - `SETUP.md` = Getting started, troubleshooting
   - `ARCHITECTURE.md` = Design decisions (this file)

## 🚀 Usage Pattern

```bash
# 1. Ensure dependencies installed
npm install  # (at root)

# 2. Ensure secrets available
ls system/.env  # or copy from vault

# 3. Run scripts from root
node scripts/atproto/sync-atproto.mjs live --kidlisp-only
node scripts/recovery/check-tape-zips-in-spaces.mjs @jeffrey
```

## 🔄 Alternatives Considered

### ❌ Keep in `system/backend/`
- **Pros**: Natural access to system/node_modules
- **Cons**: Mixed with library code, unclear which are operations vs utilities

### ❌ Move to `system/scripts/`  
- **Pros**: Still in system where deps are
- **Cons**: Scripts aren't really "system" deployment code

### ❌ Use NODE_PATH hack
- **Pros**: Scripts could stay anywhere
- **Cons**: Hacky, requires wrapper scripts, confusing

### ✅ Root `scripts/` + shared deps (CHOSEN)
- **Pros**: Clean separation, logical organization, minimal duplication
- **Cons**: Some deps duplicated (acceptable tradeoff)

## 📝 Commit Strategy

Group related changes:
1. Script organization (moves, README, SETUP)
2. Dependency sharing (package.json updates)
3. Netlify config fixes (@atproto/api externals)

Message:
```
Organize operational scripts with shared dependencies

- Move provisional scripts from system/backend/ to scripts/
- Add atproto/ (sync/backfill) and recovery/ subdirectories  
- Share dependencies: install @atproto/api, mongodb, etc. at root
- Fix netlify.toml: add @atproto/api to 4 function externals
- Add documentation: README (omakase menu), SETUP, ARCHITECTURE

All kidlisp/tapes/moods synced to ATProto successfully
```
