# Painting Tools & CLI Suite

Command-line tools for managing, inspecting, and orchestrating the painting short codes feature.

## Tools Overview

### Inspection Tools
- **`inspect-spaces.mjs`** - Browse Digital Ocean Spaces buckets
- **`inspect-mongodb.mjs`** - Query paintings collection
- **`inspect-api.mjs`** - Test API endpoints (local or live)
- **`check-lifecycle.mjs`** - Check bucket lifecycle/expiration policies

### Management Tools
- **`generate-codes.mjs`** - Generate short codes for paintings
- **`migrate-codes.mjs`** - Add codes to existing paintings
- **`audit-orphans.mjs`** - Find paintings in S3 but not in MongoDB

### Analysis Tools
- **`stats.mjs`** - Database statistics and reports
- **`duplicates.mjs`** - Find duplicate paintings by hash
- **`verify.mjs`** - Verify data integrity

## Setup

```bash
cd paintings
npm install
```

## Environment

Tools can target local or live environments:

```bash
# Local development server
export AC_ENV=local
export AC_API=http://localhost:8888

# Live production
export AC_ENV=live
export AC_API=https://aesthetic.computer
```

## Quick Start

```bash
# Inspect Digital Ocean Spaces
node inspect-spaces.mjs --list

# Check bucket lifecycle policies (expiration rules)
node check-lifecycle.mjs

# Check MongoDB paintings collection
node inspect-mongodb.mjs --count
node inspect-mongodb.mjs --recent 10

# Test API endpoints
node inspect-api.mjs --endpoint /api/tv

# Generate codes for new paintings
node generate-codes.mjs --user auth0|123 --dry-run

# Audit for orphaned files
node audit-orphans.mjs --report
```

## Configuration

Create `.env` file:

```env
# Digital Ocean Spaces
DO_SPACES_KEY=your-key
DO_SPACES_SECRET=your-secret
DO_SPACES_ENDPOINT=nyc3.digitaloceanspaces.com
DO_SPACES_BUCKET=aesthetic-computer

# MongoDB (local or live)
MONGODB_URI=mongodb://localhost:27017/aesthetic
# Or production:
# MONGODB_URI=mongodb+srv://...

# API
AC_API=http://localhost:8888
# Or production:
# AC_API=https://aesthetic.computer
```

## Development Workflow

1. **Inspect current state:**
   ```bash
   node inspect-mongodb.mjs --stats
   node inspect-spaces.mjs --count
   ```

2. **Test code generation:**
   ```bash
   node generate-codes.mjs --sample 10 --dry-run
   ```

3. **Run migration:**
   ```bash
   node migrate-codes.mjs --dry-run
   node migrate-codes.mjs --execute
   ```

4. **Verify results:**
   ```bash
   node verify.mjs --all
   node stats.mjs
   ```

## Architecture

All tools use shared modules from `/system/backend` where possible:
- `database.mjs` - MongoDB connection
- `painting-code-generator.mjs` - Code generation logic
- S3 client configuration

## See Also

- [Painting Short Codes Plan](/plans/painting-short-codes-plan.md)
- [System Backend](/system/backend/)
- [API Functions](/system/netlify/functions/)
