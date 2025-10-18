# false.work Studio

Automation tools and build scripts for false.work studio projects.

## Contents

- **unreal-builder/** - GitHub Actions workflow for building Unreal Engine 5 projects from Perforce

## Current Status (October 18, 2025)

### Spider Lily UE5 Build Automation - In Progress

**VM Setup:**
- ✅ GCP VM created (n2-standard-8: 8 vCPU, 32GB RAM, 300GB SSD)
- ✅ Bootstrap script completed
  - ✅ Chocolatey installed
  - ✅ Git for Windows installed
  - ✅ Perforce CLI (p4.exe) installed
  - ✅ Epic Games Launcher installed via Chocolatey
  - ✅ GitHub Actions runner downloaded
- ✅ aesthetic-computer-vault cloned
- ✅ Perforce SSL certificate trusted
- 🔄 **UE5.6 installing** (1-2 hours, in progress)

**Perforce Integration:**
- ✅ Can connect to Helix Core server (ssl:falsework.helixcore.io:1666)
- ⏳ **Waiting for "machine" user credentials to propagate** 
  - User just created in false.work's Helix Core Cloud
  - Need to verify password and access permissions
- ⏸️ Workspace creation pending (after login works)
- ⏸️ Project sync pending (30-60 min download after workspace created)

**Next Steps:**
1. Verify Perforce "machine" user login works
2. Create Perforce workspace pointing to //depot/SpiderLily/SL_main
3. Sync Spider Lily project (~50-100GB)
4. Configure GitHub Actions runner
5. Test first build
6. Document build workflow

**Timeline Estimate:**
- UE5 install: ~1-2 hours remaining
- Perforce sync: ~30-60 minutes (after credentials verified)
- First build test: ~30-60 minutes
- Total: ~3-5 hours to fully operational

## Setup

See individual subdirectories for specific setup instructions.
