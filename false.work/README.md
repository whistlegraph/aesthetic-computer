<img width="400" src="https://images.squarespace-cdn.com/content/v1/6838f89fea63a32f67c61d96/5bc717b9-7339-42d1-8544-66c19d6d925b/Asset+18.png?format=1500w">

### 💌 Howdy

I managed to set up a remote Windows machine to build Spider Lily! Started with a bootstrap script that kept hitting edge cases (C: vs D: drives, reentrant progress, .NET SDK, Windows Defender exclusions). Added Epic Games Launcher via Chocolatey, wrangled MSVC 14.38 for VS2022, disabled LiveCoding for automation, and finally got the whole pipeline working. Cost about $28 so far, should be ~$2-3 per rebuild.

Next up: auto-deploy to your site under a password page!

— @jeffrey on [prompt.ac/chat](https://prompt.ac/chat)

### 🔜 Next
1. Set up automated build system
2. Auto-post builds to false.work website under password-protected page
3. **🆕 Mac Builder Setup** - Configure Mac Mini for Mac builds

### ✅ Status (October 28, 2025)
- ✅ **Windows Builder** - GCP VM us-central1 (n2-standard-8: 8 vCPU, 32GB RAM, 300GB SSD)
  - ✅ UE5.6 + Perforce + GitHub Actions runner
  - ✅ Connected to Helix Core (ssl:falsework.helixcore.io:1666)
  - ✅ **Spider Lily built and deployed for Windows**
- ✅ **Mac Builder** - M4 Mac Mini with UE5.6 + Perforce
  - ✅ SSH access configured from dev container
  - ✅ FMOD Mac binaries installed
  - ✅ .NET 8 SDK installed
  - ✅ Xcode 26 compatibility patched
  - ✅ **Mac builds working!** (~1GB .app bundle)
  - ⚠️ iOS builds blocked by code signing (Team ID configured, needs Xcode GUI setup)
  - 🔜 P4 CLI setup needed
  - 🔜 Automated deployment to false.work

### 💰 Costs
**Setup so far:** ~$28
- Windows Server license: $14.46 (314.26 hours)
- N2 Instance Core: $8.81 (278.83 hours)
- N2 Instance RAM: $4.73 (1,115.31 GiB hours)

**Per rebuild:** ~$2-3 (estimated spin-up + build time)

Engineered by [Aesthetic Inc.](https://aesthetic.direct)
