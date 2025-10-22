# Unreal Engine 5 Builder for Perforce

Automated build system for Unreal Engine 5 projects hosted in Perforce on Azure.

## Overview

This GitHub Action workflow automates building UE5 projects by:
1. Provisioning a Windows build machine (self-hosted runner recommended)
2. Syncing the latest code from Perforce
3. Building the UE5 project
4. Uploading build artifacts

## Setup Options

### Option 1: Self-Hosted Runner (Recommended)
**Pros:** Faster builds, persistent UE5 installation, lower cost
**Cons:** Requires dedicated Windows machine

**Cloud Options:**
- **Google Cloud VM** - Create with `scripts/create-gcp-vm.sh` or `.ps1`
- **AWS EC2** - Any Windows instance type
- **Azure VM** - Windows Server image
- **Physical Machine** - Cheapest long-term option

### Option 2: On-Demand VM Provisioning
**Pros:** No dedicated hardware needed, scales on-demand
**Cons:** Slower (VM provisioning + UE5 install), higher cost per build
**Note:** This approach is more complex and usually not recommended

## Setup Instructions

### Prerequisites

1. **Windows Build Machine** (Physical or Cloud VM)
   - Windows 10/11 or Windows Server 2019/2022
   - Minimum: 32GB RAM, 8+ cores, 500GB SSD
   - Recommended: 64GB RAM, 16+ cores, 1TB NVMe SSD
   
   **Cloud VM Recommendations:**
   - **Google Cloud:** `n2-standard-16` or `n2-highmem-8`
   - **AWS:** `c6i.4xlarge` or `r6i.2xlarge`
   - **Azure:** `Standard_D16s_v5` or `Standard_E8s_v5`

2. **Software Installation**
   - Unreal Engine 5.x (install via Epic Games Launcher)
   - Visual Studio 2022 with:
     - Desktop development with C++
     - Game development with C++
     - .NET desktop development
   - Perforce Helix Core CLI (P4)
   - Git for Windows
   - PowerShell 7+

3. **Perforce Configuration**
   - P4PORT: Your Perforce server address (e.g., `ssl:your-server.azure.com:1666`)
   - P4USER: Build account username
   - P4PASSWD: Build account password
   - Workspace configured for the project

### Step 0: Create Cloud VM (Optional)

If using Google Cloud (you have keys in aesthetic-computer-vault):

```bash
# Linux/Mac
chmod +x scripts/create-gcp-vm.sh
./scripts/create-gcp-vm.sh YOUR_PROJECT_ID us-central1-a

# Or Windows PowerShell
.\scripts\create-gcp-vm.ps1 -ProjectId "YOUR_PROJECT_ID" -Zone "us-central1-a"
```

This creates a Windows Server 2022 VM ready for setup. Connect via RDP to continue.

For AWS or Azure, create a Windows instance manually with similar specs.

### Step 1: Configure GitHub Secrets

Add these secrets to your GitHub repository (Settings → Secrets and variables → Actions):

```
P4_SERVER     = ssl:your-perforce-server.azure.com:1666
P4_USER       = build_account
P4_PASSWORD   = your_secure_password
P4_WORKSPACE  = ue5_build_workspace
P4_CLIENT_PATH = //depot/YourProject/...
```

### Step 2: Setup Self-Hosted Runner

1. On your Windows build machine, download and configure the GitHub Actions runner:

```powershell
# Download the runner (check GitHub for latest version)
cd C:\actions-runner
curl -o actions-runner-win-x64-2.311.0.zip -L https://github.com/actions/runner/releases/download/v2.311.0/actions-runner-win-x64-2.311.0.zip
Expand-Archive -Path actions-runner-win-x64-2.311.0.zip -DestinationPath .

# Configure the runner
.\config.cmd --url https://github.com/YOUR_USERNAME/YOUR_REPO --token YOUR_TOKEN

# Add labels: windows, ue5, perforce
# When prompted for labels: windows,ue5,perforce

# Install as a service
.\svc.sh install
.\svc.sh start
```

2. Run the setup script to configure Perforce workspace:

```powershell
.\scripts\setup-build-machine.ps1
```

### Step 3: Configure Perforce Workspace

Use the provided script or manually:

```bash
p4 set P4PORT=ssl:your-server.azure.com:1666
p4 set P4USER=build_account
p4 set P4PASSWD=your_password
p4 client -o ue5_build_workspace > workspace.txt
# Edit workspace.txt to configure root and view mappings
p4 client -i < workspace.txt
```

### Step 4: Copy Workflow to Your Repo

Copy `.github/workflows/ue5-build-self-hosted.yml` to your GitHub repository.

### Step 5: Customize Build Configuration

Edit `config/build-config.json` to match your project:
- Project file path
- Build platforms (Win64, Linux, etc.)
- Build configurations (Development, Shipping, etc.)
- Cook settings

## Usage

### Manual Trigger
1. Go to Actions tab in GitHub
2. Select "UE5 Build (Self-Hosted)"
3. Click "Run workflow"
4. Choose branch and build options

### Automatic Builds
- Pushes to `main` branch trigger Development builds
- Tags matching `v*` trigger Shipping builds
- Pull requests trigger editor-only builds (validation)

## Troubleshooting

See [TROUBLESHOOTING.md](./TROUBLESHOOTING.md) for common issues and solutions.

## Cost Estimates

### Self-Hosted Runner Options

**Physical Machine:**
- Hardware: $2000-5000 one-time
- Electricity: ~$30/month
- **Total Year 1:** ~$2,360
- **Ongoing:** ~$30/month

**Google Cloud VM (n2-standard-16):**
- 24/7: ~$400-500/month
- 8hrs/day: ~$150-200/month
- Preemptible: ~$120-150/month

**AWS EC2 (c6i.4xlarge):**
- 24/7: ~$500-600/month
- 8hrs/day: ~$180-220/month
- Spot instances: ~$150-200/month

**Azure VM (Standard_D16s_v5):**
- 24/7: ~$500-800/month
- 8hrs/day: ~$200-300/month

**Plus:**
- Perforce licenses: $500-1000/year per seat
- Bandwidth: ~$50-100/month
- Storage: ~$50/month

### On-Demand VM Provisioning
- Build VM: ~$5-15 per build hour
- Storage: ~$50/month for persistent UE5 image
- Bandwidth: ~$50-100/month
- **Not recommended** - complex and expensive

## Security Notes

- Never commit Perforce credentials
- Use GitHub encrypted secrets
- Rotate passwords regularly
- Use SSL for Perforce connections
- Consider Azure Key Vault for production

## Support

For issues or questions, contact the build team or create a GitHub issue.
