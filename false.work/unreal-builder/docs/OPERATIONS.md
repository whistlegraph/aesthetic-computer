# Operational Commands - false.work UE5 Builder

Quick reference for day-to-day operations of the UE5 build VM.

## VM Management

### Check VM Status
```bash
gcloud compute instances list --project=aesthetic-computer --filter="name=ue5-builder-falsework"
```

### Start VM
```bash
gcloud compute instances start ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a

# Wait for it to be ready (30-60 seconds)
sleep 60
```

### Stop VM (Saves ~$8-10/day!)
```bash
gcloud compute instances stop ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a
```

### Get Current Status
```bash
gcloud compute instances describe ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a \
  --format="value(status,networkInterfaces[0].accessConfigs[0].natIP)"
```

### Get External IP
```bash
gcloud compute instances describe ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a \
  --format="get(networkInterfaces[0].accessConfigs[0].natIP)"
```

### Reset Windows Password
```bash
gcloud compute reset-windows-password ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a \
  --user=builduser \
  --quiet
```

### Delete VM (When Done)
```bash
gcloud compute instances delete ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a \
  --quiet
```

---

## GitHub Actions Runner Management

### Check Runner Status (In VM)
```powershell
# PowerShell on the VM
Get-Service actions.runner.*
```

### Restart Runner (In VM)
```powershell
Restart-Service actions.runner.*
```

### View Runner Logs (In VM)
```powershell
Get-Content C:\actions-runner\_diag\Runner_*.log -Tail 50
```

### Check Online Status
Go to: https://github.com/whistlegraph/aesthetic-computer/settings/actions/runners

Should see green dot next to the runner.

---

## Perforce Operations (In VM)

### Check Connection
```powershell
p4 info
p4 login -s
```

### Sync Latest
```powershell
p4 sync
```

### View Recent Changes
```powershell
p4 changes -m 5 //depot/SpiderLily/SL_main/...
```

### Check Workspace
```powershell
p4 client -o spiderlily_build_workspace
```

---

## Build Operations

### Trigger Build from GitHub
1. Go to: https://github.com/whistlegraph/aesthetic-computer/actions
2. Select "UE5 Build (Self-Hosted)"
3. Click "Run workflow"
4. Choose options and run

### Monitor Build (In VM)
```powershell
# Watch build logs
Get-Content D:\Builds\Logs\*.log -Tail 50 -Wait
```

### List Recent Builds
```powershell
Get-ChildItem D:\Builds | Sort-Object CreationTime -Descending | Select-Object -First 10
```

### Clean Old Builds (In VM)
```powershell
# Keep only 5 most recent builds
Get-ChildItem D:\Builds -Directory | 
  Sort-Object CreationTime -Descending | 
  Select-Object -Skip 5 | 
  Remove-Item -Recurse -Force
```

---

## Cost Management

### View Current Month Costs
```bash
gcloud billing accounts list

# Get detailed costs
gcloud billing projects describe aesthetic-computer \
  --format="value(billingAccountName)"
```

### Estimated Costs
- **Running:** ~$8-10/day ($240-300/month)
- **Stopped:** ~$2-3/day (just storage)
- **Best Practice:** Stop when not actively building

### Cost Optimization Schedule

**Example: Work hours only (8am-6pm weekdays)**
- Running: 50 hours/week = ~$100-125/month
- Savings: ~$150-175/month vs 24/7

```bash
# Morning: Start VM
gcloud compute instances start ue5-builder-falsework --zone=us-central1-a

# Evening: Stop VM
gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a
```

---

## Backup Operations

### Create Disk Snapshot
```bash
gcloud compute disks snapshot ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a \
  --snapshot-names="ue5-backup-$(date +%Y%m%d)"
```

### List Snapshots
```bash
gcloud compute snapshots list \
  --project=aesthetic-computer \
  --filter="name~ue5-backup"
```

### Create Machine Image (Full VM Backup)
```bash
gcloud compute machine-images create ue5-builder-image \
  --project=aesthetic-computer \
  --source-instance=ue5-builder-falsework \
  --source-instance-zone=us-central1-a
```

### Restore from Machine Image
```bash
gcloud compute instances create ue5-builder-restored \
  --project=aesthetic-computer \
  --zone=us-central1-a \
  --source-machine-image=ue5-builder-image
```

---

## Troubleshooting Commands

### VM Won't Start
```bash
# Check quotas
gcloud compute project-info describe --project=aesthetic-computer

# Check for any errors
gcloud compute operations list --project=aesthetic-computer --limit=10
```

### Can't Connect via RDP
```bash
# Verify VM is running
gcloud compute instances describe ue5-builder-falsework --zone=us-central1-a --format="value(status)"

# Check firewall rules
gcloud compute firewall-rules list --project=aesthetic-computer --filter="name=allow-rdp-ue5-builder"

# Reset Windows password
gcloud compute reset-windows-password ue5-builder-falsework --zone=us-central1-a --user=builduser
```

### Runner Shows Offline
```bash
# SSH into VM to investigate
gcloud compute ssh ue5-builder-falsework --zone=us-central1-a

# Then in VM:
Get-Service actions.runner.*
Get-Content C:\actions-runner\_diag\Runner_*.log -Tail 100
```

---

## Monitoring

### Check VM CPU/Memory Usage
```bash
gcloud compute instances describe ue5-builder-falsework \
  --project=aesthetic-computer \
  --zone=us-central1-a \
  --format="json" | jq '.cpuPlatform,.machineType'
```

### View Metrics in Console
https://console.cloud.google.com/compute/instancesDetail/zones/us-central1-a/instances/ue5-builder-falsework?project=aesthetic-computer&tab=monitoring

---

## Daily Workflow Example

### Morning (Start of Work Day)
```bash
# 1. Start VM
gcloud compute instances start ue5-builder-falsework --zone=us-central1-a

# 2. Wait for boot
sleep 60

# 3. Get IP for RDP
gcloud compute instances describe ue5-builder-falsework \
  --zone=us-central1-a \
  --format="get(networkInterfaces[0].accessConfigs[0].natIP)"

# 4. Connect via RDP (use IP from step 3)
```

### During Day
- Trigger builds from GitHub Actions
- Monitor via GitHub Actions UI
- Download artifacts when ready

### Evening (End of Work Day)
```bash
# Stop VM to save money
gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a
```

### Weekly
```bash
# Create backup snapshot
gcloud compute disks snapshot ue5-builder-falsework \
  --zone=us-central1-a \
  --snapshot-names="ue5-backup-$(date +%Y%m%d)"

# Check costs
# Visit: https://console.cloud.google.com/billing?project=aesthetic-computer
```

---

## Emergency Procedures

### Build Is Stuck
1. Check GitHub Actions UI for logs
2. SSH into VM: `gcloud compute ssh ue5-builder-falsework --zone=us-central1-a`
3. Check Task Manager for hung processes
4. If needed, restart runner: `Restart-Service actions.runner.*`

### VM Is Unresponsive
```bash
# Force stop
gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a

# Wait
sleep 30

# Start again
gcloud compute instances start ue5-builder-falsework --zone=us-central1-a
```

### Need to Rebuild Everything
```bash
# Delete VM
gcloud compute instances delete ue5-builder-falsework --zone=us-central1-a --quiet

# Re-run setup script
cd /workspaces/aesthetic-computer/false.work/unreal-builder/scripts
./quick-setup-gcp.sh
```

---

## Useful Aliases (Add to .bashrc or .config/fish/config.fish)

### Bash
```bash
alias ue5-start='gcloud compute instances start ue5-builder-falsework --zone=us-central1-a'
alias ue5-stop='gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a'
alias ue5-status='gcloud compute instances describe ue5-builder-falsework --zone=us-central1-a --format="value(status)"'
alias ue5-ip='gcloud compute instances describe ue5-builder-falsework --zone=us-central1-a --format="get(networkInterfaces[0].accessConfigs[0].natIP)"'
alias ue5-rdp='remmina -c rdp://builduser@$(ue5-ip)'
```

### Fish
```fish
alias ue5-start='gcloud compute instances start ue5-builder-falsework --zone=us-central1-a'
alias ue5-stop='gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a'
alias ue5-status='gcloud compute instances describe ue5-builder-falsework --zone=us-central1-a --format="value(status)"'
alias ue5-ip='gcloud compute instances describe ue5-builder-falsework --zone=us-central1-a --format="get(networkInterfaces[0].accessConfigs[0].natIP)"'
alias ue5-rdp='remmina -c rdp://builduser@(ue5-ip)'
```

---

## Quick Reference Card

```
┌─────────────────────────────────────────────────┐
│ false.work UE5 Builder - Quick Commands        │
├─────────────────────────────────────────────────┤
│ Start:  ue5-start                               │
│ Stop:   ue5-stop                                │
│ Status: ue5-status                              │
│ IP:     ue5-ip                                  │
│ RDP:    ue5-rdp                                 │
├─────────────────────────────────────────────────┤
│ Builds: github.com/whistlegraph/                │
│         aesthetic-computer/actions              │
├─────────────────────────────────────────────────┤
│ Cost: ~$8/day running, ~$2/day stopped          │
│ Remember: STOP VM when not building!            │
└─────────────────────────────────────────────────┘
```
