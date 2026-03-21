# Cloud VM Management Guide

Quick reference for managing your UE5 build VM across different cloud providers.

## Google Cloud Platform

### Create VM
```bash
./scripts/create-gcp-vm.sh YOUR_PROJECT_ID us-central1-a
```

### Stop VM (save costs)
```bash
gcloud compute instances stop ue5-builder-spiderlily --zone=us-central1-a
```

### Start VM
```bash
gcloud compute instances start ue5-builder-spiderlily --zone=us-central1-a
```

### Connect via RDP
```bash
# Get external IP
gcloud compute instances describe ue5-builder-spiderlily --zone=us-central1-a --format="get(networkInterfaces[0].accessConfigs[0].natIP)"

# Reset password if needed
gcloud compute reset-windows-password ue5-builder-spiderlily --zone=us-central1-a --user=builduser
```

### Delete VM
```bash
gcloud compute instances delete ue5-builder-spiderlily --zone=us-central1-a
```

### Check costs
```bash
# View billing
gcloud billing accounts list
gcloud billing projects describe YOUR_PROJECT_ID
```

## AWS EC2

### Create VM
```bash
# Via AWS Console or CLI
aws ec2 run-instances \
  --image-id ami-xxxxx \
  --instance-type c6i.4xlarge \
  --key-name your-key \
  --security-group-ids sg-xxxxx \
  --block-device-mappings DeviceName=/dev/sda1,Ebs={VolumeSize=500,VolumeType=gp3}
```

### Stop Instance
```bash
aws ec2 stop-instances --instance-ids i-xxxxx
```

### Start Instance
```bash
aws ec2 start-instances --instance-ids i-xxxxx
```

### Get Windows Password
```bash
aws ec2 get-password-data --instance-id i-xxxxx --priv-launch-key your-key.pem
```

### Terminate Instance
```bash
aws ec2 terminate-instances --instance-ids i-xxxxx
```

## Azure

### Create VM
```bash
az vm create \
  --resource-group ue5-builder-rg \
  --name ue5-builder \
  --image Win2022Datacenter \
  --size Standard_D16s_v5 \
  --admin-username builduser \
  --admin-password 'YourPassword123!'
```

### Stop VM (deallocate)
```bash
az vm deallocate --resource-group ue5-builder-rg --name ue5-builder
```

### Start VM
```bash
az vm start --resource-group ue5-builder-rg --name ue5-builder
```

### Get IP
```bash
az vm show -d --resource-group ue5-builder-rg --name ue5-builder --query publicIps -o tsv
```

### Delete VM
```bash
az vm delete --resource-group ue5-builder-rg --name ue5-builder --yes
```

## Cost-Saving Tips

### Run Only During Work Hours

**GCP Scheduled Stop/Start:**
```bash
# Stop at 6 PM (18:00)
gcloud compute instances add-metadata ue5-builder-spiderlily --zone=us-central1-a \
  --metadata=shutdown-script="shutdown /s /f /t 0"

# Use Cloud Scheduler to start/stop on schedule
```

**AWS Scheduled Actions:**
```bash
# Create schedules via AWS Console or EventBridge
aws autoscaling put-scheduled-action \
  --auto-scaling-group-name ue5-builder-asg \
  --scheduled-action-name stop-evening \
  --recurrence "0 18 * * *" \
  --min-size 0
```

### Use Preemptible/Spot Instances

**GCP Preemptible (70-90% discount):**
```bash
gcloud compute instances create ue5-builder-preemptible \
  --preemptible \
  --machine-type n2-standard-16 \
  ...
```

**AWS Spot Instances (50-90% discount):**
```bash
aws ec2 request-spot-instances \
  --instance-count 1 \
  --type persistent \
  --launch-specification file://spec.json
```

**Caveat:** Can be terminated at any time - not good for long builds!

### Resize When Not Building

Downsize to smaller instance during idle periods:

**GCP:**
```bash
gcloud compute instances set-machine-type ue5-builder-spiderlily \
  --zone=us-central1-a \
  --machine-type=n2-standard-4
```

**AWS:**
```bash
aws ec2 modify-instance-attribute \
  --instance-id i-xxxxx \
  --instance-type c6i.xlarge
```

## Monitoring Costs

### GCP
```bash
# View current month costs
gcloud billing accounts describe YOUR_BILLING_ACCOUNT

# Set budget alerts in Console
# https://console.cloud.google.com/billing/budgets
```

### AWS
```bash
# View costs
aws ce get-cost-and-usage \
  --time-period Start=2025-10-01,End=2025-10-31 \
  --granularity MONTHLY \
  --metrics BlendedCost
```

### Azure
```bash
# View costs
az consumption usage list --start-date 2025-10-01 --end-date 2025-10-31
```

## Automation Scripts

### Auto-start on build trigger (optional)

Create a GitHub Action that starts the VM before building:

```yaml
- name: Start GCP VM
  run: |
    gcloud compute instances start ue5-builder-spiderlily --zone=us-central1-a
    sleep 60  # Wait for boot
```

### Auto-stop after build

Add to workflow:
```yaml
- name: Stop VM
  if: always()
  run: |
    gcloud compute instances stop ue5-builder-spiderlily --zone=us-central1-a
```

## Backup and Snapshots

### GCP Disk Snapshot
```bash
gcloud compute disks snapshot ue5-builder-spiderlily \
  --snapshot-names=ue5-backup-$(date +%Y%m%d) \
  --zone=us-central1-a
```

### AWS EBS Snapshot
```bash
aws ec2 create-snapshot \
  --volume-id vol-xxxxx \
  --description "UE5 builder backup"
```

### Create Machine Image (for quick restore)

**GCP:**
```bash
gcloud compute images create ue5-builder-image \
  --source-disk=ue5-builder-spiderlily \
  --source-disk-zone=us-central1-a
```

## Troubleshooting

### VM won't start
- Check quotas (GCP: IAM & Admin > Quotas)
- Verify billing account is active
- Check instance logs in cloud console

### Can't connect via RDP
- Verify firewall rules allow port 3389
- Check external IP hasn't changed
- Reset Windows password

### High costs
- Check VM is stopping properly after builds
- Verify no orphaned disks/snapshots
- Use preemptible/spot instances for testing
- Consider smaller VM size

## Best Practices

1. **Tag resources** for easy tracking and cost allocation
2. **Set up billing alerts** to avoid surprises
3. **Use snapshots** before major changes
4. **Stop VM** when not actively building
5. **Monitor usage** weekly
6. **Test with smaller VM** first, then scale up if needed
