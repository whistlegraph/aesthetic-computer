# Hardware Specifications & UE5 Compatibility

## 🖥️ Current VM Configuration

**Machine Type:** `n2-standard-8`
- **CPU:** 8 vCPUs (Intel Cascade Lake)
- **RAM:** 32 GB
- **Disk:** 300GB SSD (pd-ssd)
- **GPU:** ❌ None (software rendering only)
- **OS:** Windows Server 2022

**Cost:** ~$200-250/month (24/7) or ~$80-100/month (8hrs/day weekdays)

---

## ✅ UE5 Build Requirements (Met)

For **automated builds** (no editor, no viewport), you need:

### Minimum:
- ✅ CPU: Quad-core Intel/AMD, 2.5+ GHz → **We have: 8-core, 2.8+ GHz**
- ✅ RAM: 16 GB → **We have: 32 GB**
- ✅ Storage: 100GB+ SSD → **We have: 300GB SSD**
- ⚠️ GPU: Not required for command-line builds

### Our Status:
- ✅ **CPU:** n2-standard-8 is MORE than enough (8 cores > 4 required)
- ✅ **RAM:** 32GB is DOUBLE the minimum (16GB)
- ✅ **Disk:** Fast SSD, plenty of space
- ⚠️ **GPU:** None, but NOT NEEDED for automated builds

---

## 🎮 GPU Considerations

### Do We Need a GPU?

**For automated builds: NO** ✅
- `RunUAT.bat` (build automation) doesn't need GPU
- UnrealBuildTool compiles C++ without GPU
- Cooking assets works fine on CPU
- Packaging uses CPU only

**For interactive editing: YES** ❌
- UE5 Editor needs GPU for viewport rendering
- But we're NOT running the editor!

### Current Setup:
- **No GPU attached** (saves ~$200-400/month)
- Builds will run using software rendering
- Perfectly fine for CI/CD pipelines

### If You Ever Need GPU:

Change machine type to add GPU:
```bash
# Stop VM
gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a

# Add NVIDIA T4 GPU (~$100-150/month extra)
gcloud compute instances attach-disk ue5-builder-falsework \
  --disk-type=pd-ssd \
  --zone=us-central1-a

gcloud compute instances set-machine-type ue5-builder-falsework \
  --machine-type=n1-standard-8 \
  --zone=us-central1-a

gcloud compute instances attach-accelerator ue5-builder-falsework \
  --accelerator-type=nvidia-tesla-t4 \
  --accelerator-count=1 \
  --zone=us-central1-a

# Start VM
gcloud compute instances start ue5-builder-falsework --zone=us-central1-a
```

**GPU Options & Costs:**
- NVIDIA T4: ~$100-150/month (good for most work)
- NVIDIA V100: ~$700-800/month (overkill for builds)
- NVIDIA A100: ~$2000+/month (extreme overkill)

---

## 📊 Performance Expectations

### Build Times (CPU-only):

**Current Setup (8 cores, no GPU):**
- Full clean build: 30-60 minutes
- Incremental build: 5-15 minutes
- Cooking: 10-20 minutes
- Packaging: 5-10 minutes

**If You Add GPU:**
- Same times! (GPU doesn't help build/cook)
- Only helps if you run editor interactively

**With Horde Distributed Builds:**
- Full clean build: 5-15 minutes (if you add more machines)
- Requires 2+ build machines

---

## 🔍 Verification Steps

Once you're in the VM, check the hardware:

```powershell
# Check CPU
Get-WmiObject Win32_Processor | Select-Object Name, NumberOfCores, NumberOfLogicalProcessors

# Check RAM
Get-WmiObject Win32_ComputerSystem | Select-Object @{Name="RAM (GB)";Expression={[math]::Round($_.TotalPhysicalMemory/1GB,2)}}

# Check disk
Get-PSDrive C | Select-Object Used, Free, @{Name="Size (GB)";Expression={[math]::Round($_.Used/1GB + $_.Free/1GB,2)}}

# Check for GPU
Get-WmiObject Win32_VideoController | Select-Object Name, AdapterRAM

# System info
systeminfo | findstr /B /C:"OS Name" /C:"System Type" /C:"Processor"
```

**Expected Output:**
- CPU: Intel Xeon (Cascade Lake), 8 cores
- RAM: 32 GB
- GPU: "Microsoft Basic Display Adapter" (no hardware GPU)
- This is FINE for builds! ✅

---

## 🎯 Recommendation

### Keep Current Configuration ✅

**Reasons:**
1. ✅ CPU is more than powerful enough (8 cores)
2. ✅ RAM is double the requirement (32GB)
3. ✅ Builds don't need GPU
4. ✅ Saves $200-400/month vs GPU instance
5. ✅ Fast SSD for disk I/O

### When to Upgrade:

**Add GPU only if:**
- ❌ You want to RDP in and use UE5 Editor interactively
- ❌ You need to test rendering/graphics (unlikely for CI/CD)
- ❌ You want to run game tests with actual rendering

**For just builds:** Current setup is perfect! 🎯

---

## 🚨 Real-World Check

After bootstrap completes and UE5 installs, run a test build and see:
- If it completes successfully ✅
- How long it takes ⏱️
- If you get memory errors (unlikely with 32GB) ❌

If builds fail due to hardware, we can upgrade. But **99% certain they'll work fine!**

---

## 💰 Cost Comparison

| Configuration | Monthly Cost (24/7) | Monthly Cost (8hrs/weekday) |
|---------------|---------------------|------------------------------|
| **Current (CPU only)** | **$200-250** | **$80-100** |
| With T4 GPU | $350-400 | $160-200 |
| With V100 GPU | $900-1000 | $450-500 |

**Verdict:** Keep current setup until you have a reason to change! 💸

---

## 📝 Summary

**Your VM is perfectly capable of UE5 builds:**
- ✅ 8 CPU cores (UE5 builds are CPU-intensive)
- ✅ 32GB RAM (plenty for parallel compilation)
- ✅ 300GB SSD (fast I/O for large projects)
- ⚠️ No GPU (not needed for command-line builds)

**Test it first, upgrade only if needed!** 🚀

The lack of GPU will NOT prevent builds from working. It only matters for interactive editor use or running the actual game with rendering.
