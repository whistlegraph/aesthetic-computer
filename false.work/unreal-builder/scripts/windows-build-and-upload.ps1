# Remote Build + Upload Script for Windows (GCP VM)
# Runs on the Windows build machine, builds SpiderLily, and uploads to Spaces

param(
    [string]$Version = (Get-Date -Format "yyyy.MM.dd.HH.mm"),
    [string]$Config = "Development"
)

$ErrorActionPreference = "Stop"

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "SpiderLily Windows Build Pipeline" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Version: $Version"
Write-Host "Config: $Config"
Write-Host ""

# Paths
$UE5Path = "C:\Program Files\Epic Games\UE_5.6"
$ProjectRoot = "D:\Perforce\spiderlily_build_workspace\SL_main"
$ProjectFile = "$ProjectRoot\SpiderLily.uproject"
$OutputDir = "D:\Builds\$Version"

# Load Spaces credentials from vault
$VaultDir = "C:\aesthetic-computer-vault\false.work"
$SpacesEnv = "$VaultDir\builds-spaces.env"

if (!(Test-Path $SpacesEnv)) {
    Write-Error "Spaces config not found: $SpacesEnv"
    exit 1
}

# Parse .env file
$SpacesConfig = @{}
Get-Content $SpacesEnv | ForEach-Object {
    if ($_ -match '^([^#][^=]+)=(.*)$') {
        $SpacesConfig[$matches[1].Trim()] = $matches[2].Trim()
    }
}

# Step 1: Sync Perforce
Write-Host "📥 Step 1/4: Syncing Perforce..." -ForegroundColor Yellow
Set-Location $ProjectRoot
p4 sync -f
if ($LASTEXITCODE -ne 0) {
    Write-Error "Perforce sync failed"
    exit 1
}

# Step 2: Build
Write-Host ""
Write-Host "🔨 Step 2/4: Building Windows package..." -ForegroundColor Yellow
& "$UE5Path\Engine\Build\BatchFiles\RunUAT.bat" BuildCookRun `
    -project="$ProjectFile" `
    -platform=Win64 `
    -clientconfig=$Config `
    -cook `
    -build `
    -stage `
    -pak `
    -archive `
    -archivedirectory="$OutputDir" `
    -noP4 `
    -utf8output

if ($LASTEXITCODE -ne 0) {
    Write-Error "Build failed"
    exit 1
}

Write-Host ""
Write-Host "✅ Build complete!" -ForegroundColor Green
Write-Host "  Output: $OutputDir"
Write-Host ""

# Step 3: Compress
Write-Host "🗜️  Step 3/4: Compressing build..." -ForegroundColor Yellow
$ArchiveName = "spiderlily-windows-$Version.zip"
$ArchivePath = "D:\Builds\$ArchiveName"

# Use 7zip if available, otherwise PowerShell
if (Test-Path "C:\Program Files\7-Zip\7z.exe") {
    & "C:\Program Files\7-Zip\7z.exe" a -tzip "$ArchivePath" "$OutputDir\*"
} else {
    Compress-Archive -Path "$OutputDir\*" -DestinationPath "$ArchivePath" -CompressionLevel Optimal
}

$ArchiveSize = (Get-Item $ArchivePath).Length / 1MB
Write-Host "  ✓ Created: $ArchiveName ($([math]::Round($ArchiveSize, 2)) MB)"
Write-Host ""

# Step 4: Upload to Spaces using AWS CLI (S3-compatible)
Write-Host "☁️  Step 4/4: Uploading to DigitalOcean Spaces..." -ForegroundColor Yellow

$Bucket = $SpacesConfig['BUILDS_SPACES_BUCKET']
$Region = $SpacesConfig['BUILDS_SPACES_REGION']
$Endpoint = $SpacesConfig['BUILDS_SPACES_ENDPOINT']
$AccessKey = $SpacesConfig['BUILDS_SPACES_KEY']
$SecretKey = $SpacesConfig['BUILDS_SPACES_SECRET']

# Set AWS credentials for this session
$env:AWS_ACCESS_KEY_ID = $AccessKey
$env:AWS_SECRET_ACCESS_KEY = $SecretKey

# Upload using AWS CLI
$S3Uri = "s3://$Bucket/windows/$ArchiveName"
Write-Host "  Target: $S3Uri"

aws s3 cp $ArchivePath $S3Uri `
    --endpoint-url $Endpoint `
    --acl public-read `
    --region $Region

if ($LASTEXITCODE -ne 0) {
    Write-Error "Upload failed"
    exit 1
}

# Create latest symlink
$LatestUri = "s3://$Bucket/windows/spiderlily-windows-latest.zip"
aws s3 cp $S3Uri $LatestUri `
    --endpoint-url $Endpoint `
    --acl public-read `
    --region $Region

# Generate URLs
$CdnUrl = "https://$Bucket.$Region.cdn.digitaloceanspaces.com/windows/$ArchiveName"
$LatestCdnUrl = "https://$Bucket.$Region.cdn.digitaloceanspaces.com/windows/spiderlily-windows-latest.zip"

Write-Host ""
Write-Host "✅ Upload successful!" -ForegroundColor Green
Write-Host ""
Write-Host "📦 Download URLs:" -ForegroundColor Cyan
Write-Host "   CDN:    $CdnUrl"
Write-Host "   Latest: $LatestCdnUrl"
Write-Host ""
Write-Host "🌐 Add to builds.false.work:" -ForegroundColor Cyan
Write-Host "   <a href=`"$CdnUrl`">SpiderLily Windows $Version</a>"
Write-Host ""
Write-Host "🎉 Pipeline complete!" -ForegroundColor Green
