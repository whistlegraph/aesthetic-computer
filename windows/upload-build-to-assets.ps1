# Upload SpiderLily build to aesthetic-computer assets directory
param(
    [string]$BuildPath = "C:\Users\me\Desktop\SpiderLily-2025.11.07-0207\Windows",
    [string]$Version = "2025.11.07-0207"
)

$ZipName = "spiderlily-windows-$Version.zip"
$ZipPath = "$env:TEMP\$ZipName"
$WSLPath = "\\wsl.localhost\Ubuntu\workspaces\aesthetic-computer\system\public\assets\false.work\$ZipName"

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Upload SpiderLily Build to Assets" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

# Compress build
Write-Host "🗜️  Compressing build..." -ForegroundColor Yellow
Write-Host "   Source: $BuildPath"
Compress-Archive -Path "$BuildPath\*" -DestinationPath $ZipPath -CompressionLevel Optimal -Force

$size = (Get-Item $ZipPath).Length / 1MB
$sizeRounded = [math]::Round($size, 2)
Write-Host "   ✓ Created: $ZipName ($sizeRounded MB)" -ForegroundColor Green
Write-Host ""

# Copy to WSL
Write-Host "📁 Copying to assets directory..." -ForegroundColor Yellow
Write-Host "   Target: $WSLPath"

try {
    Copy-Item -Path $ZipPath -Destination $WSLPath -Force
    Write-Host "   ✓ Copied successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "✅ Upload complete!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Next steps:" -ForegroundColor Cyan
    Write-Host "1. Run in WSL container: npm run assets:sync:up"
    Write-Host "2. Build will be at: https://assets.aesthetic.computer/false.work/$ZipName"
    Write-Host "3. Update builds.false.work/index.html with download link"
} catch {
    Write-Host "   ❌ Failed to copy to WSL" -ForegroundColor Red
    Write-Host ""
    Write-Host "Alternative: Manually copy from:" -ForegroundColor Yellow
    Write-Host "   $ZipPath"
    Write-Host "To:" -ForegroundColor Yellow
    Write-Host "   \\wsl`$\Ubuntu\workspaces\aesthetic-computer\system\public\assets\false.work\"
}

Write-Host ""
