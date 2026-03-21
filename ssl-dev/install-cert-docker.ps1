# Install mkcert CA root certificate from Docker container
# Run this in PowerShell as Administrator

Write-Host "🔍 Finding Docker container..." -ForegroundColor Cyan

# Get the container ID/name for the aesthetic-computer workspace
$container = docker ps --filter "label=devcontainer.local_folder" --format "{{.Names}}" | Select-Object -First 1

if (-not $container) {
    # Try finding by image or volume
    $container = docker ps --format "{{.Names}}" | Where-Object { $_ -match "aesthetic" } | Select-Object -First 1
}

if (-not $container) {
    Write-Host "❌ Could not find Docker container." -ForegroundColor Red
    Write-Host ""
    Write-Host "📋 Manual steps:" -ForegroundColor Yellow
    Write-Host "1. Find your container name: docker ps"
    Write-Host "2. Copy the certificate:"
    Write-Host '   docker cp <container-name>:/workspaces/aesthetic-computer/ssl-dev/rootCA.pem $env:TEMP\rootCA.pem'
    Write-Host "3. Import it:"
    Write-Host '   Import-Certificate -FilePath $env:TEMP\rootCA.pem -CertStoreLocation Cert:\LocalMachine\Root'
    Write-Host "4. Restart your browser"
    exit 1
}

Write-Host "✅ Found container: $container" -ForegroundColor Green

# Copy the certificate from the container
$tempCert = "$env:TEMP\mkcert-rootCA.pem"
Write-Host "📋 Copying certificate from container..." -ForegroundColor Cyan

docker cp "${container}:/workspaces/aesthetic-computer/ssl-dev/rootCA.pem" $tempCert

if (Test-Path $tempCert) {
    Write-Host "📜 Installing mkcert CA root certificate into Windows Trust Store..." -ForegroundColor Cyan
    Import-Certificate -FilePath $tempCert -CertStoreLocation Cert:\LocalMachine\Root
    Remove-Item $tempCert
    Write-Host ""
    Write-Host "✅ CA root certificate installed successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "⚠️  IMPORTANT: Fully close and restart Chrome for changes to take effect." -ForegroundColor Yellow
    Write-Host "   (Don't just refresh - close ALL Chrome windows and reopen)" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Your HTTPS URLs should now work:" -ForegroundColor Cyan
    Write-Host "  • https://localhost:8888"
    Write-Host "  • https://localhost:3002"
    Write-Host "  • https://localhost:8889"
} else {
    Write-Host "❌ Failed to copy certificate from container." -ForegroundColor Red
    Write-Host "Container path: /workspaces/aesthetic-computer/ssl-dev/rootCA.pem"
}
