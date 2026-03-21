# Install mkcert CA root certificate into Windows Trust Store
# Run this in PowerShell as Administrator

# First, copy the cert from the container/volume to a temp location
$tempCert = "$env:TEMP\mkcert-rootCA.pem"

# Try multiple possible paths
$possiblePaths = @(
    "\\wsl.localhost\Ubuntu\workspaces\aesthetic-computer\ssl-dev\rootCA.pem",
    "\\wsl$\Ubuntu\workspaces\aesthetic-computer\ssl-dev\rootCA.pem"
)

$certPath = $null
foreach ($path in $possiblePaths) {
    if (Test-Path $path) {
        $certPath = $path
        break
    }
}

if ($certPath) {
    Write-Host "📜 Found certificate at: $certPath" -ForegroundColor Green
    Copy-Item $certPath $tempCert -Force
    Write-Host "📜 Installing mkcert CA root certificate into Windows Trust Store..." -ForegroundColor Cyan
    Import-Certificate -FilePath $tempCert -CertStoreLocation Cert:\LocalMachine\Root
    Remove-Item $tempCert
    Write-Host "✅ CA root certificate installed successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "⚠️  Please RESTART YOUR BROWSER (fully close and reopen) for changes to take effect." -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Your HTTPS URLs should now work:" -ForegroundColor Cyan
    Write-Host "  • https://localhost:8888"
    Write-Host "  • https://localhost:3002"
    Write-Host "  • https://localhost:8889"
} else {
    Write-Host "❌ Certificate file not found." -ForegroundColor Red
    Write-Host ""
    Write-Host "📋 Manual installation steps:" -ForegroundColor Yellow
    Write-Host "1. From the Docker container, copy the certificate:"
    Write-Host "   docker cp <container-name>:/workspaces/aesthetic-computer/ssl-dev/rootCA.pem C:\Users\$env:USERNAME\Downloads\rootCA.pem"
    Write-Host ""
    Write-Host "2. Then run this command:"
    Write-Host "   Import-Certificate -FilePath C:\Users\$env:USERNAME\Downloads\rootCA.pem -CertStoreLocation Cert:\LocalMachine\Root"
    Write-Host ""
    Write-Host "3. Restart your browser"
}
