# Setup OpenSSH Server on Windows
# Run this in PowerShell as Administrator

Write-Host "🔧 Setting up OpenSSH Server on Windows..." -ForegroundColor Cyan

# Install OpenSSH Server
Write-Host "`n📦 Installing OpenSSH Server..."
Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0

# Start and enable SSH service
Write-Host "`n🚀 Starting SSH service..."
Start-Service sshd
Set-Service -Name sshd -StartupType 'Automatic'

# Configure firewall
Write-Host "`n🔥 Configuring firewall..."
if (!(Get-NetFirewallRule -Name "OpenSSH-Server-In-TCP" -ErrorAction SilentlyContinue)) {
    New-NetFirewallRule -Name 'OpenSSH-Server-In-TCP' -DisplayName 'OpenSSH Server (sshd)' `
        -Enabled True -Direction Inbound -Protocol TCP -Action Allow -LocalPort 22
}

# Set PowerShell as default shell (optional but recommended)
Write-Host "`n🐚 Setting PowerShell as default SSH shell..."
New-ItemProperty -Path "HKLM:\SOFTWARE\OpenSSH" -Name DefaultShell `
    -Value "C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe" `
    -PropertyType String -Force

# Create .ssh directory for authorized_keys
$sshDir = "$env:PROGRAMDATA\ssh"
if (!(Test-Path $sshDir)) {
    New-Item -ItemType Directory -Path $sshDir
}

Write-Host "`n✅ OpenSSH Server setup complete!" -ForegroundColor Green
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "1. Copy your public key from the devcontainer:"
Write-Host "   cat ~/.ssh/id_ed25519.pub  # or id_rsa.pub"
Write-Host ""
Write-Host "2. Add it to: C:\ProgramData\ssh\administrators_authorized_keys"
Write-Host "   (Create the file if it doesn't exist)"
Write-Host ""
Write-Host "3. Set proper permissions on administrators_authorized_keys:"
Write-Host '   icacls "C:\ProgramData\ssh\administrators_authorized_keys" /inheritance:r'
Write-Host '   icacls "C:\ProgramData\ssh\administrators_authorized_keys" /grant SYSTEM:(F)'
Write-Host '   icacls "C:\ProgramData\ssh\administrators_authorized_keys" /grant BUILTIN\Administrators:(F)'
Write-Host ""
Write-Host "Your Windows IP (from container): 172.17.0.1"
