$ErrorActionPreference = "Stop"

$identity = [Security.Principal.WindowsIdentity]::GetCurrent()
$principal = [Security.Principal.WindowsPrincipal]::new($identity)
if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
  throw "Run Windows Terminal as Administrator, then run this command again."
}

$tailscale = Join-Path $env:ProgramFiles "Tailscale\tailscale.exe"
if (-not (Test-Path $tailscale)) {
  throw "Tailscale is not installed in its standard location."
}

$windows = Get-ItemProperty "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion"
Write-Host "Windows: $($windows.ProductName) ($($windows.EditionID))"
if ($windows.EditionID -match "^(Core|CoreSingleLanguage|CoreCountrySpecific|Home)") {
  throw "This Windows Home edition cannot host Microsoft Remote Desktop."
}

# Tailscale's device-local Shields Up preference drops inbound connections even
# when the tailnet access policy and Windows Firewall allow them.
& $tailscale set --shields-up=false
if ($LASTEXITCODE -ne 0) {
  throw "Tailscale could not disable Shields Up."
}

$terminalServer = "HKLM:\SYSTEM\CurrentControlSet\Control\Terminal Server"
$rdpTcp = Join-Path $terminalServer "WinStations\RDP-Tcp"
Set-ItemProperty $terminalServer -Name fDenyTSConnections -Value 0
Set-ItemProperty $rdpTcp -Name fEnableWinStation -Value 1
$rdpPort = (Get-ItemProperty $rdpTcp -Name PortNumber).PortNumber
Set-Service -Name TermService -StartupType Automatic
Start-Service -Name TermService
$terminalSettings = Get-CimInstance `
  -Namespace "root\cimv2\TerminalServices" `
  -ClassName "Win32_TerminalServiceSetting"
$allowResult = Invoke-CimMethod `
  -InputObject $terminalSettings `
  -MethodName "SetAllowTSConnections" `
  -Arguments @{ AllowTSConnections = 1; ModifyFirewallException = 0 }
Start-Sleep -Seconds 2

# Keep RDP limited to peers arriving from Tailscale's IPv4 address range. The
# tailnet access policy remains an additional authorization layer.
Get-NetFirewallRule -Name "RemoteDesktop*" -ErrorAction SilentlyContinue |
  Disable-NetFirewallRule
$ruleName = "Remote Desktop over Tailscale"
$rule = Get-NetFirewallRule -DisplayName $ruleName -ErrorAction SilentlyContinue
if ($null -eq $rule) {
  $rule = New-NetFirewallRule `
    -DisplayName $ruleName `
    -Direction Inbound `
    -Action Allow `
    -Protocol TCP `
    -LocalPort $rdpPort `
    -RemoteAddress "100.64.0.0/10" `
    -Profile Any
} else {
  $rule | Set-NetFirewallRule -Enabled True -Direction Inbound -Action Allow -Profile Any
  $rule | Get-NetFirewallPortFilter |
    Set-NetFirewallPortFilter -Protocol TCP -LocalPort $rdpPort
  $rule | Get-NetFirewallAddressFilter |
    Set-NetFirewallAddressFilter -RemoteAddress "100.64.0.0/10"
}

# Configure an already-installed Windows OpenSSH endpoint for maintenance.
# Only Blueberry's tailnet IP can reach it, and only Blueberry's dedicated
# public key is authorized; no password is copied into this script.
$sshService = Get-Service -Name sshd -ErrorAction SilentlyContinue
$sshState = if ($null -ne $sshService) { "Installed" } else { "NotPresent" }
if ($sshState -eq "Installed") {
  $authorizedKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKBhGVdx9XGursPqyJPf1f7ijcKL7B579F7/LpcPdveb blueberry-to-skips-surface"
  $authorizedKeysPath = Join-Path $env:ProgramData "ssh\administrators_authorized_keys"
  $authorizedKeysDirectory = Split-Path $authorizedKeysPath
  New-Item -ItemType Directory -Force -Path $authorizedKeysDirectory | Out-Null
  $authorizedKeys = @(Get-Content $authorizedKeysPath -ErrorAction SilentlyContinue)
  if ($authorizedKeys -notcontains $authorizedKey) {
    @($authorizedKeys + $authorizedKey) |
      Set-Content -Path $authorizedKeysPath -Encoding ascii
  }
  & icacls.exe $authorizedKeysPath /inheritance:r /grant:r "*S-1-5-32-544:F" "*S-1-5-18:F" | Out-Null
  if ($LASTEXITCODE -ne 0) {
    throw "Could not secure the Windows OpenSSH authorized-keys file."
  }

  Set-Service -Name sshd -StartupType Automatic
  Start-Service -Name sshd

  # The optional-feature installer creates a broad port-22 rule. Replace its
  # effect with the Blueberry-only rule below.
  Get-NetFirewallRule -Name "OpenSSH-Server-In-TCP" -ErrorAction SilentlyContinue |
    Disable-NetFirewallRule
  $sshRuleName = "OpenSSH over Tailscale from Blueberry"
  $sshRule = Get-NetFirewallRule -DisplayName $sshRuleName -ErrorAction SilentlyContinue
  if ($null -eq $sshRule) {
    New-NetFirewallRule `
      -DisplayName $sshRuleName `
      -Direction Inbound `
      -Action Allow `
      -Protocol TCP `
      -LocalPort 22 `
      -RemoteAddress "100.79.75.53" `
      -Profile Any | Out-Null
  } else {
    $sshRule | Set-NetFirewallRule -Enabled True -Direction Inbound -Action Allow -Profile Any
    $sshRule | Get-NetFirewallPortFilter |
      Set-NetFirewallPortFilter -Protocol TCP -LocalPort 22
    $sshRule | Get-NetFirewallAddressFilter |
      Set-NetFirewallAddressFilter -RemoteAddress "100.79.75.53"
  }
} else {
  Write-Host "SSH SKIPPED: Windows optional feature is $sshState."
}

$listener = Get-NetTCPConnection -LocalPort $rdpPort -State Listen -ErrorAction SilentlyContinue
$tailscaleIp = & $tailscale ip -4
$rdpReady = $null -ne $listener
$terminalService = Get-CimInstance Win32_Service -Filter "Name='TermService'"
$sessionEnv = Get-Service -Name SessionEnv -ErrorAction SilentlyContinue
$userModeRdp = Get-Service -Name UmRdpService -ErrorAction SilentlyContinue
$policyPath = "HKLM:\SOFTWARE\Policies\Microsoft\Windows NT\Terminal Services"
$policy = Get-ItemProperty $policyPath -ErrorAction SilentlyContinue
$terminalSettings = Get-CimInstance `
  -Namespace "root\cimv2\TerminalServices" `
  -ClassName "Win32_TerminalServiceSetting"
$diagnostic = [ordered]@{
  user = $env:USERNAME
  product = $windows.ProductName
  edition = $windows.EditionID
  build = $windows.CurrentBuild
  rdpReady = $rdpReady
  rdpPort = $rdpPort
  allowResult = $allowResult.ReturnValue
  allowTSConnections = $terminalSettings.AllowTSConnections
  denyConnections = (Get-ItemProperty $terminalServer).fDenyTSConnections
  policyDenyConnections = $policy.fDenyTSConnections
  enableWinStation = (Get-ItemProperty $rdpTcp).fEnableWinStation
  termServiceState = $terminalService.State
  termServiceStartMode = $terminalService.StartMode
  termServicePid = $terminalService.ProcessId
  sessionEnv = $sessionEnv.Status
  umRdpService = $userModeRdp.Status
  ssh = $sshState
}
$diagnosticJson = $diagnostic | ConvertTo-Json -Compress
$diagnosticBytes = [Text.Encoding]::UTF8.GetBytes($diagnosticJson)
$diagnosticEncoded = [Uri]::EscapeDataString([Convert]::ToBase64String($diagnosticBytes))
try {
  Invoke-WebRequest -UseBasicParsing -Uri "http://blueberry:8765/diag?d=$diagnosticEncoded" | Out-Null
} catch {
  # The tiny setup server intentionally returns 404 for this status beacon;
  # its request log is the feedback channel until SSH is available.
}

if ($sshState -eq "Installed") {
  Write-Host "SSH READY: $($env:USERNAME)@${tailscaleIp}"
}
if (-not $rdpReady) {
  $service = Get-Service -Name TermService
  throw "RDP is not listening on TCP $rdpPort (TermService: $($service.Status)); diagnostic report sent."
}
Write-Host "RDP READY: ${tailscaleIp}:$rdpPort"
