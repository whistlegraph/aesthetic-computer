$ErrorActionPreference = "Stop"

$identity = [Security.Principal.WindowsIdentity]::GetCurrent()
$principal = [Security.Principal.WindowsPrincipal]::new($identity)
if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
  throw "Run Windows Terminal as Administrator, then run this command again."
}

$version = "1.25.0"
$architecture = if ($env:PROCESSOR_ARCHITECTURE -eq "ARM64") { "arm64" } else { "x64" }
$digests = @{
  arm64 = "61a74ee7a92c921e74948ba1112fd5cb63894e35ec3b28c63aa3f2c372b8a3fe"
  x64 = "a766301eeb2ca3d3a960128020198fd9c7b65c1de13d86f169a6e6ea1fb82424"
}
$installerName = "deskflow-$version-win-$architecture.msi"
$installer = Join-Path $env:TEMP $installerName
$installerUri = "https://github.com/deskflow/deskflow/releases/download/v$version/$installerName"

Write-Host "Downloading Deskflow $version for $architecture..."
Invoke-WebRequest -UseBasicParsing -Uri $installerUri -OutFile $installer
$actualDigest = (Get-FileHash -Algorithm SHA256 $installer).Hash.ToLowerInvariant()
if ($actualDigest -ne $digests[$architecture]) {
  throw "Deskflow installer checksum did not match its official release digest."
}

Get-Process -Name "deskflow-core", "deskflow" -ErrorAction SilentlyContinue |
  Stop-Process -Force
$uninstallRoots = @(
  "HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*",
  "HKLM:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\*"
)
$installedDeskflow = Get-ItemProperty $uninstallRoots -ErrorAction SilentlyContinue |
  Where-Object { $_.DisplayName -like "Deskflow*" }
foreach ($installed in $installedDeskflow) {
  if ($installed.PSChildName -match "^\{[0-9A-F-]+\}$") {
    $remove = Start-Process msiexec.exe `
      -ArgumentList @("/x", $installed.PSChildName, "/qn", "/norestart") `
      -Wait `
      -PassThru
    if ($remove.ExitCode -notin @(0, 1605, 3010)) {
      throw "Existing Deskflow uninstall failed with exit code $($remove.ExitCode)."
    }
  }
}

$install = Start-Process msiexec.exe `
  -ArgumentList @("/i", "`"$installer`"", "/qn", "/norestart") `
  -Wait `
  -PassThru
if ($install.ExitCode -notin @(0, 3010)) {
  throw "Deskflow installer failed with exit code $($install.ExitCode)."
}

$core = Get-ChildItem (Join-Path $env:ProgramFiles "Deskflow") `
  -Filter "deskflow-core.exe" `
  -File `
  -Recurse `
  -ErrorAction SilentlyContinue |
  Select-Object -First 1 -ExpandProperty FullName
if ($null -eq $core) {
  throw "Deskflow installed, but deskflow-core.exe was not found."
}

$deskflowData = Join-Path $env:ProgramData "Deskflow"
$tls = Join-Path $deskflowData "tls"
$settings = Join-Path $deskflowData "surface-client.conf"
$log = Join-Path $deskflowData "surface-client.log"
$certificate = (Join-Path $tls "deskflow.pem").Replace('\', '/')
New-Item -ItemType Directory -Force -Path $tls | Out-Null

@"
[client]
dynamicConnectionInterval=true
remoteHost=100.79.75.53

[core]
computerName=Skips-MSFT-Surface-3-Laptop
coreMode=1
lastVersion=1.25.0.0
port=24800
processMode=0

[gui]
enableUpdateCheck=false
startCoreWithGui=false

[log]
file=$log
level=INFO
toFile=true

[security]
certificate=$certificate
checkPeerFingerprints=true
tlsEnabled=true
"@ | Set-Content -Path $settings -Encoding ascii

@(
  "v2:sha256:0f3ad9948c903ef4881a3e7092ea511572b4d57bbe12abac2993bab7252d0ce1",
  "v2:sha256:76208504df49a431b6183cadb6478ba8bc43b1f14d7f12e4fa4772d48404834c"
) | Set-Content -Path (Join-Path $tls "trusted-servers") -Encoding ascii

$taskName = "Aesthetic Computer Deskflow"
$taskAction = New-ScheduledTaskAction `
  -Execute $core `
  -Argument "client -s `"$settings`""
$taskTrigger = New-ScheduledTaskTrigger -AtLogOn -User $identity.Name
$taskPrincipal = New-ScheduledTaskPrincipal `
  -UserId $identity.Name `
  -LogonType Interactive `
  -RunLevel Highest
$taskSettings = New-ScheduledTaskSettingsSet `
  -AllowStartIfOnBatteries `
  -DontStopIfGoingOnBatteries `
  -MultipleInstances IgnoreNew `
  -RestartCount 20 `
  -RestartInterval (New-TimeSpan -Minutes 1)
Register-ScheduledTask `
  -TaskName $taskName `
  -Action $taskAction `
  -Trigger $taskTrigger `
  -Principal $taskPrincipal `
  -Settings $taskSettings `
  -Force | Out-Null
Remove-ItemProperty `
  -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Run" `
  -Name "AestheticComputerDeskflow" `
  -ErrorAction SilentlyContinue

$runPaths = @(
  "HKCU:\Software\Microsoft\Windows\CurrentVersion\Run",
  "HKCU:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Run"
)
foreach ($runPath in $runPaths) {
  if (-not (Test-Path $runPath)) { continue }
  $runProperties = (Get-ItemProperty $runPath).PSObject.Properties |
    Where-Object { $_.Name -notlike "PS*" -and ($_.Name -match "(?i)teams" -or $_.Value -match "(?i)teams") }
  foreach ($runProperty in $runProperties) {
    Remove-ItemProperty -Path $runPath -Name $runProperty.Name -ErrorAction SilentlyContinue
  }
}
$packagedApps = "HKCU:\Software\Classes\Local Settings\Software\Microsoft\Windows\CurrentVersion\AppModel\SystemAppData"
if (Test-Path $packagedApps) {
  Get-ChildItem $packagedApps -ErrorAction SilentlyContinue |
    Where-Object { $_.PSChildName -match "(?i)teams" } |
    ForEach-Object {
      Get-ChildItem $_.PSPath -Recurse -ErrorAction SilentlyContinue |
        Where-Object { $_.PSChildName -match "(?i)StartupTask" } |
        ForEach-Object {
          New-ItemProperty -Path $_.PSPath -Name "State" -Value 1 -PropertyType DWord -Force | Out-Null
          New-ItemProperty -Path $_.PSPath -Name "UserEnabledStartupOnce" -Value 0 -PropertyType DWord -Force | Out-Null
        }
    }
}
$classicTeamsConfig = Join-Path $env:APPDATA "Microsoft\Teams\desktop-config.json"
if (Test-Path $classicTeamsConfig) {
  $teamsConfig = Get-Content $classicTeamsConfig -Raw
  $teamsConfig = $teamsConfig -replace '(?i)("openAtLogin"\s*:\s*)true', '${1}false'
  $teamsConfig | Set-Content -Path $classicTeamsConfig -Encoding utf8
}

Get-Process -Name "deskflow-core" -ErrorAction SilentlyContinue |
  Stop-Process -Force
Start-Process -FilePath $core -ArgumentList @("client", "-s", "`"$settings`"")
Start-Sleep -Seconds 4

$running = $null -ne (Get-Process -Name "deskflow-core" -ErrorAction SilentlyContinue)
$logTail = if (Test-Path $log) { (Get-Content $log -Tail 30) -join "`n" } else { "log missing" }
$report = [ordered]@{
  user = $env:USERNAME
  running = $running
  architecture = $architecture
  version = $version
  log = $logTail
}
$reportJson = $report | ConvertTo-Json -Compress
$reportBytes = [Text.Encoding]::UTF8.GetBytes($reportJson)
$reportEncoded = [Uri]::EscapeDataString([Convert]::ToBase64String($reportBytes))
try {
  Invoke-WebRequest -UseBasicParsing -Uri "http://blueberry:8765/deskflow-report?d=$reportEncoded" | Out-Null
} catch {
  # The temporary setup server records the request and intentionally returns
  # 404. Once Deskflow connects, its TLS log becomes the feedback channel.
}

if (-not $running) {
  throw "Deskflow installed but its client process exited; check $log."
}
Write-Host "DESKFLOW READY: move Blueberry's pointer off its right edge."
