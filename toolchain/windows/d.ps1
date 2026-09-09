$ErrorActionPreference = "Stop"

$identity = [Security.Principal.WindowsIdentity]::GetCurrent()
$principal = [Security.Principal.WindowsPrincipal]::new($identity)
if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
  throw "Run Windows Terminal as Administrator, then run this command again."
}

$settings = Join-Path $env:ProgramData "Deskflow\surface-client.conf"
if (-not (Test-Path $settings)) {
  throw "Surface Deskflow settings are missing; run deskflow.ps1 first."
}

$content = Get-Content $settings
$content = $content -replace '^remoteHost=.*$', 'remoteHost=100.79.75.53'
$content | Set-Content -Path $settings -Encoding ascii

$deskflowData = Join-Path $env:ProgramData "Deskflow"
$tls = Join-Path $deskflowData "tls"
$certificatePath = Join-Path $tls "deskflow.pem"
$canonicalSettings = Join-Path $deskflowData "Deskflow.conf"
New-Item -ItemType Directory -Force -Path $tls | Out-Null

# deskflow-core expects a client certificate but does not create one. The GUI
# owns that first-run step, so give it the same settings and let it run only
# long enough to generate the PEM and local fingerprint database.
if (-not (Test-Path $certificatePath)) {
  $gui = Get-ChildItem (Join-Path $env:ProgramFiles "Deskflow") `
    -Filter "deskflow.exe" `
    -File `
    -Recurse `
    -ErrorAction SilentlyContinue |
    Select-Object -First 1 -ExpandProperty FullName
  if ($null -eq $gui) {
    throw "deskflow.exe was not found, so the TLS certificate could not be created."
  }

  Copy-Item -Force $settings $canonicalSettings
  Get-Process -Name "deskflow-core", "deskflow" -ErrorAction SilentlyContinue |
    Stop-Process -Force
  Start-Process -FilePath $gui
  for ($attempt = 0; $attempt -lt 40 -and -not (Test-Path $certificatePath); $attempt++) {
    Start-Sleep -Milliseconds 500
  }
  Get-Process -Name "deskflow-core", "deskflow" -ErrorAction SilentlyContinue |
    Stop-Process -Force
  if (-not (Test-Path $certificatePath)) {
    throw "Deskflow opened but did not create $certificatePath."
  }
}

$core = Get-ChildItem (Join-Path $env:ProgramFiles "Deskflow") `
  -Filter "deskflow-core.exe" `
  -File `
  -Recurse `
  -ErrorAction SilentlyContinue |
  Select-Object -First 1 -ExpandProperty FullName
if ($null -eq $core) {
  throw "deskflow-core.exe was not found."
}

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

# The scheduled task supersedes the earlier HKCU Run entry and avoids two
# clients racing each other at sign-in.
Remove-ItemProperty `
  -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Run" `
  -Name "AestheticComputerDeskflow" `
  -ErrorAction SilentlyContinue

# Disable Teams at sign-in without uninstalling it. This covers classic Teams'
# Run value/config and the packaged MSTeams StartupTask used by newer releases.
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
Start-Sleep -Seconds 3

$running = $null -ne (Get-Process -Name "deskflow-core" -ErrorAction SilentlyContinue)
$fingerprint = "missing"
if (Test-Path $certificatePath) {
  try {
    $pem = Get-Content $certificatePath -Raw
    $match = [regex]::Match(
      $pem,
      '-----BEGIN CERTIFICATE-----\s*(?<data>.*?)\s*-----END CERTIFICATE-----',
      [Text.RegularExpressions.RegexOptions]::Singleline
    )
    if ($match.Success) {
      $der = [Convert]::FromBase64String(($match.Groups['data'].Value -replace '\s', ''))
      $certificate = [Security.Cryptography.X509Certificates.X509Certificate2]::new($der)
      $sha256 = [Security.Cryptography.SHA256]::Create()
      $fingerprint = ([BitConverter]::ToString($sha256.ComputeHash($certificate.RawData))).Replace('-', '').ToLowerInvariant()
    }
  } catch {
  }
}
try {
  Invoke-WebRequest -UseBasicParsing -Uri "http://blueberry:8765/deskflow-connect?running=$running&fingerprint=$fingerprint" | Out-Null
} catch {
}

if (-not $running) {
  throw "Deskflow client exited."
}
Write-Host "DESKFLOW CONNECTING: 100.79.75.53"
Write-Host "CLIENT FINGERPRINT: $fingerprint"
Write-Host "AUTOSTART READY: Deskflow will reconnect whenever $($identity.Name) signs in."
Write-Host "TEAMS LOGIN DISABLED: Teams remains installed for manual use."
