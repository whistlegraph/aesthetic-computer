# progress-watchdog.ps1 — keep the on-screen display alive. Registered as the
# `ac-osd` scheduled task (at sign-in, then every 5 minutes, in the signed-in
# session): starts progress-window.ps1 when no copy is running.
$running = Get-CimInstance Win32_Process -Filter "Name='powershell.exe'" |
  Where-Object { $_.CommandLine -like '*progress-window.ps1*' }
if (-not $running) {
  Start-Process powershell -WindowStyle Hidden -ArgumentList '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', (Join-Path $HOME 'progress-window.ps1')
}
