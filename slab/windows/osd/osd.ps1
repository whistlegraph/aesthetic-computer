# osd.ps1 — turn the build glass (progress-window.ps1) on or off.
#   powershell -File osd.ps1 off   disable the ac-osd watchdog task and close the window
#   powershell -File osd.ps1 on    enable the task and start it (the window opens in the signed-in session)
# Safe over ssh: the task, not this process, owns the window.
param([ValidateSet('on', 'off', 'status')][string]$State = 'status')
$windows = { Get-CimInstance Win32_Process -Filter "Name='powershell.exe'" | Where-Object { $_.CommandLine -like '* -File *progress-window.ps1*' } }
switch ($State) {
  'off' { Disable-ScheduledTask ac-osd | Out-Null; & $windows | ForEach-Object { Stop-Process -Id $_.ProcessId -Force } }
  'on'  { Enable-ScheduledTask ac-osd | Out-Null; Start-ScheduledTask ac-osd; Start-Sleep 2 }
}
"ac-osd task: $((Get-ScheduledTask ac-osd).State); window running: $([bool](& $windows))"
