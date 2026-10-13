# theme-by-day.ps1 — light mode during Pacific daytime (07:00–19:00), dark otherwise.
# Runs from the "ac-theme-by-day" scheduled task every 15 minutes and at sign-in.
# Only writes when the mode actually changes, then tells running apps
# (ImmersiveColorSet), so the taskbar and Menu Band flip together.
$pt = [System.TimeZoneInfo]::ConvertTimeBySystemTimeZoneId([DateTime]::UtcNow, 'Pacific Standard Time')
$light = if ($pt.Hour -ge 7 -and $pt.Hour -lt 19) { 1 } else { 0 }
$key = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize'
$now = (Get-ItemProperty $key -ErrorAction SilentlyContinue)
if ($now.AppsUseLightTheme -eq $light -and $now.SystemUsesLightTheme -eq $light) { return }
Set-ItemProperty $key AppsUseLightTheme $light -Type DWord
Set-ItemProperty $key SystemUsesLightTheme $light -Type DWord
Add-Type -Namespace AC -Name Theme -MemberDefinition @'
[DllImport("user32.dll", CharSet = CharSet.Unicode)]
public static extern IntPtr SendMessageTimeout(IntPtr hWnd, uint Msg, UIntPtr wParam, string lParam, uint flags, uint timeout, out UIntPtr result);
'@
$r = [UIntPtr]::Zero
[void][AC.Theme]::SendMessageTimeout([IntPtr]0xffff, 0x1A, [UIntPtr]::Zero, 'ImmersiveColorSet', 2, 3000, [ref]$r)
"$(Get-Date -Format s) PT $($pt.ToString('HH:mm')) -> $(if ($light) {'light'} else {'dark'})" | Add-Content "$env:LOCALAPPDATA\theme-by-day.log"
