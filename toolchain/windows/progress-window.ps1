# progress-window.ps1 — a large on-screen display on the MiniBook: the current
# step in big type, the compiler's lines under it. Borderless, translucent,
# topmost and click-through, so it never takes the laptop away from whoever is
# using it. It tails ~\ac-progress.log (timestamped lines are steps; indented
# lines are build output) and dims a little after 20 s with nothing new.
Add-Type -AssemblyName System.Windows.Forms, System.Drawing
Add-Type @'
using System; using System.Runtime.InteropServices;
public static class ClickThrough {
  [DllImport("user32.dll")] static extern int GetWindowLong(IntPtr h, int i);
  [DllImport("user32.dll")] static extern int SetWindowLong(IntPtr h, int i, int v);
  public static void Apply(IntPtr h) { SetWindowLong(h, -20, GetWindowLong(h, -20) | 0x20 | 0x80000 | 0x80); }
}
'@
$log = Join-Path $HOME 'ac-progress.log'
if (-not (Test-Path $log)) { Set-Content $log ((Get-Date -Format HH:mm:ss) + '  ready') }
$area = [Windows.Forms.Screen]::PrimaryScreen.WorkingArea
$form = New-Object Windows.Forms.Form
$form.FormBorderStyle = 'None'; $form.ShowInTaskbar = $false; $form.TopMost = $true
$form.StartPosition = 'Manual'
$form.Size = New-Object Drawing.Size([int]($area.Width * .72), [int]($area.Height * .44))
# Top left of the display.
$form.Location = New-Object Drawing.Point(($area.Left + 16), ($area.Top + 16))
$form.BackColor = [Drawing.Color]::FromArgb(34, 36, 46)
$form.Opacity = .96
$head = New-Object Windows.Forms.Label
$head.Dock = 'Top'; $head.Height = [int]($form.Height * .36)
$head.Font = New-Object Drawing.Font('Segoe UI Semibold', 44)
$head.ForeColor = [Drawing.Color]::FromArgb(255, 255, 255)
$head.Padding = New-Object Windows.Forms.Padding(28, 18, 28, 0); $head.AutoEllipsis = $true
$body = New-Object Windows.Forms.Label
$body.Dock = 'Fill'
$body.Font = New-Object Drawing.Font('Consolas', 17)
$body.ForeColor = [Drawing.Color]::FromArgb(212, 218, 228)
$body.Padding = New-Object Windows.Forms.Padding(32, 4, 28, 12)
$form.Controls.Add($body); $form.Controls.Add($head)
$form.Add_Shown({ [ClickThrough]::Apply($form.Handle) })
$script:lastSize = -1; $script:changedAt = Get-Date; $script:step = ''
# How old the headline is, said in words: a step that stopped moving has to
# look stopped. Past ten minutes the headline goes gray and says idle.
function Age($seconds) {
  if ($seconds -lt 60) { return 'just now' }
  if ($seconds -lt 3600) { return ('{0} min ago' -f [int]($seconds / 60)) }
  return ('{0} h ago' -f [math]::Round($seconds / 3600, 1))
}
$timer = New-Object Windows.Forms.Timer; $timer.Interval = 400
$timer.Add_Tick({
  try {
    $size = (Get-Item $log).Length
    if ($size -ne $script:lastSize) {
      $script:lastSize = $size; $script:changedAt = Get-Date
      $lines = @(Get-Content $log -Tail 40 -ErrorAction Stop)
      $step = ($lines | Where-Object { $_ -match '^\d\d:\d\d:\d\d  ' } | Select-Object -Last 1)
      $script:step = if ($step) { $step.Substring(10) } else { '' }
      $script:failed = $step -match 'FAIL'
      $body.Text = ($lines | Select-Object -Last 9) -join "`r`n"
    }
    $idle = ((Get-Date) - $script:changedAt).TotalSeconds
    if ($idle -ge 600) {
      $head.Text = 'idle  ·  last: ' + $script:step + '  ·  ' + (Age $idle)
      $head.ForeColor = [Drawing.Color]::FromArgb(150, 156, 168)
    } else {
      $head.Text = $script:step + '  ·  ' + (Age $idle)
      $head.ForeColor = if ($script:failed) { [Drawing.Color]::FromArgb(255, 120, 130) } else { [Drawing.Color]::FromArgb(255, 255, 255) }
    }
    $form.Opacity = if ($idle -lt 20) { .96 } else { .7 }
  } catch {}
})
$timer.Start()
[void]$form.ShowDialog()
