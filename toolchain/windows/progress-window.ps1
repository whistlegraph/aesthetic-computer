# progress-window.ps1 — a large on-screen display on the MiniBook: the current
# step in big type, the compiler's lines under it. Borderless, translucent,
# topmost and click-through, so it never takes the laptop away from whoever is
# using it. It tails ~\ac-progress.log (timestamped lines are steps; indented
# lines are build output) and fades back after 20 s with nothing new.
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
$form.Size = New-Object Drawing.Size([int]($area.Width * .9), [int]($area.Height * .44))
$form.Location = New-Object Drawing.Point(($area.Left + ($area.Width - $form.Width) / 2), ($area.Bottom - $form.Height - 24))
$form.BackColor = [Drawing.Color]::FromArgb(18, 19, 24)
$form.Opacity = .88
$head = New-Object Windows.Forms.Label
$head.Dock = 'Top'; $head.Height = [int]($form.Height * .36)
$head.Font = New-Object Drawing.Font('Segoe UI Semibold', 44)
$head.ForeColor = [Drawing.Color]::FromArgb(245, 247, 250)
$head.Padding = New-Object Windows.Forms.Padding(28, 18, 28, 0); $head.AutoEllipsis = $true
$body = New-Object Windows.Forms.Label
$body.Dock = 'Fill'
$body.Font = New-Object Drawing.Font('Consolas', 17)
$body.ForeColor = [Drawing.Color]::FromArgb(150, 158, 172)
$body.Padding = New-Object Windows.Forms.Padding(32, 4, 28, 12)
$form.Controls.Add($body); $form.Controls.Add($head)
$form.Add_Shown({ [ClickThrough]::Apply($form.Handle) })
$script:lastSize = -1; $script:changedAt = Get-Date
$timer = New-Object Windows.Forms.Timer; $timer.Interval = 400
$timer.Add_Tick({
  try {
    $size = (Get-Item $log).Length
    if ($size -ne $script:lastSize) {
      $script:lastSize = $size; $script:changedAt = Get-Date
      $lines = @(Get-Content $log -Tail 40 -ErrorAction Stop)
      $step = ($lines | Where-Object { $_ -match '^\d\d:\d\d:\d\d  ' } | Select-Object -Last 1)
      $head.Text = if ($step) { $step.Substring(10) } else { '' }
      $head.ForeColor = if ($step -match 'FAIL') { [Drawing.Color]::FromArgb(255, 110, 120) } else { [Drawing.Color]::FromArgb(245, 247, 250) }
      $body.Text = ($lines | Select-Object -Last 9) -join "`r`n"
    }
    $idle = ((Get-Date) - $script:changedAt).TotalSeconds
    $form.Opacity = if ($idle -lt 20) { .88 } else { .28 }
  } catch {}
})
$timer.Start()
[void]$form.ShowDialog()
