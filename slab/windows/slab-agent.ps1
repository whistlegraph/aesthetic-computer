# slab-agent.ps1 — the slab `frame` agent for a Windows 11 box.
#
# Speaks the same file protocol as FrameCapture.swift inside the Mac
# menubar app, so `frame <machine>` on a Mac works unchanged against a
# Windows machine (the client ships its request over `ssh host bash -s`,
# which Git bash answers here):
#
#   ~/.local/share/slab/state/frame.req       mode line, written by the client
#   ~/.local/share/slab/state/frame.out.json  envelope
#   ~/.local/share/slab/state/frame.out.jpg   thumbnail (≤ 1568 px wide)
#   ~/.local/share/slab/state/frame.done      touched when both are written
#
# Mode tokens honoured: window (default) | screen, noocr, fast, novisual,
# crop=x,y,w,h, cursor=x,y, session=…, native-click=<b64 json>,
# native-drag=<b64 json>, native-guard=<b64 json>. Unknown tokens are ignored.
#
# Observes with GDI (CopyFromScreen), reads text with Windows.Media.Ocr, walks
# the foreground window with UI Automation, and sends real input with
# SendInput. Must run INSIDE the signed-in desktop session (start it through
# the ac-desk task or the Run key), never from an ssh session: those have no
# desktop to capture. Windows PowerShell 5.1 only — it is the one with WinRT.

$ErrorActionPreference = 'Continue'
Add-Type -AssemblyName System.Drawing, System.Windows.Forms, System.Runtime.WindowsRuntime, UIAutomationClient, UIAutomationTypes

Add-Type @"
using System;
using System.Runtime.InteropServices;
using System.Text;
public class SlabNative {
  [DllImport("user32.dll")] public static extern bool SetProcessDPIAware();
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
  [DllImport("user32.dll")] public static extern bool GetWindowRect(IntPtr h, out RECT r);
  [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);
  [DllImport("user32.dll", CharSet = CharSet.Unicode)] public static extern int GetWindowTextW(IntPtr h, StringBuilder s, int n);
  [DllImport("user32.dll")] public static extern bool GetCursorPos(out POINT p);
  [DllImport("user32.dll")] public static extern bool SetCursorPos(int x, int y);
  [DllImport("user32.dll")] public static extern uint SendInput(uint n, INPUT[] inputs, int size);
  [DllImport("user32.dll")] public static extern bool EnumWindows(EnumProc cb, IntPtr l);
  [DllImport("user32.dll")] public static extern bool IsWindowVisible(IntPtr h);
  [DllImport("dwmapi.dll")] public static extern int DwmGetWindowAttribute(IntPtr h, int attr, out RECT r, int size);
  public delegate bool EnumProc(IntPtr h, IntPtr l);
  [StructLayout(LayoutKind.Sequential)] public struct RECT { public int Left, Top, Right, Bottom; }
  [StructLayout(LayoutKind.Sequential)] public struct POINT { public int X, Y; }
  [StructLayout(LayoutKind.Sequential)] public struct MOUSEINPUT { public int dx, dy; public uint mouseData, dwFlags, time; public IntPtr dwExtraInfo; }
  [StructLayout(LayoutKind.Sequential)] public struct KEYBDINPUT { public ushort wVk, wScan; public uint dwFlags, time; public IntPtr dwExtraInfo; }
  [StructLayout(LayoutKind.Explicit)] public struct INPUTUNION { [FieldOffset(0)] public MOUSEINPUT mi; [FieldOffset(0)] public KEYBDINPUT ki; }
  [StructLayout(LayoutKind.Sequential)] public struct INPUT { public uint type; public INPUTUNION u; }
  public static void Mouse(uint flags, int dx, int dy, uint data) {
    INPUT[] i = new INPUT[1]; i[0].type = 0; i[0].u.mi.dwFlags = flags; i[0].u.mi.dx = dx; i[0].u.mi.dy = dy; i[0].u.mi.mouseData = data;
    SendInput(1, i, Marshal.SizeOf(typeof(INPUT)));
  }
  public static void Key(ushort vk, bool up) {
    INPUT[] i = new INPUT[1]; i[0].type = 1; i[0].u.ki.wVk = vk; i[0].u.ki.dwFlags = up ? 2u : 0u;
    SendInput(1, i, Marshal.SizeOf(typeof(INPUT)));
  }
}
"@
[SlabNative]::SetProcessDPIAware() | Out-Null

# ── WinRT OCR plumbing (Windows PowerShell 5.1) ──────────────────────────
[void][Windows.Media.Ocr.OcrEngine, Windows.Foundation, ContentType = WindowsRuntime]
[void][Windows.Graphics.Imaging.BitmapDecoder, Windows.Foundation, ContentType = WindowsRuntime]
[void][Windows.Storage.Streams.InMemoryRandomAccessStream, Windows.Foundation, ContentType = WindowsRuntime]
[void][Windows.Globalization.Language, Windows.Foundation, ContentType = WindowsRuntime]
$asTaskGeneric = ([System.WindowsRuntimeSystemExtensions].GetMethods() | Where-Object {
  $_.Name -eq 'AsTask' -and $_.GetParameters().Count -eq 1 -and $_.GetParameters()[0].ParameterType.Name -eq 'IAsyncOperation`1' })[0]
function Await($op, $type) {
  $t = $asTaskGeneric.MakeGenericMethod($type).Invoke($null, @($op)); $t.Wait(-1) | Out-Null; $t.Result
}
$script:ocr = [Windows.Media.Ocr.OcrEngine]::TryCreateFromUserProfileLanguages()
if (-not $script:ocr) { $script:ocr = [Windows.Media.Ocr.OcrEngine]::TryCreateFromLanguage([Windows.Globalization.Language]::new('en-US')) }

function Ocr-Bitmap([System.Drawing.Bitmap]$bmp, [int]$ox, [int]$oy) {
  if (-not $script:ocr) { return @() }
  $ms = New-Object System.IO.MemoryStream
  $bmp.Save($ms, [System.Drawing.Imaging.ImageFormat]::Bmp); $ms.Position = 0
  $ras = [System.IO.WindowsRuntimeStreamExtensions]::AsRandomAccessStream($ms)
  $dec = Await ([Windows.Graphics.Imaging.BitmapDecoder]::CreateAsync($ras)) ([Windows.Graphics.Imaging.BitmapDecoder])
  $sb = Await ($dec.GetSoftwareBitmapAsync()) ([Windows.Graphics.Imaging.SoftwareBitmap])
  $res = Await ($script:ocr.RecognizeAsync($sb)) ([Windows.Media.Ocr.OcrResult])
  $out = New-Object System.Collections.ArrayList
  foreach ($line in $res.Lines) {
    $x0 = 1e9; $y0 = 1e9; $x1 = -1e9; $y1 = -1e9
    foreach ($w in $line.Words) { $r = $w.BoundingRect; if ($r.X -lt $x0) { $x0 = $r.X }; if ($r.Y -lt $y0) { $y0 = $r.Y }; if ($r.X + $r.Width -gt $x1) { $x1 = $r.X + $r.Width }; if ($r.Y + $r.Height -gt $y1) { $y1 = $r.Y + $r.Height } }
    if ($x1 -le $x0) { continue }
    [void]$out.Add([ordered]@{ t = $line.Text; cx = [int]($ox + ($x0 + $x1) / 2); cy = [int]($oy + ($y0 + $y1) / 2); r = @([int]($ox + $x0), [int]($oy + $y0), [int]($x1 - $x0), [int]($y1 - $y0)) })
  }
  $ms.Dispose()
  ,$out.ToArray()
}

# ── UI Automation (the AX tree's cousin) ────────────────────────────────
$roleMap = @{ 'ControlType.Button' = 'AXButton'; 'ControlType.Edit' = 'AXTextField'; 'ControlType.Text' = 'AXStaticText'; 'ControlType.CheckBox' = 'AXCheckBox';
  'ControlType.RadioButton' = 'AXRadioButton'; 'ControlType.ComboBox' = 'AXPopUpButton'; 'ControlType.Hyperlink' = 'AXLink'; 'ControlType.MenuItem' = 'AXMenuItem';
  'ControlType.ListItem' = 'AXRow'; 'ControlType.TabItem' = 'AXRadioButton'; 'ControlType.Slider' = 'AXSlider'; 'ControlType.Image' = 'AXImage'; 'ControlType.Window' = 'AXWindow' }
function Walk-Uia([IntPtr]$hwnd, [int]$cap, [int]$budgetMs) {
  $els = New-Object System.Collections.ArrayList
  try {
    $root = [System.Windows.Automation.AutomationElement]::FromHandle($hwnd)
    $walker = [System.Windows.Automation.TreeWalker]::ControlViewWalker
    $queue = New-Object System.Collections.Generic.Queue[System.Windows.Automation.AutomationElement]
    $queue.Enqueue($root)
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    while ($queue.Count -gt 0 -and $els.Count -lt $cap -and $sw.ElapsedMilliseconds -lt $budgetMs) {
      $el = $queue.Dequeue()
      try {
        $c = $el.Current
        if (-not $c.IsOffscreen -and $c.BoundingRectangle.Width -gt 0) {
          $ct = $c.ControlType.ProgrammaticName
          $role = if ($roleMap.ContainsKey($ct)) { $roleMap[$ct] } else { 'UIA' + ($ct -replace 'ControlType\.', '') }
          $acts = New-Object System.Collections.ArrayList
          if ($el.GetCurrentPropertyValue([System.Windows.Automation.AutomationElement]::IsInvokePatternAvailableProperty)) { [void]$acts.Add('AXPress') }
          if ($el.GetCurrentPropertyValue([System.Windows.Automation.AutomationElement]::IsTogglePatternAvailableProperty)) { [void]$acts.Add('AXPress') }
          if ($el.GetCurrentPropertyValue([System.Windows.Automation.AutomationElement]::IsValuePatternAvailableProperty)) { [void]$acts.Add('AXSetValue') }
          if ($c.Name -or $acts.Count -gt 0) {
            $b = $c.BoundingRectangle
            [void]$els.Add([ordered]@{ role = $role; title = [string]$c.Name; cx = [int]($b.X + $b.Width / 2); cy = [int]($b.Y + $b.Height / 2);
              r = @([int]$b.X, [int]$b.Y, [int]$b.Width, [int]$b.Height); actions = @($acts | Select-Object -Unique) })
          }
        }
      } catch {}
      try { $child = $walker.GetFirstChild($el); while ($child -ne $null) { $queue.Enqueue($child); $child = $walker.GetNextSibling($child) } } catch {}
    }
  } catch {}
  ,$els.ToArray()
}

# ── helpers ─────────────────────────────────────────────────────────────
function Window-Title([IntPtr]$h) { $sb = New-Object System.Text.StringBuilder 512; [void][SlabNative]::GetWindowTextW($h, $sb, 512); $sb.ToString() }
function Window-Bounds([IntPtr]$h) {
  $r = New-Object SlabNative+RECT
  if ([SlabNative]::DwmGetWindowAttribute($h, 9, [ref]$r, 16) -ne 0) { [void][SlabNative]::GetWindowRect($h, [ref]$r) }
  $r
}
function Visible-Windows([int]$cap) {
  $list = New-Object System.Collections.ArrayList
  $cb = [SlabNative+EnumProc]{ param($h, $l)
    if ($list.Count -ge $cap) { return $false }
    if ([SlabNative]::IsWindowVisible($h)) { $t = Window-Title $h; if ($t) { $b = Window-Bounds $h; $wpid = 0; [void][SlabNative]::GetWindowThreadProcessId($h, [ref]$wpid)
      $app = try { (Get-Process -Id $wpid -ErrorAction Stop).ProcessName } catch { '?' }
      [void]$list.Add([ordered]@{ app = $app; title = $t; pid = [int]$wpid; x = $b.Left; y = $b.Top; w = $b.Right - $b.Left; h = $b.Bottom - $b.Top }) } }
    $true }
  [void][SlabNative]::EnumWindows($cb, [IntPtr]::Zero)
  ,$list.ToArray()
}
function Save-Jpeg([System.Drawing.Bitmap]$bmp, [string]$path, [int]$maxW) {
  $img = $bmp
  if ($bmp.Width -gt $maxW) { $h = [int]($bmp.Height * $maxW / $bmp.Width); $img = New-Object System.Drawing.Bitmap $bmp, $maxW, $h }
  $codec = [System.Drawing.Imaging.ImageCodecInfo]::GetImageEncoders() | Where-Object { $_.MimeType -eq 'image/jpeg' }
  $ep = New-Object System.Drawing.Imaging.EncoderParameters 1
  $ep.Param[0] = New-Object System.Drawing.Imaging.EncoderParameter ([System.Drawing.Imaging.Encoder]::Quality), 72L
  $img.Save($path, $codec, $ep)
  if ($img -ne $bmp) { $img.Dispose() }
}
function Decode-B64Json($s) { try { [System.Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($s)) | ConvertFrom-Json } catch { $null } }

# ── native input ────────────────────────────────────────────────────────
function Move-To([int]$x, [int]$y) {
  $sw = [System.Windows.Forms.Screen]::PrimaryScreen.Bounds
  $nx = [int]($x * 65535 / ($sw.Width - 1)); $ny = [int]($y * 65535 / ($sw.Height - 1))
  [SlabNative]::Mouse(0x8001, $nx, $ny, 0)   # MOVE | ABSOLUTE
}
function Click-At($req) {
  $x = [int]$req.x; $y = [int]$req.y
  $button = if ($req.button) { [string]$req.button } else { 'left' }
  $count = if ($req.count) { [int]$req.count } else { 1 }
  $down = 0x0002; $up = 0x0004
  if ($button -eq 'right') { $down = 0x0008; $up = 0x0010 }
  Move-To $x $y; Start-Sleep -Milliseconds 20
  if ($req.drag) {
    [SlabNative]::Mouse($down, 0, 0, 0)
    $steps = [Math]::Max(2, [int]([double]($(if ($req.drag.durationMs) { $req.drag.durationMs } else { 160 })) / 8))
    for ($i = 1; $i -le $steps; $i++) { Move-To ([int]($x + ($req.drag.x - $x) * $i / $steps)) ([int]($y + ($req.drag.y - $y) * $i / $steps)); Start-Sleep -Milliseconds 8 }
    [SlabNative]::Mouse($up, 0, 0, 0)
    return [ordered]@{ sent = $true; kind = 'drag'; from = @($x, $y); to = @([int]$req.drag.x, [int]$req.drag.y) }
  }
  for ($i = 0; $i -lt $count; $i++) {
    [SlabNative]::Mouse($down, 0, 0, 0)
    if ($req.holdMs) { Start-Sleep -Milliseconds ([int]$req.holdMs) } else { Start-Sleep -Milliseconds 12 }
    [SlabNative]::Mouse($up, 0, 0, 0)
    if ($i -lt $count - 1) { Start-Sleep -Milliseconds 60 }
  }
  [ordered]@{ sent = $true; kind = 'click'; x = $x; y = $y; button = $button; count = $count }
}

# ── one observation ─────────────────────────────────────────────────────
$state = "$HOME\.local\share\slab\state"
New-Item -ItemType Directory -Force $state | Out-Null
$utf8 = New-Object System.Text.UTF8Encoding $false

function Trace($m) { try { Add-Content -Path "$state\agent.trace" -Value ("{0:HH:mm:ss.fff} {1}" -f (Get-Date), $m) } catch {} }
function Produce([string]$mode) {
  Trace "produce: $mode"
  $t0 = [System.Diagnostics.Stopwatch]::StartNew()
  $tm = [ordered]@{}
  $tokens = $mode -split ' ' | Where-Object { $_ }
  $flags = @{}; foreach ($tk in $tokens) { $flags[$tk] = $true }
  $session = ($tokens | Where-Object { $_ -like 'session=*' } | Select-Object -First 1); if ($session) { $session = $session.Substring(8) } else { $session = 'legacy' }
  $fr = [ordered]@{ capture = 'ok'; capture_scope = 'window'; coordinateSpace = 'windows-physical-pixels' }

  # Native input first, then observe what it did.
  $nativeToken = $tokens | Where-Object { $_ -like 'native-click=*' -or $_ -like 'native-drag=*' -or $_ -like 'native-guard=*' } | Select-Object -First 1
  if ($nativeToken) {
    $kind, $b64 = $nativeToken -split '=', 2
    $req = Decode-B64Json $b64
    if (-not $req) { $fr.nativeInput = @{ error = 'Invalid native request; no action sent' } }
    elseif ($kind -eq 'native-guard') { $fr.nativeInput = @{ guard = 'ok' } }
    else { $ti = [System.Diagnostics.Stopwatch]::StartNew(); $fr.nativeInput = Click-At $req; $fr.nativeInput.ms = $ti.ElapsedMilliseconds; Start-Sleep -Milliseconds 120 }
    $fr.nativeCapabilities = @('guarded-click-v1', 'guarded-drag-v1', 'click-hold-v1')
  }

  Trace "geometry"
  # Geometry.
  $screen = [System.Windows.Forms.Screen]::PrimaryScreen.Bounds
  $scale = [Math]::Round(([System.Drawing.Graphics]::FromHwnd([IntPtr]::Zero)).DpiX / 96, 2)
  $fg = [SlabNative]::GetForegroundWindow()
  $fgPid = 0; [void][SlabNative]::GetWindowThreadProcessId($fg, [ref]$fgPid)
  $fgApp = try { (Get-Process -Id $fgPid -ErrorAction Stop).ProcessName } catch { '?' }
  $fgTitle = Window-Title $fg
  $wb = Window-Bounds $fg
  $region = $screen
  if ($flags['screen'] -or $fg -eq [IntPtr]::Zero -or ($wb.Right - $wb.Left) -le 0) { $fr.capture_scope = 'screen' }
  else { $region = New-Object System.Drawing.Rectangle ([Math]::Max(0, $wb.Left)), ([Math]::Max(0, $wb.Top)), ([Math]::Min($screen.Width, $wb.Right) - [Math]::Max(0, $wb.Left)), ([Math]::Min($screen.Height, $wb.Bottom) - [Math]::Max(0, $wb.Top)) }
  $cropTok = $tokens | Where-Object { $_ -like 'crop=*' } | Select-Object -First 1
  if ($cropTok) { $c = $cropTok.Substring(5) -split ','; if ($c.Count -eq 4) { $region = New-Object System.Drawing.Rectangle ([int]$c[0]), ([int]$c[1]), ([int]$c[2]), ([int]$c[3]); $fr.capture_scope = 'crop' } }
  if ($region.Width -le 0 -or $region.Height -le 0) { $region = $screen; $fr.capture_scope = 'screen' }
  if ($fr.capture_scope -ne 'screen') { $fr.crop = [ordered]@{ x = $region.X; y = $region.Y; w = $region.Width; h = $region.Height } }

  Trace "pixels"
  # Pixels.
  $t = [System.Diagnostics.Stopwatch]::StartNew()
  $bmp = New-Object System.Drawing.Bitmap $region.Width, $region.Height
  $g = [System.Drawing.Graphics]::FromImage($bmp)
  $g.CopyFromScreen($region.Location, [System.Drawing.Point]::Empty, $region.Size); $g.Dispose()
  $tm.capture = $t.ElapsedMilliseconds

  Trace "meta"
  # Meta.
  $t.Restart()
  $cur = New-Object SlabNative+POINT; [void][SlabNative]::GetCursorPos([ref]$cur)
  $fr.meta = [ordered]@{
    host = $env:COMPUTERNAME; os = 'windows'
    screen = [ordered]@{ w = $screen.Width; h = $screen.Height; scale = $scale }
    cursor = [ordered]@{ x = $cur.X; y = $cur.Y }
    frontmost = [ordered]@{ app = $fgApp; pid = [int]$fgPid; title = $fgTitle; bundle = $fgApp }
    windows = [object[]](Visible-Windows 24)
  }
  $tm.meta = $t.ElapsedMilliseconds

  Trace "ocr"
  # OCR.
  $t.Restart()
  if ($flags['noocr']) { $fr.ocr = @() } else { $fr.ocr = [object[]](Ocr-Bitmap $bmp $region.X $region.Y) }
  $tm.ocr = $t.ElapsedMilliseconds

  Trace "uia"
  # UIA.
  $t.Restart()
  $cap = if ($flags['fast']) { 80 } else { 300 }
  $els = if ($fg -ne [IntPtr]::Zero) { [object[]](Walk-Uia $fg $cap 600) } else { @() }
  $fr.ax = [ordered]@{ trusted = $true; app = $fgApp; scope = $(if ($fr.capture_scope -eq 'screen') { 'application' } else { 'window' }); elements = $els }
  $tm.ax = $t.ElapsedMilliseconds

  $fr.visual = @()
  $fr.observation = [ordered]@{ id = [guid]::NewGuid().ToString(); session = $session; windowId = [int64]$fg; kind = 'frame'; coordinateSpace = 'windows-physical-pixels' }

  Trace "thumb"
  # Thumb + envelope.
  $t.Restart()
  $jpg = "$state\frame.out.jpg"; $json = "$state\frame.out.json"
  Save-Jpeg $bmp $jpg 1568
  $bmp.Dispose()
  $fr.thumb_bytes = (Get-Item $jpg).Length
  $tm.thumb = $t.ElapsedMilliseconds
  $tm.wall = $t0.ElapsedMilliseconds
  $fr.timings_ms = $tm
  [IO.File]::WriteAllText($json, ($fr | ConvertTo-Json -Depth 8 -Compress), $utf8)
  Trace "done $($tm.wall)ms"
}

# ── request loop ────────────────────────────────────────────────────────
$req = "$state\frame.req"; $done = "$state\frame.done"
"slab-agent up in session $([System.Diagnostics.Process]::GetCurrentProcess().SessionId) · ocr=$([bool]$script:ocr) · $state"
while ($true) {
  if (Test-Path $req) {
    $mode = ''
    try { $mode = [IO.File]::ReadAllText($req) } catch {}
    Remove-Item $req -Force -ErrorAction SilentlyContinue
    try { Produce $mode } catch {
      [IO.File]::WriteAllText("$state\frame.out.json", (@{ capture = 'error'; reason = "$($_.Exception.Message)" } | ConvertTo-Json -Compress), $utf8)
      Remove-Item "$state\frame.out.jpg" -Force -ErrorAction SilentlyContinue
    }
    [IO.File]::WriteAllText($done, 'ok', $utf8)
  }
  Start-Sleep -Milliseconds 15
}
