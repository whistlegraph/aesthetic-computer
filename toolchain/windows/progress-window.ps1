# Click-through build glass inside the work area, above the Menu Band. Only panels carry transparency;
# type stays opaque. Animation touches a small progress line, not the desktop.
param([string]$LogPath = (Join-Path $HOME 'ac-progress.log'))
$ErrorActionPreference = 'Stop'
trap { Add-Content (Join-Path $HOME 'ac-progress-window-errors.log') ($_ | Out-String); exit 1 }
Add-Type -AssemblyName PresentationFramework, PresentationCore, WindowsBase, System.Windows.Forms
Add-Type @'
using System; using System.Runtime.InteropServices;
public static class ProgressGlass {
  [DllImport("user32.dll")] static extern int GetWindowLong(IntPtr h, int i);
  [DllImport("user32.dll")] static extern int SetWindowLong(IntPtr h, int i, int v);
  public static void Apply(IntPtr h) {
    SetWindowLong(h, -20, GetWindowLong(h, -20) | 0x20 | 0x80000 | 0x80 | 0x08000000);
  }
}
'@
if (-not (Test-Path $LogPath)) { Set-Content $LogPath ((Get-Date -Format HH:mm:ss) + '  ready') }
[xml]$xaml = @'
<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="AC build progress" WindowStyle="None" ResizeMode="NoResize"
        AllowsTransparency="True" Background="Transparent" ShowInTaskbar="False"
        ShowActivated="False" Topmost="True" WindowStartupLocation="Manual">
  <Grid x:Name="Root" Background="#10071521" Margin="0">
    <Grid x:Name="Content" Margin="18,18,18,18">
      <Grid.RowDefinitions>
        <RowDefinition Height="Auto"/>
        <RowDefinition Height="26"/>
        <RowDefinition Height="*"/>
        <RowDefinition Height="Auto"/>
      </Grid.RowDefinitions>
      <Border x:Name="HeadlinePanel" Background="#42081120" CornerRadius="22" Padding="28,22,28,24">
        <TextBlock x:Name="Headline" FontFamily="Segoe UI Semibold" FontSize="58"
                   Foreground="#FFF5F3FF" TextWrapping="Wrap" TextTrimming="None"/>
      </Border>
      <Grid Grid.Row="1" Margin="28,11,28,11" ClipToBounds="True">
        <Border Background="#3373CFCF" CornerRadius="2"/>
        <Border x:Name="Motion" Width="150" HorizontalAlignment="Left" CornerRadius="2">
          <Border.Background>
            <LinearGradientBrush StartPoint="0,0" EndPoint="1,0">
              <GradientStop Color="#007DE9DF" Offset="0"/>
              <GradientStop Color="#FF7DE9DF" Offset="0.45"/>
              <GradientStop Color="#FFCBACFF" Offset="0.75"/>
              <GradientStop Color="#00CBACFF" Offset="1"/>
            </LinearGradientBrush>
          </Border.Background>
          <Border.RenderTransform><TranslateTransform x:Name="MotionPosition"/></Border.RenderTransform>
        </Border>
      </Grid>
      <Border Grid.Row="2" Background="#57081120" CornerRadius="22" Padding="28,22">
        <ScrollViewer x:Name="LogScroll" VerticalScrollBarVisibility="Hidden" HorizontalScrollBarVisibility="Disabled" Focusable="False">
          <StackPanel x:Name="LogLines"/>
        </ScrollViewer>
      </Border>
      <TextBlock x:Name="Freshness" Grid.Row="3" FontFamily="Consolas" FontSize="13"
                 Foreground="#D6D5DBEA" Margin="28,12,0,0"/>
    </Grid>
  </Grid>
</Window>
'@
$window = [Windows.Markup.XamlReader]::Load((New-Object Xml.XmlNodeReader $xaml))
$headline = $window.FindName('Headline'); $linesPanel = $window.FindName('LogLines')
$scroll = $window.FindName('LogScroll'); $freshness = $window.FindName('Freshness')
$fontFile = Join-Path $PSScriptRoot 'progress-fonts\ywft-processing-regular.ttf'
if (Test-Path $fontFile) {
  $fontFolder = [Uri]::new((Split-Path $fontFile) + '\')
  $headline.FontFamily = [Windows.Media.FontFamily]::new($fontFolder, './#YWFT Processing')
}
$motion = $window.FindName('Motion'); $motionPosition = $window.FindName('MotionPosition')
$palette = @{}
foreach ($entry in @{
  plain='#FFE0E6F1'; muted='#FFA4B4CA'; error='#FFFF8C9B'; warning='#FFFFD395';
  success='#FF87EFCD'; path='#FF8CCDF7'; number='#FFD5B2FF'; keyword='#FFAFA9FF'
}.GetEnumerator()) {
  $palette[$entry.Key] = [Windows.Media.BrushConverter]::new().ConvertFromString($entry.Value)
  $palette[$entry.Key].Freeze()
}
$script:lastStamp = ''; $script:changedAt = (Get-Item $LogPath).LastWriteTime
$machineName = (Get-ItemProperty 'HKLM:\SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName').ComputerName.ToLowerInvariant()
$script:step = 'ready'; $script:failed = $false; $script:complete = $false
$script:fontSize = 18; $script:visibleLines = 18; $script:animating = $false
$speaker = $null
try {
  Add-Type -AssemblyName System.Speech
  $speaker = [System.Speech.Synthesis.SpeechSynthesizer]::new()
  $speaker.Rate = 0; $speaker.Volume = 65
} catch {}
$spokenPath = Join-Path $HOME 'ac-progress-spoken.txt'
$script:lastSpoken = if (Test-Path $spokenPath) { Get-Content $spokenPath -Raw } else { '' }
function Speak-Step([string]$step) {
  if (-not $speaker -or $step.Trim() -eq $script:lastSpoken.Trim()) { return }
  if (((Get-Date) - $script:changedAt).TotalSeconds -gt 15) { return }
  $message = switch -Regex ($step) {
    'BUILD FAILED' { 'The build failed.'; break }
    'INSTALL FAILED|LAUNCH FAILED' { 'The Xbox update failed.'; break }
    'installing on the xbox' { 'Sending the new build to Xbox.'; break }
    'running on the xbox' { 'Oskiewar is running on Xbox.'; break }
    'building native-bios' { 'Building Oskiewar.'; break }
    'built in' { 'Build complete.'; break }
  }
  if ($message) {
    $script:lastSpoken = $step; Set-Content $spokenPath $step -NoNewline
    $speaker.SpeakAsyncCancelAll(); [void]$speaker.SpeakAsync($message)
  }
}
# Quotes and file paths form whole tokens; a long path wraps instead of vanishing.
$tokenPattern = '(?<error>\b(?:error|failed|failure|fatal|rejected)\b)|(?<warning>\b(?:warning|warn)\b)|(?<success>\b(?:succeeded|success|passed|built|verified|running|done)\b)|(?<path>[A-Za-z]:\\[^\r\n\[\]<>]*?\.(?:cpp|hpp|mjs|js|vcxproj|sln|msix|appx|wav)|\b[\w./\\-]+\.(?:cpp|hpp|mjs|js|vcxproj|sln|msix|appx|wav)\b)|(?<quote>"[^"]*"|''[^'']*'')|(?<number>\b\d+(?:\.\d+)*(?:ms|s|%|Hz|fps|KB|MB)?\b)|(?<keyword>\b(?:xbox|msbuild|compiling|linking|syncing|fetching|installing|launching)\b)'
function Add-LogLine([string]$text) {
  $block = New-Object Windows.Controls.TextBlock
  $block.FontFamily = [Windows.Media.FontFamily]::new('Cascadia Code, Consolas')
  $block.FontSize = $script:fontSize; $block.LineHeight = $script:fontSize * 1.5
  $block.TextWrapping = 'Wrap'; $block.TextTrimming = 'None'; $block.Foreground = $palette.plain
  $block.Margin = [Windows.Thickness]::new(0,0,0,5)
  $text = $text.TrimStart()
  $offset = 0
  foreach ($match in [regex]::Matches($text, $tokenPattern, 'IgnoreCase')) {
    if ($match.Index -gt $offset) { [void]$block.Inlines.Add([Windows.Documents.Run]::new($text.Substring($offset,$match.Index-$offset))) }
    $run = [Windows.Documents.Run]::new($match.Value)
    $kind = 'muted'
    foreach ($key in @('error','warning','success','path','number','keyword')) {
      if ($match.Groups[$key].Success) { $kind = $key; break }
    }
    if ($match.Groups['quote'].Success) { $kind = 'success' }
    $run.Foreground = $palette[$kind]; [void]$block.Inlines.Add($run)
    $offset = $match.Index + $match.Length
  }
  if ($offset -lt $text.Length) { [void]$block.Inlines.Add([Windows.Documents.Run]::new($text.Substring($offset))) }
  [void]$linesPanel.Children.Add($block)
}
function Fit-Headline {
  $size = [math]::Min(64, [math]::Max(32, $window.Width / 23))
  $available = [math]::Max(200, $window.Width - 148)
  do {
    $headline.FontSize = $size
    $headline.Measure([Windows.Size]::new($available, [double]::PositiveInfinity))
    if ($headline.DesiredSize.Height -le $window.Height * .34 -or $size -le 18) { break }
    $size -= 2
  } while ($true)
}
function Fit-Screen {
  $bounds = [Windows.Forms.Screen]::PrimaryScreen.WorkingArea
  $source = [Windows.PresentationSource]::FromVisual($window)
  $scale = $source.CompositionTarget.TransformFromDevice
  $origin = $scale.Transform([Windows.Point]::new($bounds.Left, $bounds.Top))
  $extent = $scale.Transform([Windows.Point]::new($bounds.Width, $bounds.Height))
  # The whole display: the laptop is the build machine's face, so the step
  # reads from across the room.
  $window.Left = $origin.X; $window.Top = $origin.Y
  $window.Width = $extent.X; $window.Height = $extent.Y
  $script:fontSize = [math]::Max(14, [math]::Min(21, $window.Width / 75))
  $script:visibleLines = [math]::Max(8, [int]($window.Height / ($script:fontSize * 1.5)))
  Fit-Headline
}
function Set-Motion([bool]$active) {
  if ($active -eq $script:animating) { return }
  $script:animating = $active
  if ($active) {
    $travel = [Windows.Media.Animation.DoubleAnimation]::new(-150, [math]::Max(200, $window.Width - 148), [Windows.Duration]::new([TimeSpan]::FromSeconds(3.6)))
    $travel.RepeatBehavior = [Windows.Media.Animation.RepeatBehavior]::Forever
    [Windows.Media.Animation.Timeline]::SetDesiredFrameRate($travel, 24)
    $motionPosition.BeginAnimation([Windows.Media.TranslateTransform]::XProperty, $travel)
    $motion.Opacity = .95
  } else {
    $motionPosition.BeginAnimation([Windows.Media.TranslateTransform]::XProperty, $null)
    $motionPosition.X = 0; $motion.Opacity = .35
  }
}
function Refresh-Log {
  $file = Get-Item $LogPath
  $stamp = "$($file.Length):$($file.LastWriteTimeUtc.Ticks)"
  if ($stamp -ne $script:lastStamp) {
    # Do not lock out MSBuild's writer while reading its tail.
    $stream = [IO.File]::Open($LogPath, 'Open', 'Read', [IO.FileShare]::ReadWrite)
    try {
      if ($stream.Length -gt 131072) { [void]$stream.Seek(-131072, 'End') }
      $reader = [IO.StreamReader]::new($stream)
      $lines = @($reader.ReadToEnd() -split '\r?\n' | Select-Object -Last 90)
    } finally { if ($reader) { $reader.Dispose() }; $stream.Dispose() }
    $script:lastStamp = $stamp; $script:changedAt = $file.LastWriteTime
    $step = $lines | Where-Object { $_ -match '^\d\d:\d\d:\d\d  ' } | Select-Object -Last 1
    if ($step) { $script:step = $step.Substring(10).Trim() }
    $script:failed = $script:step -match 'FAIL|error|rejected'
    $script:complete = $script:step -match 'running on|done in|verified|success|ready'
    if ($step) { Speak-Step $step }
    if ($headline.Text -ne $script:step) {
      $headline.Text = $script:step; Fit-Headline
      $fade = [Windows.Media.Animation.DoubleAnimation]::new(.45, 1, [Windows.Duration]::new([TimeSpan]::FromMilliseconds(450)))
      $headline.BeginAnimation([Windows.UIElement]::OpacityProperty, $fade)
    }
    $headline.Foreground = if ($script:failed) { $palette.error } elseif ($script:complete) { $palette.success } else { [Windows.Media.Brushes]::White }
    $linesPanel.Children.Clear()
    # The headline has one home. Show the latest evidence beneath it.
    $body = @($lines | Where-Object { $_.Trim() -and $_ -ne $step } | Select-Object -Last $script:visibleLines)
    foreach ($line in $body) { Add-LogLine $line }
    $scroll.UpdateLayout(); $scroll.ScrollToEnd()
  }
  $age = [math]::Max(0, ((Get-Date) - $script:changedAt).TotalSeconds)
  $state = if ($script:failed) { 'failed' } elseif ($script:complete) { 'complete' } elseif ($age -ge 600) { 'idle' } else { 'working' }
  $elapsed = if ($age -lt 60) { '{0}s' -f [int]$age } elseif ($age -lt 3600) { '{0}m' -f [int]($age/60) } else { '{0:N1}h' -f ($age/3600) }
  $freshness.Text = "$machineName   /   $state   /   updated $elapsed ago"
  Set-Motion ($state -eq 'working')
}
$window.Add_SourceInitialized({
  $handle = [Windows.Interop.WindowInteropHelper]::new($window).Handle
  [ProgressGlass]::Apply($handle)
  Fit-Screen
})
$window.Add_Loaded({ Refresh-Log })
$timer = [Windows.Threading.DispatcherTimer]::new(); $timer.Interval = [TimeSpan]::FromMilliseconds(500)
$timer.Add_Tick({
  try { Refresh-Log } catch { Add-Content (Join-Path $HOME 'ac-progress-window-errors.log') $_.Exception.Message }
})
$window.Add_Closed({ $timer.Stop(); if ($speaker) { $speaker.Dispose() } })
$timer.Start()
[void]$window.ShowDialog()
