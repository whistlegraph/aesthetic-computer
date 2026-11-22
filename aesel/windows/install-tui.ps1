# Install the Node-based Aesel TUI in the current Windows user's profile.
[CmdletBinding()]
param([string]$InstallRoot = (Join-Path $env:LOCALAPPDATA 'Aesthetic Computer\AeselTUI'))
$ErrorActionPreference = 'Stop'
$node = (Get-Command node.exe -CommandType Application -ErrorAction Stop).Source
$tar = (Get-Command tar.exe -CommandType Application -ErrorAction Stop).Source
$major = (& $node --version).TrimStart('v').Split('.')[0]
if ($LASTEXITCODE -ne 0 -or [int]$major -lt 22) { throw 'Install Node.js 22 or newer, then run this installer again.' }

$release = Invoke-RestMethod 'https://prompt.ac/aesel.json'
if ($release.version -notmatch '^\d+\.\d+\.\d+([.-][a-zA-Z0-9.-]+)?$' -or $release.sha256 -notmatch '^[0-9a-f]{64}$') {
    throw 'Invalid Aesel release manifest.'
}
$tag = "$($release.version)-$($release.sha256.Substring(0,12))"
$releases = Join-Path $InstallRoot 'releases'
$bin = Join-Path $InstallRoot 'bin'
New-Item -ItemType Directory -Force -Path $releases,$bin | Out-Null
$versionDir = Join-Path $releases $tag
$stage = Join-Path $releases ([guid]::NewGuid().ToString())
New-Item -ItemType Directory -Path $stage | Out-Null
try {
    if (-not (Test-Path -LiteralPath $versionDir)) {
        $archive = Join-Path $stage 'release.tar.gz'
        Invoke-WebRequest -UseBasicParsing 'https://prompt.ac/aesel.tar.gz' -OutFile $archive
        if ((Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash -ne $release.sha256) { throw 'Release checksum mismatch.' }
        $app = Join-Path $stage 'app'
        New-Item -ItemType Directory -Path $app | Out-Null
        & $tar -xzf $archive -C $app
        if ($LASTEXITCODE -ne 0) { throw 'Could not unpack Aesel.' }
        $version = & $node (Join-Path $app 'src\terminal-cli.mjs') --version
        if ($LASTEXITCODE -ne 0 -or $version -ne "aesel $($release.version)") { throw 'The unpacked TUI failed its version check.' }
        try { [IO.Directory]::Move($app,$versionDir) }
        catch { if (-not [IO.Directory]::Exists($versionDir)) { throw } }
    }
    $cmd = Join-Path $bin 'a.cmd'
    $temp = Join-Path $stage 'a.cmd'
    $launcher = "@echo off`r`nsetlocal`r`nif not defined BROWSER set `"BROWSER=explorer.exe`"`r`nnode.exe `"%~dp0..\releases\$tag\src\terminal-cli.mjs`" %*`r`n"
    [IO.File]::WriteAllText($temp,$launcher,[Text.Encoding]::ASCII)
    if ([IO.File]::Exists($cmd)) { [IO.File]::Replace($temp,$cmd,(Join-Path $stage 'previous.cmd')) }
    else { [IO.File]::Move($temp,$cmd) }
    $userPath = [Environment]::GetEnvironmentVariable('Path','User')
    $newPath = (@($bin) + @($userPath -split ';' | Where-Object { $_ -and $_ -ne $bin })) -join ';'
    if ($userPath -ne $newPath) { [Environment]::SetEnvironmentVariable('Path',$newPath,'User') }
    $env:Path = (@($bin) + @($env:Path -split ';' | Where-Object { $_ -and $_ -ne $bin })) -join ';'
    Write-Output "Aesel TUI $($release.version) is installed. Type a to open it."
} finally {
    if (Test-Path -LiteralPath $stage) { Remove-Item -LiteralPath $stage -Recurse -Force }
}
