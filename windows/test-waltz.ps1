<#
.SYNOPSIS
    Run generative waltz tests on Windows

.DESCRIPTION
    Wrapper for test-generative-waltz.mjs that works on Windows PowerShell

.PARAMETER Bars
    Number of bars (default: 8)

.PARAMETER Scale
    Musical scale: major, minor, dorian (default: major)

.PARAMETER Seed
    Random seed for reproducibility

.PARAMETER Tempo
    Tempo: viennese, medium, slow (default: slow)

.PARAMETER TopLine
    Add ornamental top line

.PARAMETER Infinite
    Continuously generate waltzes

.EXAMPLE
    .\test-waltz.ps1 -Bars 12 -Scale minor -Tempo viennese -TopLine

.NOTES
    Requires Node.js and VS Code with --remote-debugging-port=9222
#>

param(
    [int]$Bars = 8,
    [string]$Scale = "major",
    [int]$Seed = (Get-Random),
    [string]$Tempo = "slow",
    [switch]$TopLine,
    [switch]$Infinite
)

# Navigate to repo root
$repoRoot = "\\wsl.localhost\Ubuntu\home\me\aesthetic-computer"
if (Test-Path $repoRoot) {
    Set-Location $repoRoot
} else {
    Write-Host "Repository not found at $repoRoot" -ForegroundColor Red
    exit 1
}

# Build arguments
$args = @($Bars, $Scale, $Seed, $Tempo)
if ($TopLine) { $args += "topline" }
if ($Infinite) { $args += "infinite" }

Write-Host "🎵 Running waltz: $Bars bars, $Scale, seed $Seed, tempo $Tempo" -ForegroundColor Cyan

# Run the test
node .vscode\tests\test-generative-waltz.mjs @args
