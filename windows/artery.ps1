<#
.SYNOPSIS
    Artery CDP automation tool for Windows

.DESCRIPTION
    PowerShell wrapper for artery.mjs - control Aesthetic Computer via Chrome DevTools Protocol

.PARAMETER Command
    Command to run: repl, panel, toggle-local, jump, type, eval, perf

.PARAMETER Args
    Additional arguments for the command

.EXAMPLE
    .\artery.ps1 panel
    .\artery.ps1 toggle-local
    .\artery.ps1 jump notepat
    .\artery.ps1 repl
    .\artery.ps1 perf 10        # Monitor WebGPU performance for 10 seconds

.NOTES
    Requires Node.js and VS Code with --remote-debugging-port=9222
#>

param(
    [Parameter(Mandatory=$true, Position=0)]
    [string]$Command,
    
    [Parameter(ValueFromRemainingArguments=$true)]
    [string[]]$Args
)

# Navigate to repo root
$repoRoot = "\\wsl.localhost\Ubuntu\home\me\aesthetic-computer"
if (Test-Path $repoRoot) {
    Set-Location $repoRoot
} else {
    Write-Host "Repository not found at $repoRoot" -ForegroundColor Red
    exit 1
}

Write-Host "🩸 Artery: $Command" -ForegroundColor Magenta

# Run artery with command and args
node .vscode\artery.mjs $Command @Args
