# Disable LiveCoding in Spider Lily Project
# Run this on the VM before packaging builds

Write-Host "Disabling LiveCoding in Spider Lily project..." -ForegroundColor Cyan

$ProjectRoot = "C:\Perforce"

# Find all Target.cs files
$TargetFiles = Get-ChildItem -Path "$ProjectRoot\Source" -Filter "*Target.cs" -Recurse

if ($TargetFiles.Count -eq 0) {
    Write-Host "ERROR: No Target.cs files found in $ProjectRoot\Source" -ForegroundColor Red
    exit 1
}

Write-Host "Found $($TargetFiles.Count) Target.cs file(s)" -ForegroundColor Yellow

foreach ($file in $TargetFiles) {
    Write-Host "Processing: $($file.FullName)" -ForegroundColor White
    
    $content = Get-Content $file.FullName -Raw
    
    # Check if LiveCoding is already disabled
    if ($content -match "bBuildWithLiveCodingConsole\s*=\s*false" -or 
        $content -match "bUseLiveCodingConsole\s*=\s*false") {
        Write-Host "  LiveCoding already disabled in this file" -ForegroundColor Green
        continue
    }
    
    # Add LiveCoding disable to the constructor
    # Look for the constructor pattern
    if ($content -match "public\s+\w+Target\s*\(\s*TargetInfo\s+Target\s*\)\s*:\s*base\s*\(\s*Target\s*\)\s*\{") {
        # Add after the opening brace of the constructor
        $newContent = $content -replace "(public\s+\w+Target\s*\(\s*TargetInfo\s+Target\s*\)\s*:\s*base\s*\(\s*Target\s*\)\s*\{)", 
            "`$1`n`t`t// ADDED: Disable LiveCoding for automated builds`n`t`tbBuildWithLiveCodingConsole = false;`n"
        
        # Make file writable
        attrib -r $file.FullName
        
        # Write the modified content
        Set-Content -Path $file.FullName -Value $newContent -NoNewline
        
        Write-Host "  LiveCoding disabled!" -ForegroundColor Green
    } else {
        Write-Host "  WARNING: Could not find Target constructor pattern" -ForegroundColor Yellow
        Write-Host "  Manual edit may be required" -ForegroundColor Yellow
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Next Steps:" -ForegroundColor Cyan
Write-Host "1. Review changes in $ProjectRoot\Source" -ForegroundColor White
Write-Host "2. Try building again with package-for-team.ps1" -ForegroundColor White
Write-Host "========================================" -ForegroundColor Cyan
