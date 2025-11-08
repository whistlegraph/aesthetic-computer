# Update the build script on Windows to use lower memory requirements
$buildScript = @'
# Add after line with -IgnoreCookErrors:
Find: -IgnoreCookErrors
Replace with: -IgnoreCookErrors `
    -CookIniSettings="CookSettings.MemoryMinFreePhysical=1024" `
    -CookIniSettings="CookSettings.MemoryMinFreeVirtual=1024"
'@

Write-Host "To update the build script on Windows, add these flags after -IgnoreCookErrors:"
Write-Host ""
Write-Host '    -CookIniSettings="CookSettings.MemoryMinFreePhysical=1024" `' -ForegroundColor Yellow
Write-Host '    -CookIniSettings="CookSettings.MemoryMinFreeVirtual=1024"' -ForegroundColor Yellow
Write-Host ""
Write-Host "This reduces the memory requirement from 2048MB to 1024MB"
