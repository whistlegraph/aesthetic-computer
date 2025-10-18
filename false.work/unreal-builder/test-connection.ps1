# Test Perforce Connection for false.work Studio
# Run this script to verify Perforce is configured correctly before setting up builds

Write-Host "=== Testing Perforce Connection for Spider Lily ===" -ForegroundColor Cyan
Write-Host ""

# Set credentials
$env:P4PORT = "ssl:falsework.helixcore.io:1666"
$env:P4USER = "machine"
$env:P4CLIENT = "spiderlily_build_workspace"

# Note: P4PASSWD should be set separately for security
# Run: p4 set P4PASSWD=AestheticComp1

Write-Host "Testing with:" -ForegroundColor Yellow
Write-Host "  Server: $env:P4PORT"
Write-Host "  User: $env:P4USER"
Write-Host "  Workspace: $env:P4CLIENT"
Write-Host ""

# Test 1: Can we reach the server?
Write-Host "[1/5] Testing server connection..." -ForegroundColor Yellow
try {
    $info = p4 info 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "  ✓ Server is reachable" -ForegroundColor Green
        
        # Show server info
        $info | Select-String "Server address:" | ForEach-Object {
            Write-Host "  $_" -ForegroundColor Gray
        }
        $info | Select-String "Server version:" | ForEach-Object {
            Write-Host "  $_" -ForegroundColor Gray
        }
    } else {
        Write-Host "  ✗ Cannot reach server" -ForegroundColor Red
        Write-Host "  Error: $info" -ForegroundColor Red
        exit 1
    }
} catch {
    Write-Host "  ✗ Connection failed: $_" -ForegroundColor Red
    exit 1
}

Write-Host ""

# Test 2: Are we logged in?
Write-Host "[2/5] Checking authentication..." -ForegroundColor Yellow
$loginStatus = p4 login -s 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Host "  ✓ Authenticated successfully" -ForegroundColor Green
    Write-Host "  $loginStatus" -ForegroundColor Gray
} else {
    Write-Host "  ✗ Not logged in" -ForegroundColor Red
    Write-Host "  Please run: p4 set P4PASSWD=AestheticComp1" -ForegroundColor Yellow
    Write-Host "  Then run: p4 login" -ForegroundColor Yellow
    exit 1
}

Write-Host ""

# Test 3: Does the workspace exist?
Write-Host "[3/5] Checking workspace..." -ForegroundColor Yellow
$workspace = p4 client -o $env:P4CLIENT 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Host "  ✓ Workspace exists" -ForegroundColor Green
    
    # Show workspace details
    $workspace | Select-String "^Root:" | ForEach-Object {
        Write-Host "  $_" -ForegroundColor Gray
    }
    $workspace | Select-String "^View:" -A 5 | ForEach-Object {
        Write-Host "  $_" -ForegroundColor Gray
    }
} else {
    Write-Host "  ✗ Workspace not found" -ForegroundColor Red
    Write-Host "  Create it with: p4 client $env:P4CLIENT" -ForegroundColor Yellow
    Write-Host "  Or run setup-build-machine.ps1" -ForegroundColor Yellow
}

Write-Host ""

# Test 4: Can we access the depot?
Write-Host "[4/5] Checking depot access..." -ForegroundColor Yellow
$depotPath = "//depot/SpiderLily/SL_main/..."
$depotTest = p4 files -m 1 $depotPath 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Host "  ✓ Can access depot: $depotPath" -ForegroundColor Green
    Write-Host "  Sample file: $depotTest" -ForegroundColor Gray
} else {
    Write-Host "  ✗ Cannot access depot: $depotPath" -ForegroundColor Red
    Write-Host "  Error: $depotTest" -ForegroundColor Red
    Write-Host "  Verify depot path is correct" -ForegroundColor Yellow
}

Write-Host ""

# Test 5: Check latest changes
Write-Host "[5/5] Checking recent changes..." -ForegroundColor Yellow
$changes = p4 changes -m 5 $depotPath 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Host "  ✓ Latest changes:" -ForegroundColor Green
    $changes | ForEach-Object {
        Write-Host "  $_" -ForegroundColor Gray
    }
} else {
    Write-Host "  ✗ Cannot retrieve changes" -ForegroundColor Red
    Write-Host "  Error: $changes" -ForegroundColor Red
}

Write-Host ""
Write-Host "=== Test Complete ===" -ForegroundColor Cyan
Write-Host ""
Write-Host "If all tests passed, you're ready to set up the build machine!" -ForegroundColor Green
Write-Host "Next step: Run setup-build-machine.ps1" -ForegroundColor Yellow
