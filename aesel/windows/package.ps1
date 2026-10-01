$ErrorActionPreference = 'Stop'
$root = Resolve-Path "$PSScriptRoot/../.."
$version = (Get-Content "$root/aesel/package.json" -Raw | ConvertFrom-Json).version
$revision = (git -C $root rev-parse HEAD).Trim()
$dist = "$PSScriptRoot/dist"
New-Item -ItemType Directory -Force $dist | Out-Null
New-Item -ItemType Directory -Force "$dist/deps" | Out-Null
$runtimeInstaller = "$dist/deps/WebView2Setup.exe"
Invoke-WebRequest 'https://go.microsoft.com/fwlink/p/?LinkId=2124703' -OutFile $runtimeInstaller
$signature = Get-AuthenticodeSignature $runtimeInstaller
if ($signature.Status -ne 'Valid' -or $signature.SignerCertificate.Subject -notmatch 'O=Microsoft Corporation') { throw 'WebView2 installer signature check failed' }
dotnet publish "$PSScriptRoot/Aesel.csproj" -c Release -r win-x64 --self-contained true -p:Version=$version -p:InformationalVersion="$version+$revision" -o "$dist/app"
if ($LASTEXITCODE) { throw 'Windows build failed' }
$process = Start-Process "$dist/app/Aesel.exe" -ArgumentList @('--smoke-test', "`"$dist/smoke`"") -PassThru
if (-not $process.WaitForExit(120000)) { $process.Kill(); throw 'Windows smoke timed out' }
if ($process.ExitCode -ne 0) { if (Test-Path "$dist/smoke/error.txt") { Get-Content "$dist/smoke/error.txt" }; throw 'Windows smoke failed' }
if (-not (Get-Content "$dist/smoke/result.json" | ConvertFrom-Json).passed) { throw 'Missing passing smoke result' }
$iscc = "${env:ProgramFiles(x86)}/Inno Setup 6/ISCC.exe"
if (-not (Test-Path $iscc)) { throw 'Install Inno Setup 6 to package the installer' }
& $iscc "/DAppVersion=$version" "$PSScriptRoot/setup.iss"
if ($LASTEXITCODE) { throw 'Installer build failed' }
$name = "aesel-$version-windows-x64-setup.exe"
# Exercise the actual per-user installer as well as the publish output.
$installed = "$dist/installed"
$setup = Start-Process "$dist/$name" -ArgumentList @('/VERYSILENT','/SUPPRESSMSGBOXES','/NORESTART',"/DIR=`"$installed`"") -Wait -PassThru
if ($setup.ExitCode -ne 0 -or -not (Test-Path "$installed/Aesel.exe")) { throw 'Installer smoke failed' }
$process = Start-Process "$installed/Aesel.exe" -ArgumentList @('--smoke-test', "`"$dist/installed-smoke`"") -PassThru
if (-not $process.WaitForExit(120000)) { $process.Kill(); throw 'Installed app smoke timed out' }
if ($process.ExitCode -ne 0) { Get-Content "$dist/installed-smoke/error.txt" -ErrorAction SilentlyContinue; throw 'Installed app smoke failed' }
$sha = (Get-FileHash "$dist/$name" -Algorithm SHA256).Hash.ToLowerInvariant()
@{version=$version; revision=$revision; file=$name; sha256=$sha; architecture='x64'; signed=$false; minimumWindows='10'; channel='beta'} | ConvertTo-Json | Set-Content "$dist/latest.json" -Encoding utf8NoBOM
"$sha  $name" | Set-Content "$dist/SHA256SUMS.txt" -Encoding utf8NoBOM
