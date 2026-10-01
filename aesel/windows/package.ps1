$ErrorActionPreference = 'Stop'
$root = Resolve-Path "$PSScriptRoot/../.."
$version = (Get-Content "$root/aesel/package.json" -Raw | ConvertFrom-Json).version
$revision = (git -C $root rev-parse HEAD).Trim()
$dist = "$PSScriptRoot/dist"
New-Item -ItemType Directory -Force $dist | Out-Null
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
$sha = (Get-FileHash "$dist/$name" -Algorithm SHA256).Hash.ToLowerInvariant()
@{version=$version; revision=$revision; file=$name; sha256=$sha; architecture='x64'; signed=$false; minimumWindows='10'; channel='beta'} | ConvertTo-Json | Set-Content "$dist/latest.json" -Encoding utf8NoBOM
"$sha  $name" | Set-Content "$dist/SHA256SUMS.txt" -Encoding utf8NoBOM
