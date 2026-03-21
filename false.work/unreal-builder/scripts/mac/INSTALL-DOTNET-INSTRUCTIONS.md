# Installing .NET 8 SDK on Mac

## Quick Install

Open a terminal on your Mac and run these commands:

```bash
# Download .NET 8 SDK installer
cd ~/Downloads
curl -L -o dotnet-sdk-8.0.111-osx-arm64.pkg "https://download.visualstudio.microsoft.com/download/pr/c29f1f61-e6b9-4f93-a5e1-e046bbf0e110/d9a23e0ca0ce715a8e8d52fe7e07dbdf/dotnet-sdk-8.0.111-osx-arm64.pkg"

# Install (will prompt for password)
sudo installer -pkg ~/Downloads/dotnet-sdk-8.0.111-osx-arm64.pkg -target /

# Verify
dotnet --version
```

Expected output: `8.0.111` or similar

## Alternative: Direct Download

Visit: https://dotnet.microsoft.com/download/dotnet/8.0

Download: **.NET 8.0 SDK (v8.0.111) - macOS Arm64 Installer**

Double-click the downloaded `.pkg` file and follow the installer.

## After Installation

Test that UnrealBuildTool can now run:
```bash
"/Users/Shared/Epic Games/UE_5.6/Engine/Binaries/DotNET/UnrealBuildTool/UnrealBuildTool" -help
```

## From Dev Container

Once .NET is installed on the Mac, run:
```bash
./false.work/unreal-builder/scripts/mac/ssh-mac.fish "dotnet --version"
```

Then try the build again:
```bash
./false.work/unreal-builder/scripts/mac/ssh-mac.fish 'bash -s' < ./false.work/unreal-builder/scripts/mac/build-spiderlily-editor.sh
```
