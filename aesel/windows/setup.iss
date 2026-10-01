#ifndef AppVersion
  #error AppVersion is required
#endif
[Setup]
AppId={{E61F581B-68D8-4D5F-B024-DF0BCC4DB1B8}
AppName=Aesel
AppVersion={#AppVersion}
AppPublisher=Aesthetic Computer
AppPublisherURL=https://aesel.app/
DefaultDirName={localappdata}\Programs\Aesel
DefaultGroupName=Aesel
UninstallDisplayIcon={app}\Aesel.exe
SetupIconFile=assets\aesel.ico
PrivilegesRequired=lowest
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
MinVersion=10.0
OutputDir=dist
OutputBaseFilename=aesel-{#AppVersion}-windows-x64-setup
Compression=lzma2
SolidCompression=yes
WizardStyle=modern
CloseApplications=yes
SetupMutex=AestheticComputer.Aesel.Setup
AppMutex=Local\AestheticComputer.Aesel.Windows
[Files]
Source: "dist\app\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "dist\deps\WebView2Setup.exe"; DestDir: "{tmp}"; Flags: deleteafterinstall
[Icons]
Name: "{autoprograms}\Aesel"; Filename: "{app}\Aesel.exe"
[Run]
Filename: "{tmp}\WebView2Setup.exe"; Parameters: "/silent /install"; StatusMsg: "Checking Microsoft WebView2 Runtime…"; Flags: waituntilterminated
Filename: "{app}\Aesel.exe"; Description: "Open Aesel"; Flags: nowait postinstall skipifsilent
; Account and notebook data are deliberately retained on uninstall. Reinstalling
; or updating must not delete art. Remove LocalAppData/Aesthetic Computer/Aesel
; separately to erase local data.
