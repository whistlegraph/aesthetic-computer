# Windows 11 24H2+: report, apply, or restore keyboard-friendly touchpad input.
# Changes sensitivity only; the remaining settings are read and preserved.
param([switch]$Apply, [switch]$Restore)
$ErrorActionPreference = 'Stop'
if ($Apply -and $Restore) { throw 'Choose -Apply or -Restore.' }

Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class SlabTouchpad {
    [StructLayout(LayoutKind.Sequential)]
    public struct Parameters {
        public uint version, maxContacts, legacyFeatures, status, options;
        public uint sensitivity, cursorSpeed, feedbackIntensity, clickForce;
        public uint rightClickWidth, rightClickHeight;
    }
    [DllImport("user32.dll", SetLastError=true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    public static extern bool SystemParametersInfoW(uint action, uint size,
                                                   ref Parameters value, uint flags);
    public static Parameters Read() {
        var p = new Parameters { version = 1 };
        if (!SystemParametersInfoW(0xAE, (uint)Marshal.SizeOf(p), ref p, 0))
            throw new System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error());
        return p;
    }
    public static void Write(Parameters p) {
        if (!SystemParametersInfoW(0xAF, (uint)Marshal.SizeOf(p), ref p, 3))
            throw new System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error());
    }
}
'@

$current = [SlabTouchpad]::Read()
if (($current.status -band 1) -eq 0) { throw 'No Precision Touchpad detected.' }
$backup = Join-Path $env:LOCALAPPDATA 'Slab\touchpad-sensitivity.json'
$before = $current.sensitivity
if ($Apply) {
    if ($before -ne 0) {
        if (-not (Test-Path $backup)) {
            New-Item -ItemType Directory -Force (Split-Path $backup) | Out-Null
            @{ sensitivity = $before; savedAt = [DateTime]::UtcNow.ToString('o') } |
                ConvertTo-Json | Set-Content -Encoding UTF8 $backup
        }
        $current.sensitivity = 0
        [SlabTouchpad]::Write($current)
    }
} elseif ($Restore) {
    if (-not (Test-Path $backup)) { throw 'No saved sensitivity to restore.' }
    $saved = Get-Content -Raw $backup | ConvertFrom-Json
    if ($saved.sensitivity -notin 0,1,2,3,4) { throw 'Invalid saved sensitivity.' }
    $current.sensitivity = [uint32]$saved.sensitivity
    [SlabTouchpad]::Write($current)
}
$after = [SlabTouchpad]::Read()
if (($Apply -and $after.sensitivity -ne 0) -or
    ($Restore -and $after.sensitivity -ne $current.sensitivity)) {
    throw 'Windows did not accept the sensitivity change.'
}
@{ before = $before; after = $after.sensitivity; contacts = $after.maxContacts;
   backup = $(if (Test-Path $backup) { $backup } else { $null }) } | ConvertTo-Json
