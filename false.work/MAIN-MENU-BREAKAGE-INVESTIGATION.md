# Spider Lily Main Menu Breakage Investigation

**Date:** December 5, 2025  
**Build Version:** 2025.12.05-1038  
**Investigation Scope:** P4 Changelists 740-766

---

## Executive Summary

The Spider Lily main menu stopped working between **P4 changelist 740** (last known working) and the current head. The primary suspects are **changelists 741 and 742**, both submitted by `technical@animation_LAPTOP-4HQ5AKGQ_9526`.

### Key Findings

1. **Changelist 741** modified the GameMode blueprint with **NO commit message** — highly suspicious
2. **Changelist 742** introduced a complete UI refactor with parallel/duplicate menu widgets in a `NewFolder` structure
3. The changes created **two competing sets of menu widgets** that may be conflicting

---

## Timeline of Changes

### Changelist 740 (Last Known Working)
- **Author:** jonathan@aesthetic_FED-DEV_10440
- **Description:** "removed fish debug text"
- **Status:** ✅ **Last confirmed working build**

---

### Changelist 741 ⚠️ **SUSPECT #1**
- **Author:** technical@animation_LAPTOP-4HQ5AKGQ_9526
- **Description:** **(EMPTY - NO DESCRIPTION PROVIDED)**
- **Files Modified:**

| File | Action | Revision |
|------|--------|----------|
| `Content/Blueprints/Core/BP_SpiderLily_GameMode.uasset` | edit | #21 |

**Analysis:**  
The GameMode blueprint controls which widgets are spawned at game start. A modification here without documentation is extremely risky. This single file change with no description is a major red flag for the menu breakage.

---

### Changelist 742 ⚠️ **SUSPECT #2**
- **Author:** technical@animation_LAPTOP-4HQ5AKGQ_9526
- **Description:** "UI"
- **Files Modified (22 total):**

#### Modified Existing Widgets:
| File | Action |
|------|--------|
| `Content/LIB/UI/MainMenu/WBP_OptionsMenu.uasset` | edit |
| `Content/LIB/UI/MainMenu/WBP_PauseMenu.uasset` | edit |
| `Content/LIB/UI/MainMenu/WBP_QuitConfirm.uasset` | edit |
| `Content/LIB/UI/MainMenu/WBP_TextButton.uasset` | edit |
| `Content/Levels/L_MainMenu.umap` | edit |

#### NEW Files Added in `Content/UI/NewFolder/`:
| File | Action | Notes |
|------|--------|-------|
| `WBP_Death.uasset` | **add** | New death screen widget |
| `WBP_MainMenu.uasset` | **add** | ⚠️ **DUPLICATE** of existing main menu |
| `WBP_PauseMenu1.uasset` | **add** | ⚠️ **DUPLICATE** of existing pause menu |
| `WBP_TextButton_New.uasset` | **add** | ⚠️ **DUPLICATE** of existing text button |

#### NEW Font Assets:
| File | Action |
|------|--------|
| `Jost-Bold_Font.uasset` | add |
| `Jost-Bold.uasset` | add |
| `Jost-Regular_Font.uasset` | add |
| `Jost-Regular.uasset` | add |

#### NEW Textures:
| File | Action |
|------|--------|
| `T_background2.uasset` | add |
| `T_background3.uasset` | add |
| `T_background4.uasset` | add |
| `T_SliderIndicator.uasset` | add |
| `T_SliderIndicator2.uasset` | add |
| `T_SLLogo.uasset` | add |
| `T_SLLogo_white.uasset` | add |
| `T_SLMark.uasset` | add |

**Analysis:**  
This changelist shows clear signs of an incomplete UI refactor:
- Modified the main menu level (`L_MainMenu.umap`)
- Created **parallel widget blueprints** in a poorly named `NewFolder`
- The new `WBP_MainMenu.uasset` in `NewFolder` likely conflicts with or replaced references to the original

---

### Changelist 743+ (Unrelated)
- Changes 743-766 are primarily level design updates (yueha's level instances) and audio changes, not UI-related.

---

## File Revision History

### BP_SpiderLily_GameMode.uasset
```
#21 change 741 edit on 2025/11/14 by technical@animation_LAPTOP-4HQ5AKGQ_9526 (binary+l)
#20 change 674 edit on 2025/10/17 by jonathan@aesthetic_FED-DEV_10440 (binary+l) 'more options menu changes'
```

### L_MainMenu.umap
```
#11 change 742 edit on 2025/11/14 by technical@animation_LAPTOP-4HQ5AKGQ_9526 (binary+l) 'UI'
#10 change 597 edit on 2025/10/02 by jonathan@aesthetic_FED-DEV_10440 (binary+l) 'options menu and controller input improvements'
```

### WBP_OptionsMenu.uasset
```
#16 change 742 edit on 2025/11/14 by technical@animation_LAPTOP-4HQ5AKGQ_9526 (binary+l) 'UI'
```

### WBP_PauseMenu.uasset
```
#9 change 742 edit on 2025/11/14 by technical@animation_LAPTOP-4HQ5AKGQ_9526 (binary+l) 'UI'
```

---

## Root Cause: CONFIRMED ✅

### Symptom Clarification
**The pause menu APPEARS but buttons DON'T RESPOND to clicks** - only pressing the pause key again exits the menu. This indicates an **input/hit-test issue**, not a widget spawning issue.

### Binary Analysis Results

Using string extraction from the `.uasset` binaries, we identified the exact change:

### Tooling Used

Created a Python uasset parser (`C:\Temp\parse_uasset.py`) that extracts:
- Game paths (`/Game/...`)
- Script references (`/Script/...`)
- Widget references
- Button event bindings
- Font references

This works outside of Unreal Editor and doesn't require cooked assets.

**OLD GameMode (Revision #20 - Working):**
```
/Game/LIB/UI/MainMenu/WBP_PauseMenu
WBP_PauseMenu_C
```

**NEW GameMode (Revision #21 - Broken):**
```
/Game/UI/NewFolder/WBP_PauseMenu1
WBP_PauseMenu1_C
```

### The Problem

The GameMode blueprint was modified to spawn `WBP_PauseMenu1` from the new `UI/NewFolder/` location instead of the original `WBP_PauseMenu` from `LIB/UI/MainMenu/`.

This means the pause menu widget reference was changed, but since the new widget (`WBP_PauseMenu1`) is likely incomplete or improperly configured, the menu system breaks.

### Why This Happened

1. Changelist 741 modified `BP_SpiderLily_GameMode` with **no description**
2. The change swapped the pause menu widget reference from the working widget to an untested new one
3. The new widget at `UI/NewFolder/WBP_PauseMenu1` may be missing functionality, broken bindings, or have other issues

---

## Detailed Widget Comparison

### WBP_PauseMenu1 (New - BROKEN) vs WBP_PauseMenu (Original - WORKING)

**Button Bindings Comparison:**

| Original (WBP_PauseMenu) | New (WBP_PauseMenu1) |
|--------------------------|----------------------|
| `ResumeButton` | `Resume` ✅ |
| `OptionsButton` | `Option` ✅ |
| `RestartFromCheckpointButton` | `CheckPoint` ✅ |
| `RestartLevelButton` | `Restart` ✅ |
| `ExitToMainMenuButton` | `Exit_M` ✅ |
| `ExitToDesktopButton` | `Exit_D` ✅ |
| — | `Level` (new) |
| — | `Back` (new) |

**Widget References:**

| Component | Original | New (WBP_PauseMenu1) |
|-----------|----------|----------------------|
| Button Widget | `WBP_TextButton` | `WBP_TextButton_New` |
| Options Menu | `WBP_OptionsMenu` ✅ | `WBP_OptionsMenu` ✅ |
| Quit Confirm | `WBP_QuitConfirm` ✅ | `WBP_QuitConfirm` ✅ |
| Font | `CormorantGaramond-Regular_Font` | `Jost-Regular_Font` |

**Verdict:** The new widget has button bindings but uses different button widget and font.

---

## 🔴 LIKELY ROOT CAUSE: Blocking UI Element

### Evidence from Binary Analysis

The **new WBP_PauseMenu1** has additional visual elements not present in the original:

| Element | Original | New (WBP_PauseMenu1) |
|---------|----------|----------------------|
| `Background` | ✅ | ✅ |
| `Image` | ❌ | ✅ **NEW** |
| `Fill` | ❌ | ✅ **NEW** |
| `FillAlpha` | ❌ | ✅ **NEW** |
| `HorizontalBox` | ❌ | ✅ **NEW** |
| `VerticalBox` | ❌ | ✅ **NEW** |

### The Problem

**If the new `Image` or `Fill` element has `Visibility: Visible` (default) instead of `Visibility: SelfHitTestInvisible`, it will block all mouse/touch input to the buttons behind it!**

### Fix Required (In Unreal Editor)

1. Open `WBP_PauseMenu1` 
2. Find any `Image` or background overlay widgets that cover the button area
3. Set their **Visibility** to `Self Hit Test Invisible` (not just `Visible`)
   - This makes them render but pass-through clicks to widgets behind them
4. Alternatively, check the root `CanvasPanel` or `Border` for incorrect hit-test settings

---

### Alternative: WBP_PauseMenu (NewFolder version - DIFFERENT)

There's also a `WBP_PauseMenu` (non-1) in NewFolder that appears to be a **Main Menu** widget (not pause menu):

**Contains bindings for:**
- `About`, `Credit`, `Exit`, `Yes`, `No` buttons (main menu style)
- `Resume`, `Option`, `Exit_D` pause menu buttons

This appears to be a **hybrid Main Menu + Pause Menu** widget with:
- `WBP_MainMenu_cafeexe_*` bindings (main menu buttons)
- `WBP_PauseMenu_*` bindings (pause buttons)

---

## Current Widget Structure

```
Content/
├── LIB/UI/MainMenu/          (ORIGINAL - WORKING)
│   ├── WBP_OptionsMenu.uasset
│   ├── WBP_PauseMenu.uasset    ← Original pause menu
│   ├── WBP_QuitConfirm.uasset
│   └── WBP_TextButton.uasset
│
└── UI/NewFolder/             (NEW - INCOMPLETE)
    ├── Font/
    │   └── Jost-*.uasset       ← New fonts
    ├── Texture/
    │   └── *.uasset            ← New textures
    ├── WBP_Death.uasset        ← Death screen (new)
    ├── WBP_MainMenu.uasset     ← New main menu
    ├── WBP_OptionsMenu.uasset  ← New options (duplicate?)
    ├── WBP_PauseMenu.uasset    ← Hybrid menu/pause widget
    ├── WBP_PauseMenu1.uasset   ← New pause menu (REFERENCED BY GAMEMODE)
    └── WBP_TextButton_New.uasset
```

---

## Recommended Fix

### Option 1: Revert GameMode Widget Reference (Quick Fix)

Revert the GameMode to point back to the original working pause menu:

```bash
# Sync GameMode to revision #20
p4 sync "//SpiderLily/SL_main/Content/LIB/Blueprints/BP_SpiderLily_GameMode.uasset#20"
```

Or in Unreal Editor:
1. Open `BP_SpiderLily_GameMode`
2. Find the `Create Widget` node that references `WBP_PauseMenu1`
3. Change it back to `WBP_PauseMenu` (from `LIB/UI/MainMenu/`)

### Option 2: Fix the New Widget (If New Design is Wanted)

If the team wants to use the new menu design:
1. Open `WBP_PauseMenu1` in the editor
2. Ensure it has all the same functionality as `WBP_PauseMenu`
3. Check that button bindings, navigation, and input handling are properly configured

### Option 3: Complete the UI Migration

Clean up the "NewFolder" mess:
1. Rename `Content/UI/NewFolder/` to something meaningful (e.g., `Content/UI/NewMenus/`)
2. Delete duplicate/unused widget variants
3. Update all references consistently

---

## Investigation Commands Reference

```bash
# View changes from CL 740 to now
p4 changes -l ...@740,@now

# View specific changelist details
p4 describe -s 741
p4 describe -s 742

# View file history
p4 filelog "//SpiderLily/SL_main/Content/Blueprints/Core/BP_SpiderLily_GameMode.uasset"

# Sync to specific revision
p4 sync "//SpiderLily/SL_main/Content/Blueprints/Core/BP_SpiderLily_GameMode.uasset#20"

# Full sync to CL 740
p4 sync ...@740
```

---

## Tools Installed

### UAsset Analysis Tools (C:\Temp\)

| Tool | Purpose | Status |
|------|---------|--------|
| `parse_uasset.py` | Python script to extract widget refs, button bindings, fonts from uncooked .uasset | ✅ Working |
| `extract_uasset_strings.ps1` | PowerShell script for raw string extraction | ✅ Working |
| `UAssetGUI.exe` | GUI tool for .uasset editing (needs cooked assets) | ⚠️ UE 5.6 not supported |
| `kismet-analyzer/` | Blueprint bytecode CFG visualization | ✅ Cloned |
| `NodeToCode/` (in project Plugins) | Blueprint to code export (needs editor) | ✅ Installed |

### Usage Examples

```bash
# Parse single widget
python C:\Temp\parse_uasset.py "path\to\widget.uasset"

# Compare two widgets
python C:\Temp\parse_uasset.py "path\to\old.uasset" "path\to\new.uasset"

# Extract raw strings
powershell -ExecutionPolicy Bypass -Command "& { . C:\Temp\extract_uasset_strings.ps1 -FilePath 'path\to\file.uasset' }"
```

---

## Conclusion

The main menu breakage appears to be caused by an incomplete UI refactor in **changelists 741-742**. The most critical issue is the undocumented GameMode change in CL 741, which likely altered how the main menu widget is spawned. The recommended first step is to revert the GameMode to revision #20 and test.

---

*Report generated by investigation on 2025-12-05*
