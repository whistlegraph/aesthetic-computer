# SameBoy in Unreal Engine 5.6 -- Game Boy ROM on Texture

**Goal:** Clone the SameBoy emulator, build it as a C library, and integrate it into the SpiderLily UE5.6 project so a Game Boy ROM renders live onto a texture/material applied to a mesh in-world.

**Host machine:** Jeffrey's MacBook Pro (ac-host / host.docker.internal)
**UE project:** SpiderLily (Perforce: `ssl:falsework.helixcore.io:1666`, depot `//depot/SpiderLily/SL_main/...`)
**MacBook workspace:** `~/Perforce/spiderlily_build_workspace_macbook/`

---

## Progress Tracker

- [x] **Phase 1.1** Clone SameBoy to `~/Projects/SameBoy` on MacBook
- [x] **Phase 1.2** Install dependencies (`brew install rgbds cppp`)
- [x] **Phase 1.3** Build `libsameboy.a` (ARM64, native_release) -- 390KB static lib
- [x] **Phase 1.4** Verify build -- 22 headers (1836 lines), lib links clean
- [x] **Phase 2.1** Copy lib + headers to `Source/ThirdParty/SameBoy/` in UE project
- [x] **Phase 2.2** Create `SameBoy.Build.cs` (external module rules)
- [x] **Phase 2.3** Create `GameBoyEmulatorComponent.h` (Public/) and `.cpp` (Private/)
- [x] **Phase 2.4** Update `SpiderLily.Build.cs` with RHI/RenderCore + SameBoy lib path
- [x] **Phase 2.5** Extract SPIDERLILY.gbc ROM from email (mail@aesthetic.computer) -- 524KB
- [x] **Phase 2.6** Copy ROM to `Content/SPIDERLILY.gbc` in UE project
- [x] **Phase 3.0** Compile SpiderLily Editor -- SUCCEEDED (11s, SameBoy linked)
- [x] **Phase 3.1** GameBoyScreenActor created in C++ (no Blueprint needed) -- auto-creates mesh, material, loads ROM
- [ ] **Phase 3.2** Open editor, place GameBoyScreenActor in level, test in PIE
- [ ] **Phase 3.3** Wire up input routing (keyboard/gamepad to GB buttons)
- [ ] **Phase 3.4** Test in PIE (Play In Editor)
- [ ] **Phase 4** Audio routing (optional)
- [ ] **Phase 5** Polish (CRT shader, save states, interaction system)

---

## Existing Infrastructure

### false.work / SpiderLily
- UE5.6 project managed in Perforce (Helix Core)
- Build automation in `/false.work/unreal-builder/`
- Mac builds work on M4 Mac Mini and Jeffrey's MacBook
- FMOD audio integration already in place
- Build config: `/false.work/unreal-builder/config/build-config.json`

### Game Boy in AC (web)
- WasmBoy (WebAssembly) emulator already integrated in bios.mjs
- `gameboy.mjs` piece renders 160x144 framebuffer
- `slgb.mjs` auto-loads SpiderLily.gbc ROM
- `/kidlisp-gameboy/` has GBDK toolchain for ROM development
- SpiderLily ROM asset: `/system/public/assets/false.work/SpiderLily.gbc`

---

## Phase 1: Clone & Build SameBoy as Library

### 1.1 Clone SameBoy
```bash
# On the MacBook (SSH from devcontainer)
ssh jas@host.docker.internal

# Clone into a working directory alongside the UE project
cd ~/Projects  # or wherever you prefer
git clone https://github.com/LIJI32/SameBoy.git
cd SameBoy
```

**Repository:** https://github.com/LIJI32/SameBoy
**License:** MIT
**Language:** Portable C
**Build system:** GNU Make

### 1.2 Install Dependencies
```bash
# RGBDS (required for boot ROM compilation)
brew install rgbds

# cppp (required for header cleanup when building as library)
# Check if available via brew, otherwise build from source
brew install cppp || pip3 install cppp
```

No SDL2 needed for the library target.

### 1.3 Build as Static Library
```bash
cd ~/Projects/SameBoy

# For native ARM64 (MacBook Pro Apple Silicon)
make lib CONF=native_release

# Output:
#   build/lib/libsameboy.a       (static library)
#   build/lib/libsameboy.dylib   (dynamic library)
#   build/lib/headers/            (public API headers)
```

For a universal binary (x86-64 + ARM64):
```bash
make lib CONF=fat_release
```

### 1.4 Verify the Build
```bash
# Check the library was built
ls -la build/lib/
file build/lib/libsameboy.a

# Check headers
ls build/lib/headers/
```

---

## Phase 2: Integrate into SpiderLily UE5 Project

### 2.1 Add SameBoy as a Third-Party Library

Create the directory structure in the Perforce workspace:
```
SpiderLily/
  Source/
    ThirdParty/
      SameBoy/
        include/         # Copy from build/lib/headers/
        lib/
          Mac/
            libsameboy.a  # ARM64 static library
          Win64/
            sameboy.lib   # (future: Windows build)
        SameBoy.Build.cs  # UE build rules
```

### 2.2 Create SameBoy.Build.cs
```csharp
// Source/ThirdParty/SameBoy/SameBoy.Build.cs
using UnrealBuildTool;
using System.IO;

public class SameBoy : ModuleRules
{
    public SameBoy(ReadOnlyTargetRules Target) : base(Target)
    {
        Type = ModuleType.External;

        string ThirdPartyPath = Path.Combine(ModuleDirectory);
        string IncludePath = Path.Combine(ThirdPartyPath, "include");

        PublicIncludePaths.Add(IncludePath);

        if (Target.Platform == UnrealTargetPlatform.Mac)
        {
            string LibPath = Path.Combine(ThirdPartyPath, "lib", "Mac", "libsameboy.a");
            PublicAdditionalLibraries.Add(LibPath);
        }
        else if (Target.Platform == UnrealTargetPlatform.Win64)
        {
            string LibPath = Path.Combine(ThirdPartyPath, "lib", "Win64", "sameboy.lib");
            PublicAdditionalLibraries.Add(LibPath);
        }
    }
}
```

### 2.3 Create the GameBoy Emulator Actor Component

**File:** `Source/SpiderLily/GameBoyEmulator/GameBoyEmulatorComponent.h`
```cpp
#pragma once

#include "CoreMinimal.h"
#include "Components/ActorComponent.h"
#include "Engine/Texture2DDynamic.h"
#include "GameBoyEmulatorComponent.generated.h"

// Forward declare SameBoy types
extern "C" {
    #include "gb.h"
}

UCLASS(ClassGroup=(Custom), meta=(BlueprintSpawnableComponent))
class SPIDERLILY_API UGameBoyEmulatorComponent : public UActorComponent
{
    GENERATED_BODY()

public:
    UGameBoyEmulatorComponent();

    // ROM loading
    UFUNCTION(BlueprintCallable, Category="GameBoy")
    bool LoadROMFromFile(const FString& FilePath);

    UFUNCTION(BlueprintCallable, Category="GameBoy")
    bool LoadROMFromBuffer(const TArray<uint8>& ROMData);

    // Emulator control
    UFUNCTION(BlueprintCallable, Category="GameBoy")
    void StartEmulation();

    UFUNCTION(BlueprintCallable, Category="GameBoy")
    void StopEmulation();

    UFUNCTION(BlueprintCallable, Category="GameBoy")
    void SetButtonState(int32 Button, bool Pressed);

    // Output texture (160x144 RGBA)
    UPROPERTY(BlueprintReadOnly, Category="GameBoy")
    UTexture2DDynamic* ScreenTexture;

    // Material to auto-apply texture to
    UPROPERTY(EditAnywhere, BlueprintReadWrite, Category="GameBoy")
    UMaterialInterface* ScreenMaterial;

    UPROPERTY(EditAnywhere, BlueprintReadWrite, Category="GameBoy")
    FName TextureParameterName = "GameBoyScreen";

protected:
    virtual void BeginPlay() override;
    virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;
    virtual void TickComponent(float DeltaTime, ELevelTick TickType,
        FActorComponentTickFunction* ThisTickFunction) override;

private:
    GB_gameboy_t GB;
    uint32 PixelBuffer[160 * 144];  // XRGB8888 framebuffer
    bool bEmulating = false;
    bool bFrameReady = false;

    void UpdateTexture();

    // SameBoy callbacks
    static void VBlankCallback(GB_gameboy_t* gb, GB_vblank_type_t type);
};
```

**File:** `Source/SpiderLily/GameBoyEmulator/GameBoyEmulatorComponent.cpp`
```cpp
#include "GameBoyEmulatorComponent.h"
#include "Engine/Texture2DDynamic.h"
#include "Materials/MaterialInstanceDynamic.h"

static const int32 GB_WIDTH = 160;
static const int32 GB_HEIGHT = 144;

UGameBoyEmulatorComponent::UGameBoyEmulatorComponent()
{
    PrimaryComponentTick.bCanEverTick = true;
}

void UGameBoyEmulatorComponent::BeginPlay()
{
    Super::BeginPlay();

    // Create dynamic texture for Game Boy screen
    ScreenTexture = UTexture2DDynamic::Create(GB_WIDTH, GB_HEIGHT);
    ScreenTexture->Filter = TF_Nearest;  // Pixel-perfect scaling
    ScreenTexture->SRGB = true;

    // Initialize SameBoy
    GB_init(&GB, GB_MODEL_CGB_E);  // Game Boy Color
    GB_set_pixels_output(&GB, PixelBuffer);
    GB_set_vblank_callback(&GB, &UGameBoyEmulatorComponent::VBlankCallback);

    // Store 'this' pointer for callbacks
    GB_set_user_data(&GB, this);
}

void UGameBoyEmulatorComponent::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    StopEmulation();
    GB_free(&GB);
    Super::EndPlay(EndPlayReason);
}

bool UGameBoyEmulatorComponent::LoadROMFromFile(const FString& FilePath)
{
    int Result = GB_load_rom(&GB, TCHAR_TO_UTF8(*FilePath));
    return Result == 0;
}

bool UGameBoyEmulatorComponent::LoadROMFromBuffer(const TArray<uint8>& ROMData)
{
    GB_load_rom_from_buffer(&GB, ROMData.GetData(), ROMData.Num());
    return true;
}

void UGameBoyEmulatorComponent::StartEmulation()
{
    bEmulating = true;
}

void UGameBoyEmulatorComponent::StopEmulation()
{
    bEmulating = false;
}

void UGameBoyEmulatorComponent::SetButtonState(int32 Button, bool Pressed)
{
    GB_set_key_state(&GB, (GB_key_t)Button, Pressed);
}

void UGameBoyEmulatorComponent::TickComponent(float DeltaTime, ELevelTick TickType,
    FActorComponentTickFunction* ThisTickFunction)
{
    Super::TickComponent(DeltaTime, TickType, ThisTickFunction);

    if (!bEmulating) return;

    // Run enough cycles for ~1 frame (Game Boy runs at ~59.7 fps)
    // GB_run executes one instruction at a time; run until vblank fires
    bFrameReady = false;
    while (!bFrameReady && bEmulating)
    {
        GB_run(&GB);
    }

    UpdateTexture();
}

void UGameBoyEmulatorComponent::UpdateTexture()
{
    if (!ScreenTexture) return;

    // Convert XRGB8888 to BGRA8 and upload to GPU
    FTexture2DDynamicResource* TextureResource =
        static_cast<FTexture2DDynamicResource*>(ScreenTexture->GetResource());

    if (TextureResource)
    {
        // Use UpdateTextureRegions for efficient GPU upload
        struct FUpdateData
        {
            uint32* SourceBuffer;
        };

        FUpdateData* UpdateData = new FUpdateData();
        UpdateData->SourceBuffer = PixelBuffer;

        ENQUEUE_RENDER_COMMAND(UpdateGameBoyTexture)(
            [TextureResource, UpdateData](FRHICommandListImmediate& RHICmdList)
            {
                FRHITexture* TextureRHI = TextureResource->GetTexture2DRHI();
                if (TextureRHI)
                {
                    uint32 Stride;
                    void* TextureData = RHICmdList.LockTexture2D(
                        TextureRHI, 0, RLM_WriteOnly, Stride, false);

                    if (TextureData)
                    {
                        // Copy pixel buffer (convert XRGB -> BGRA)
                        uint8* Dest = (uint8*)TextureData;
                        for (int32 i = 0; i < 160 * 144; i++)
                        {
                            uint32 Pixel = UpdateData->SourceBuffer[i];
                            Dest[i * 4 + 0] = (Pixel >> 0) & 0xFF;   // B
                            Dest[i * 4 + 1] = (Pixel >> 8) & 0xFF;   // G
                            Dest[i * 4 + 2] = (Pixel >> 16) & 0xFF;  // R
                            Dest[i * 4 + 3] = 255;                    // A
                        }
                        RHICmdList.UnlockTexture2D(TextureRHI, 0, false);
                    }
                }
                delete UpdateData;
            }
        );
    }
}

void UGameBoyEmulatorComponent::VBlankCallback(GB_gameboy_t* gb, GB_vblank_type_t type)
{
    UGameBoyEmulatorComponent* Self =
        static_cast<UGameBoyEmulatorComponent*>(GB_get_user_data(gb));
    if (Self)
    {
        Self->bFrameReady = true;
    }
}
```

### 2.4 Update SpiderLily.Build.cs

Add SameBoy dependency to the game module:
```csharp
PublicDependencyModuleNames.AddRange(new string[] {
    "Core", "CoreUObject", "Engine", "InputCore",
    "RHI", "RenderCore"  // For texture updates
});

// Add SameBoy third-party module
AddPrivateDependency("SameBoy");
```

---

## Phase 3: Blueprint Setup

### 3.1 Create a GameBoy Screen Actor
1. Create a new Blueprint Actor: `BP_GameBoyScreen`
2. Add a **Static Mesh** component (a plane or TV-shaped mesh)
3. Add the **GameBoyEmulatorComponent**
4. Create a **Material** with a Texture parameter named "GameBoyScreen"
5. In the Blueprint:
   - On BeginPlay: Create Dynamic Material Instance, set on mesh
   - Set `ScreenMaterial` on the component
   - Call `LoadROMFromFile` with the path to SpiderLily.gbc
   - Call `StartEmulation`

### 3.2 Input Routing
Map player input (keyboard/gamepad) to `SetButtonState`:
- GB_KEY_A = 0, GB_KEY_B = 1
- GB_KEY_SELECT = 2, GB_KEY_START = 3
- GB_KEY_RIGHT = 4, GB_KEY_LEFT = 5
- GB_KEY_UP = 6, GB_KEY_DOWN = 7

---

## Phase 4: Audio (Optional)

SameBoy provides audio via `GB_set_audio_callback`. To route Game Boy audio into UE5:
1. Set sample rate: `GB_set_sample_rate(&GB, 44100)`
2. Implement audio callback to fill a circular buffer
3. Use a **USoundWaveProcedural** or **Audio Submix** to play the buffer
4. This can be deferred -- visual output is the priority

---

## Phase 5: Polish & Ship

- **Save states:** `GB_save_state_to_buffer` / `GB_load_state_from_buffer`
- **Screen shader:** Add CRT scanline / pixel grid effect in the material
- **Interaction:** Player walks up to a Game Boy in-world, presses interact, input routes to emulator
- **Multiple instances:** Each `GameBoyEmulatorComponent` is independent, can run different ROMs on different objects

---

## Reference Projects

| Project | URL | Notes |
|---------|-----|-------|
| **SameBoy** | https://github.com/LIJI32/SameBoy | MIT, portable C, our emulator |
| **UnrealLibretro** | https://github.com/N7Alpha/UnrealLibretro | UE Libretro frontend, good reference for texture pipeline |
| **UnrealGB** | https://github.com/CarstenZarbock/UnrealGB | UE4 GB emulator port, incomplete but demonstrates the pattern |
| **SameBoyT4** | https://github.com/Ryzee119/SameBoyT4 | Teensy embedded port, good minimal embedding example |

---

## File Checklist

### On MacBook (Perforce workspace)
- [ ] Clone SameBoy to `~/Projects/SameBoy`
- [ ] `brew install rgbds`
- [ ] `make lib CONF=native_release`
- [ ] Copy `build/lib/headers/` to `SpiderLily/Source/ThirdParty/SameBoy/include/`
- [ ] Copy `build/lib/libsameboy.a` to `SpiderLily/Source/ThirdParty/SameBoy/lib/Mac/`
- [ ] Create `SameBoy.Build.cs`
- [ ] Create `GameBoyEmulatorComponent.h` and `.cpp`
- [ ] Update `SpiderLily.Build.cs` with dependencies
- [ ] Copy SpiderLily.gbc ROM into `Content/ROMs/` (or load from a known path)
- [ ] Create Blueprint actor with mesh + component
- [ ] Create screen material with texture parameter
- [ ] Test in editor (PIE)

### In this repo (aesthetic-computer)
- [ ] This plan file

---

## Quick Start Commands

```bash
# SSH to MacBook from devcontainer
ssh jas@host.docker.internal

# Clone SameBoy
cd ~/Projects && git clone https://github.com/LIJI32/SameBoy.git

# Install deps & build
brew install rgbds
cd SameBoy && make lib CONF=native_release

# Verify
ls build/lib/
# Should see: libsameboy.a, libsameboy.dylib, headers/

# Copy into UE project
UE_PROJECT=~/Perforce/spiderlily_build_workspace_macbook/SL_main/SpiderLily
mkdir -p "$UE_PROJECT/Source/ThirdParty/SameBoy/include"
mkdir -p "$UE_PROJECT/Source/ThirdParty/SameBoy/lib/Mac"
cp -r build/lib/headers/* "$UE_PROJECT/Source/ThirdParty/SameBoy/include/"
cp build/lib/libsameboy.a "$UE_PROJECT/Source/ThirdParty/SameBoy/lib/Mac/"
```
