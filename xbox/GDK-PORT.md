# The GDK port

Companion to `papers/arxiv-oskiewar-store/store.tex`, which established that UWP
is closed to Xbox Store games and that ID@Xbox plus a GDK port is the only retail
console route. This is the engineering plan that follows from it, plus an answer
to the question of whether the live-reload loop can survive the trip.

The first draft of this document was written on macOS and nothing in it had been
compiled. Two of the three things it flagged as unverified are now settled, one
of them against the plan: **there is no D3D11 on the Xbox Game OS at all.** The
port is a bigger job than the first draft priced, and §1a says how much bigger.

There is now a CI lane — `.github/workflows/xbox-gdk-desktop.yml`, separate from
the AppVeyor UWP MSIX build, which is untouched — that installs the public GDK
and builds `xbox/native-bios/gdk/NativeBiosGdk.vcxproj` on every push touching
`xbox/native-bios/**`.

Sources were fetched from Microsoft-owned pages on **7 and 8 August 2026**.
Every claim carries its URL and that date.

---

## 1. What App.cpp actually asks of Windows

Grepped at HEAD, not recalled. `App.cpp` is 2,653 lines with 48 `ref new` sites
across twelve WinRT namespaces. The inventory:

| Capability | WinRT surface used | Sites | GDK replacement |
| --- | --- | --- | --- |
| App model | `IFrameworkView`, `CoreApplicationView`, `CoreApplication::Run`, `Activated` | 288-295, 507, 2642-2651 | `WinMain` + `XGameRuntimeInitialize` |
| Window | `CoreWindow`, `CoreWindowEventArgs`, `Closed`, `SystemNavigationManager::BackRequested` | 295-298, 511, 2489 | `CreateWindowExW` + `WndProc` |
| Message pump | `CoreDispatcher::ProcessEvents(ProcessAllIfPresent)` | 447 | `PeekMessageW` loop |
| Swap chain | `CreateSwapChainForCoreWindow` | 559 | `CreateSwapChainForHwnd` on PC; on console the whole D3D11 graph is gone (§1a) |
| Controller | `Gamepad::Gamepads`, `GetCurrentReading`, `GamepadButtons`, `RawGameController` | 1679-1740, 1876-1885 | `GameInput` (`IGameInput::GetCurrentReading`) |
| Package path | `Package::Current->InstalledLocation->Path` | 156, 593 | `GetModuleFileNameW` |
| Local path | `ApplicationData::Current->LocalFolder->Path` | 65, 2129 | `XPersistentLocalStorageGetPathSize`/`GetPath` — UTF-8, needs widening |
| File I/O | `_wfopen_s`, `_wstat64`, `fread` | throughout | **already portable** |
| Removable media | `KnownFolders::RemovableDevices`, `StorageFolder`, `StorageFile` | `PhotoDiscService.cpp` | none — cut on console (10.13.4) |
| HTTP | `HttpClient::GetStringAsync` / `GetBufferAsync` / `PostAsync`, `HttpStringContent`, `Uri` | 1351-1357, 1425-1430, 1810-1813, 1917-1921, 2061-2062 | `libHttpClient` |
| UDP | `DatagramSocket`, `BindServiceNameAsync`, `GetOutputStreamAsync`, `HostName`, `DataWriter` | 1227-1290, 1467-1480 | Winsock |
| WebSocket | `MessageWebSocket` | `OskiewarLivePublisher.cpp` | `HCWebSocket*` (libHttpClient) |
| MIDI | `MidiInPort`, `DeviceInformation::FindAllAsync`, `MidiMessageType` | 1513-1542, 1567-1596 | none on console |
| JSON | `JsonObject`, `JsonValue`, `JsonValueType` | 1386-1404, 1944-1985 | vendor a parser (not a platform capability) |
| Image decode | `BitmapDecoder`, `PixelDataProvider`, `BitmapTransform` | 2076-2087, `PhotoDiscService.cpp` | none — vendor `stb_image` |
| Text | `DWriteCreateFactory`, `CreateFontFileReference`, `ID2D1DeviceContext::DrawGlyphRun` | 593+, 2412 | verbatim on PC; **no D2D/DWrite on console** (§1a) |
| Device info | `AnalyticsInfo::VersionInfo`, `EasClientDeviceInformation` | 1841-1850 | `XSystemGetDeviceType` |
| Memory | `MemoryManager::AppMemoryUsage` and two limits | 1856-1858 | `XMemGetAllocationStatistics` |
| Network state | `NetworkInformation::GetInternetConnectionProfile` | 1859 | `XNetworkingGetConnectivityHint` |
| Language | C++/CX: `ref new`, `String^`, `TypedEventHandler`, `create_task`, `task<>` | 48 / 8 / 4 / 15 / 11 | standard C++ against flat C APIs |

What crosses untouched is more than it looks. `QuickJsEngine.cpp` — 873 lines,
the entire interpreter binding — contains **zero** WinRT references. All six HLSL
shaders, the D3D11 render graph, the XAudio2 voice graph and its DSP, the image
effects in `xbox/runtime/`, and `oskiewar.js` are portable. Storage is nearly free
because every read is already `_wfopen_s` on a wide path; only *acquiring* the
path is WinRT.

- **JSON is not a platform capability.** `Windows.Data.Json` should be replaced
  with one vendored header used by both backends, not hidden behind an interface.

---

## 1a. Two GDKs, and the graphics answer

### Which GDK you can actually have

The **public GDK** is free, needs no licence, and targets Windows PC. It is on
GitHub at <https://github.com/microsoft/GDK> (accessed 8 August 2026), installs
with `winget install Microsoft.Gaming.GDK`, and — the part that matters for CI —
Microsoft publishes the same headers and libraries as NuGet packages
(`Microsoft.GDK.Windows`, whose release notes read "Provides BWOI support for
Microsoft GDK (260403) for Windows projects";
<https://www.nuget.org/packages/Microsoft.GDK.Windows>, accessed 8 August 2026).
That is a scripted, unattended, cacheable install on any hosted runner. Our CI
downloads the 141 MB `.nupkg` and keeps the ~25 MB of it we link against.

The **Xbox Extensions** — the GXDK, which is what produces
`Gaming.Xbox.Scarlett.x64` and `Gaming.Xbox.XboxOne.x64` — are not. The repo
README's words: "A version of the GDK with Xbox Extensions (GDKX) to target Xbox
consoles is only available to licensed partners in a managed program." This is
not merely a download gate. The console reference documentation itself returns
*Access restricted*: "Access to this topic requires membership in a
non-disclosure agreement (NDA) Xbox developer program"
(<https://learn.microsoft.com/en-us/gaming/gdk/docs/features/graphics/d3d12x/d3d12x-overview>,
accessed 8 August 2026, signed out). So a hosted runner cannot build the console
target, and neither can anyone here until ID@Xbox concept approval lands.

Also worth knowing before writing a project file: as of the October 2025 GDK the
new header and library layout **replaces the `Gaming.Desktop.x64` custom MSBuild
platform with the stock `x64` platform**, plus include/lib paths under
`<gdk>\windows\`, and the old layout is scheduled for removal about a year out.
<https://learn.microsoft.com/en-us/gaming/gdk/docs/gdk-dev/pc-dev/overviews/gdk-new-layout>
(accessed 8 August 2026). `NativeBiosGdk.vcxproj` is built the new way, which is
also the only way that works from the NuGet payload — the custom platform's
`.props`/`.targets` ship with the installer, not with the package.

### Direct2D, DirectWrite, and the thing underneath them

Settled, and it splits by target.

**On PC the answer is yes, and trivially so.** A GDK PC title is a plain Win32
process: "PC game developers are free to continue using the full power of the
PC; this includes all existing Win32 APIs… *Restricting what games and game
developers can do on PC is not and never will be a goal for the Microsoft Game
Development Kit (GDK).*" So `D2D1CreateFactory`, `DWriteCreateFactory` and
`CreateFontFileReference` over the packaged TTF are the existing App.cpp code
verbatim. The smoke harness creates all three and pulls a glyph index out of
`ywft-processing-regular.ttf`, so this is a checked fact on that target rather
than a reading.

**On console the answer is no — and the reason is worse than the question.**
The same page carries, twice, in a call-out: "**On Xbox, Microsoft Game
Development Kit (GDK) supports only D3D12.x.** D3D11.x is not supported. If you
have a game that uses D3D11.x, you'll need to move to D3D12.x as part of moving
to the Microsoft Game Development Kit (GDK). On Windows PC, both D3D11 and D3D12
are supported."
<https://learn.microsoft.com/en-us/gaming/gdk/docs/gdk-dev/intro/introduction>
(accessed 8 August 2026). Chuck Walbourn, who authors both the GDK layout docs
and DirectXTK, puts the text half plainly: "UWP on the Xbox does support
Direct2D/DirectWrite, but Microsoft GDKX and Xbox One XDK do not"
(<https://github.com/microsoft/DirectXTK12/wiki/Drawing-text>), and elsewhere
"The Xbox Game OS does not support Direct3D 11, Direct2D/DirectWrite, OpenGL,
GDI, or any older version of Direct3D"
(<https://walbourn.github.io/vcpkg-now-supports-targeting-xbox/>, both accessed
8 August 2026).

This is the largest single correction to the first draft, which had the D3D11
render graph in the "crosses untouched" column. It does not. **Console is a
D3D12 rewrite of the renderer, not a port of it** — six shaders, the post chain,
the sprite path and the stencil pass, all of it — and the text path on top of
that is a glyph atlas rasterized from the packaged TTF, because there is no
DirectWrite to ask. The swap chain question the first draft raised is moot: it
was a D3D11/DXGI question about an API that is not there.

Two consequences worth acting on. The PC target is now the near-term one, and
it is genuinely close — everything above compiles and links today. And if
console is still the goal, the renderer should be written against D3D12 *on PC*
first, where it can be developed and tested without a devkit, rather than
carried to console as D3D11 and discovered there.

---

## 2. The abstraction

`xbox/native-bios/platform/ac_platform.hpp` — new, and the substance of this
work. It sits opposite the existing `xbox/runtime/include/ac/runtime.hpp`:
`runtime.hpp` faces the piece, `ac_platform.hpp` faces the machine. Namespace
`ac::xbox::platform`, matching the existing idiom, free of Windows headers so it
reads and eventually compiles anywhere.

Fourteen interfaces, one per row of the inventory that genuinely differs between
backends: `Window`, `HostEvents`, `Paths`, `Clock`, `Input`, `Http`, `UdpInlet`,
`UdpOutlet`, `WebSocket`, `Midi`, `AudioDevice`, `ImageDecoder`, `TextRenderer`,
`SystemInfo` — aggregated behind a single `Platform` and a single
`make_platform(HostEvents&)`. Which backend linked is a link-time fact chosen by
the build, never a runtime branch.

It is deliberately not idealized. `Midi` hands back a raw status byte because
`App.cpp` already decodes NoteOn/NoteOff/PitchBend/ControlChange itself.
`AudioDevice` hides only engine creation, because the voice graph is shared code
and XAudio2 exists on both sides. Sockets are three narrow interfaces — a bound
inlet, a fixed-destination outlet, a text WebSocket — because those are the
three shapes actually in use, not a general socket API. `Paths::removable()`
defaults to empty, because a retail console backend must return nothing there.

`HostEvents` carries `suspending`, `resuming` and `constrained`, which `App.cpp`
handles nowhere today. XR-001 requires all three of a console title, so the
interface asks for them now rather than after a certification failure.

`platform/gdk/ac_platform_gdk.cpp` is the second backend, and it is now in a
project and compiled. Real implementations, against the real GDK headers:

- `Window` — `RegisterClassExW` + `CreateWindowExW` + a `WndProc`, and a
  `PeekMessageW` pump for `CoreDispatcher::ProcessEvents`. `native_handle()`
  hands back the HWND, which `CreateSwapChainForHwnd` takes directly.
- `Paths` — `GetModuleFileNameW` for the package root;
  `XPersistentLocalStorageGetPathSize`/`GetPath` for the writable one. Note the
  GDK spells it `GetPath`, not `GetRootPath` as the first draft guessed, and it
  returns UTF-8 where the interface wants UTF-16, so there is a widening step.
  Off console it falls back to `%LOCALAPPDATA%` when there is no package
  identity, which is the loose-build and CI case and marked as such.
- `Clock` — the split divide, `GetSystemTimeAsFileTime`, `GetTickCount64`.
- `Input` — `GameInputCreate` + `GetCurrentReading(GameInputKindGamepad, …)` +
  `GetGamepadState`. The bit values in `GameInputGamepadButtons` are identical to
  the WinRT `GamepadButtons` ones, so the name table at App.cpp:1698 carries
  across unchanged; that was a guess in the first draft and is now checked
  against the header.
- `SystemInfo` — `XSystemGetAnalyticsInfo`, `XSystemGetDeviceType`,
  `XNetworkingGetConnectivityHint`. The SSID has no GDK equivalent and is
  dropped rather than faked.
- `AudioDevice` — `XAudio2Create` + a mastering voice.

Still `// TODO(gdk)`, compiled but inert: `Http`, `UdpInlet`, `UdpOutlet`,
`WebSocket`, `Midi`, `ImageDecoder`, `TextRenderer`. Each names the replacement
API and the `App.cpp` line it stands in for. They are last on purpose — a
headless smoke test needs a clock, a path and a file handle, and needs none of
these.

The WinRT backend is not extracted. Its implementation is the code already in
`App.cpp`, and moving 2,653 lines of C++/CX behind an interface without a
compiler to check the move is exactly the sweeping edit nobody can verify. The
inventory table above is the extraction map; doing it is a Windows job and a
short one.

### The clock overflow, fixed

`App.cpp:1789` computed monotonic time as `counter * 1000000 / frequency`. On a
10 MHz QPC that multiply overflows int64 at about 10.6 days of uptime and starts
reporting negative time — three live bugs, none of which anyone connected to how
long the console had been on. Now:

```cpp
counter.QuadPart / frequency.QuadPart * 1000000 +
counter.QuadPart % frequency.QuadPart * 1000000 / frequency.QuadPart
```

Same value, no overflow until roughly 29,000 years. The remainder term is
bounded by `frequency`, so it cannot overflow either.

Now measured rather than reasoned. Compiled and run over nine cases including a
year of uptime: on a 10 MHz QPC the naive form is still correct at ten days and
reports `-894274407370` at eleven, while the split form reports `950400000000`.
The check is `ClockMathHolds` in `gdk/gdk_smoke.cpp` and it runs in CI.

There was a **second** site with the same bug that the first pass missed:
`App.cpp:1783`, the `qpc_us` stamp on the `AC_NATIVE_INPUT` telemetry line. It
is now the split form too. Every other `* 1000000` on a QPC value in the tree is
on a *delta* in `double`, which is fine.

---

## 3. Dynamic updates

@jeffrey's question: can we keep the live-reload workflow under GDK and flag it
out before submitting? The instinct is right and the answer splits in two.

### What policy forecloses

Store Policies v7.19, published 10 September 2025, effective 14 October 2025;
page revised 30 July 2026.
<https://learn.microsoft.com/en-us/windows/apps/publish/store-policies>
(accessed 7 August 2026). Correcting the brief and the paper's section numbering:
**10.2 is Security.** "Product is Testable" is 10.3, and 10.8 is Financial
Transactions. There is no policy section about downloading executable code.

**10.2.5**, verbatim:

> All game products, (exclusive of games made available through a subscription
> in PC gaming subscription products) and any products offered on Xbox consoles
> must be submitted using supported package types for ingestion and distribution
> by the Microsoft Store. For any products submitted in this manner, such
> products and in-product offerings must be installed and updated only through
> the Microsoft Store. (Note: This policy does not apply to products that are
> subject to the requirements in 10.2.9.)

The 10.2.9 carve-out permits a direct HTTPS installer URL, and is explicitly
"Non-gaming products" that "may only be made available to PC devices." It does
not reach us. XR-009 restates the rule in blunter and broader terms — "All of
your apps, modules, and in-app products must be installed, **serviced**, and
updated only through the Store."
<https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/console/certification-requirements>
(v16.3, 1 July 2026; accessed 7 August 2026).

**10.2.2**, verbatim, and this is the clause that actually governs an
interpreter:

> Your product must not attempt to fundamentally change or extend its described
> functionality or introduce features or functionality that are in violation of
> Store Policies through any form of dynamic inclusion of code. Your product
> should not, for example, download a remote script and subsequently execute
> that script in a manner that is not consistent with the described functionality

Note the hinge: **"consistent with the described functionality."** 10.2.2 does
not prohibit downloading script, and it does not prohibit interpreting script.
It prohibits using downloaded script to become a different product than the one
certified and described on the store page. Compliance is a function of the
listing copy, which means it is a decision made before submission rather than an
argument had after it.

The wording also moved in our favour. The archived v7.6 text read "must not
attempt to change or extend the described functionality"; v7.16 inserted
"fundamentally" and split off the policy-violation limb.
<https://learn.microsoft.com/en-us/windows/apps/publish/store-policy-archive/store-policy-7-6>
and
<https://learn.microsoft.com/en-us/windows/apps/publish/store-policies-change-history>
(both accessed 7 August 2026). XR-009 still quotes the **pre-2022** wording, so
the XR document and current Store Policy disagree on this sentence. Cite both.

### Where the line actually sits

Games download data constantly and Microsoft documents the mechanisms for doing
it. Three primary sources put content-after-release beyond argument:

**Global Title Storage** exists for "Data that everyone can read, such as
rosters, maps, challenges, or art resources," is developer-written through
Partner Center, caps at 256 MB, and throttles to roughly one call per minute
averaged over an hour.
<https://learn.microsoft.com/en-us/gaming/gdk/docs/services/storage/title-storage/live-title-storage-overview>
(accessed 7 August 2026). Its getting-started page names "daily challenges, game
maps, and art resources" as the intended payload.

**PlayFab Title Data** is the sharpest citation available. Microsoft's own words:
"Use it to manage game settings remotely without deploying a new build," and
Override Title Data allows "updat[ing] the remote configuration of the game
without the need to deploy a new build."
<https://learn.microsoft.com/en-us/gaming/playfab/live-service-management/game-configuration/titledata/>
(accessed 7 August 2026). It is free for games shipping on Xbox.

**DLC packages** are Store-distributed by construction — authored as add-ons in
Partner Center, enumerated and mounted at runtime with `XPackageMount`.
<https://learn.microsoft.com/en-us/gaming/gdk/docs/features/common/packaging/packaging-downloadable-content-dlc>
(accessed 7 August 2026).

So the line is not "may a shipped title fetch things." It is: **the certified
binary, and the functionality the store page describes, may only change through
the Store.** Data that a shipped runtime consumes is content. Script that makes
the runtime do something the listing does not describe is a product update.

The single most useful passage on our exact case is in XR-018, under game mods
and scripting.
<https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/xr/xr018>
(v1.6, 1 September 2025; accessed 7 August 2026). Verbatim, console-only bullets:

> UGC may not contain standalone executables or be constructed in such a way
> that an intermediary step (JIT, script compilation etc) would output an
> executable file.
>
> Mods must not have any form of direct network access.
>
> Mods must not have direct file system access.

Read closely, that is not a ban on interpreting script. It is four engineering
constraints, and the current architecture already satisfies all four. QuickJS-ng
is a bytecode interpreter with no JIT and never emits a file. The `Api` in
`runtime.hpp` gives pieces no socket, no URL, no path and no filesystem
primitive — the host owns every destination, which was a deliberate design
choice and turns out to be the thing that makes this position defensible. This
should be said out loud in an exception request, not discovered in review.

### The answer

Yes to the build split. No to shipping the live lane.

On console, the Device Portal path and any HTTPS piece fetch are dead — 10.2.5
is categorical, XR-009 says "serviced," and there is no sandbox or
creative-runtime exception. The `xbox/ota/` lane is retired for console, and its
closing policy note reasons from Apple's App Review 2.5.2 rather than Microsoft's
rulebook; that framing should go.

What survives is better than nothing: the retail title can legitimately pull
**data** after release through Global Title Storage or PlayFab Title Data — new
fighter rosters, tuning constants, seasonal palettes, event schedules, the AC
community feed — as long as it is data the shipped `oskiewar.js` interprets, not a
replacement for `oskiewar.js`. If the live loop matters enough, the design move is
to widen what `oskiewar.js` reads as data, so a large fraction of what is currently
hot-reloaded as script becomes content the certified runtime consumes.

On PC the calculus differs, because only 10.2.2 applies and it is intent-based.
A listing that describes a fighting game and then hot-loads new pieces changes
its described functionality. A listing that describes an Aesthetic Computer
runtime whose declared purpose is running AC pieces is a different conversation.
That conversation is had in the listing copy, deliberately, before submission.

---

## 4. The build split

### What ships today

Worse than the paper recorded, and worth correcting. `xbox/native-bios` has **no
CI at all**. The only workflow, `.github/workflows/xbox-native-latency.yml`,
builds `xbox/native-latency` — a different, older probe project — and triggers
only on `xbox/native-latency/**`. So `native-bios` is built by hand.

And the piece it shipped was not `smoke_piece.js`. It was `kSmokePiece`, a
twelve-line raw string literal at `App.cpp:49`, compiled in and staged
unconditionally at boot. `smoke_piece.js` is a byte-identical duplicate copied
into the package as Content and **never read by anything**. Two copies of the
same demo, one of them dead. The 5,813-line game arrived only over Device
Portal.

### The mechanism

Three layers, because a single preprocessor flag is not the "cannot reach it"
property the question asks for.

**1. Fail-safe default.** `AC_DEV_LIVE_PIECE` guards the live lane and defaults
to `1` in `NativeBios.vcxproj` via an `AcDevLivePiece` property. A retail build
is `msbuild /p:AcDevLivePiece=0`. Defaulting on preserves today's behaviour
exactly; the flag is a deliberate opt-out, and a new configuration that forgets
it inherits dev, not accidental retail — which is the safe direction for a flag
whose failure mode is a broken dev loop rather than a bad submission.

**2. Source exclusion in the GDK project.** For the port, the live lane moves
out of `App.cpp` into its own translation unit behind `platform::LiveSource`,
listed in the `<ClCompile>` group of the dev configuration only. Retail does not
compile a definition of `make_live_source`. A stray call then fails at **link**,
loudly, instead of shipping a code path that can reach out-of-band script. That
inversion is the point: absence is enforced by the linker, not by a reviewer
reading `#if` blocks. The declaration in `ac_platform.hpp` is itself inside
`#if AC_DEV_LIVE_PIECE`, so retail cannot even name the type.

**3. A CI tripwire on the artifact, not the source.** The retail job greps the
built package for the strings that only the live lane contains —
`live-piece.js`, `AC_NATIVE_LIVE_READY`, the Device Portal host — and fails if
any survive. Checking the output is what catches a flag that silently stopped
being passed.

### Getting the game into the package

Landed, and it is the smallest high-value change here. `App.cpp` now reads
`oskiewar.js` out of the package at boot through the existing `ReadPackageBytes`,
and falls back to `kSmokePiece` only if the file is absent. `NativeBios.vcxproj`
adds `..\live\oskiewar.js` as Content, alongside the shaders and the fonts.

Package Content rather than a compiled-in byte array, for three reasons. MSVC
rejects string literals over 65,535 characters and `oskiewar.js` is 237 KB, so the
obvious raw-string approach does not compile. Package content is signed,
immutable, and Store-delivered, which is precisely what 10.2.5 asks for. And it
keeps the game a file in the repo that the existing tooling can still edit. If
certification ever objects to reading script off disk, the fallback is a
generated byte-array header — but the objection would be misplaced, since the
package is the unit the Store signs and distributes.

### CI

`.github/workflows/xbox-gdk-desktop.yml`, on `windows-2022`, on any push
touching `xbox/native-bios/**`, `xbox/runtime/**` or `xbox/live/oskiewar.js` —
the game is package content now, so it belongs in the trigger.

GitHub Actions rather than AppVeyor, for one reason: AppVeyor's config here is a
single `build_script` producing the UWP MSIX, and that is still how the console
gets its builds. A second workflow file cannot touch it; a second job inside
`appveyor.yml` would mean restructuring it. Actions also caches the GDK payload,
which is 141 MB we do not want to fetch on every push.

The steps: restore or fetch the pinned `Microsoft.GDK.Windows` NuGet payload and
keep only `windows/{include,lib/x64,bin/x64}`; read `_GRDK_EDITION` back out of
`grdk.h` so the log says which GDK built it; MSBuild the project; run the smoke
binary, which exits non-zero on any required check and names it. There is also
an informational step that recompiles the interpreter core under
`WINAPI_FAMILY=WINAPI_FAMILY_GAMES` — the API partition a console build uses —
to find out early whether QuickJS-ng sits inside the console subset. It reports
rather than gates, because without the Xbox Extensions it cannot be conclusive.

**The retail dev/live split described above is still design, not code.** The
`AcDevLivePiece` property and the `AC_DEV_LIVE_PIECE` define exist in both
projects and the GDK one defaults to `0`, but the string scan over a built
package and the `oskiewar.js` size assertion are not written, and the WinRT lane's
dev/retail jobs are not written. That is a separate change against
`appveyor.yml` and should be one.

---

## 5. Risk

**An interpreter in a submitted package will draw questions.** The most likely
objection is not 10.2.2 — it is XR-018. The runtime's declared purpose is
interpreting user-authored pieces, and XR-018 states that "Titles that leverage
UGC as a core gameplay mechanic (such as drawing games and sandbox games with
customizable worlds) remain in-scope for this XR." A reviewer will read AC
exactly that way, and they will be right. XR-018 is certification-tested, its
test case 018-01 is published, and it requires published content guidelines, an
in-product reporting path or proactive detection, readiness to remove content on
request, and graceful handling of users whose UGC privilege is restricted. None
of that exists in the codebase.

**The privilege gate is the sharpest edge.** `XPRIVILEGE_USER_CREATED_CONTENT`
(247) governs whether a user may see others' UGC at all, and XR-018 says
blocking whole modes for restricted users "is not a preferred solution." If the
only thing the title does is render community-authored pieces, there may be no
graceful degradation available, and that becomes an exception request rather
than a code change.

**The community feed's status is genuinely unresolved.** XR-018 defines UGC as
"any in-game digital content produced by a player," and the AC chat text
`App.cpp` renders was not produced by a player of this title, in this title. But
it is displayed to other people in an online state, and XR-018 exempts only
content that stays local or stays offline — "offline content that is
subsequently shared online is not exempt." I read the requirement as applying.
That is a reading, not a citation.

**XR-009 is not certification-tested.** It carries no asterisk and there is no
Security section on the published test-cases page. That is not permission — it
means enforcement is policy review and spot-check rather than a scripted test,
which is a worse place to be caught, not a better one.

**Ask for the exception early.** The certification guide invites it in as many
words: "Microsoft does not want to block new or innovative features. If you are
creating something new, please tell us in your exception request," and requires
all exception requests closed before submission.
<https://learn.microsoft.com/en-us/gaming/game-publishing/concepts/certification/certification-guide>
(accessed 7 August 2026). An art runtime that interprets scripts is exactly the
thing that paragraph is for. Going in having already named the four XR-018
scripting constraints and shown the architecture satisfies them is a much better
conversation than being asked.

**Could not verify.** One thing:

- The often-quoted Store rule about not downloading or executing code "by any
  means other than the mechanisms provided by the platform." It is absent from
  current v7.19 and from archived v7.6. **Do not cite it.** It appears to be
  from the retired Windows Store App Certification Requirements, and no primary
  source for it was found.

The two graphics unknowns are settled in §1a. Both went against the plan.

The ID@Xbox eligibility page is JavaScript-rendered and returned a summarized
body rather than clean source; its requirements should be re-read in a browser
before anything depends on the exact wording.

---

## 6. Next steps

**Next, and the whole list changed shape once §1a landed:**

1. Get the GDK workflow green. It has never run — see §7 for exactly what a
   round-trip has to confirm.
2. Port the renderer to **D3D12, on PC**. This is now the long pole, and doing
   it on the desktop target means it can be built and tested against WARP in CI
   without a devkit. Carrying D3D11 to console is not an option; discovering
   that after concept approval would be the expensive version of this.
3. Rasterize text from the packaged TTF into an atlas, drawn through the sprite
   path. DirectWrite is a PC-only convenience now, and the atlas is what both
   targets can share. It is also a prerequisite for step 2 not being done twice.
4. Vendor `stb_image` for PNG and JPEG and one header for JSON, and route
   `PhotoDiscService` and the painting fetch through them **under WinRT first**.
   That deletes `Windows.Graphics.Imaging` and `Windows.Data.Json` from the
   inventory before the rest of the port touches them.
5. Fill in `Http` and the socket lanes on the GDK side — libHttpClient for the
   first, Winsock for the others — and move the WinRT bodies behind the same
   interfaces so the two backends converge.
6. Draft the XR-018 answer and the exception request. Content guidelines, the
   reporting path, and the privilege-restricted degradation story are design
   work, not platform work, and they sit on the critical path after concept
   approval — so having them written is pure upside.
7. Fix `xbox/ota/README.md`'s policy note, which argues Apple's rulebook.

**Needs the Xbox Extensions, which need ID@Xbox:** anything that compiles for
`Gaming.Xbox.*.x64` at all. That is a gate on the program, not on the work — and
it is why steps 2 and 3 are specified as PC work.

**Needs a devkit:** everything downstream of concept approval — achievements,
the Xbox user model, gamertag-primary naming with `@handle` demoted per XR-007,
privilege checks, suspend/resume/constrained, Recent Player reporting, and
measuring the photo-disc copy path against XR-133's 1 GiB per five minutes.

---

## 7. What is built, and what still needs a build

**Compiles and links on the GDK PC target** (pending the first CI run):
`QuickJsEngine.cpp` and all four QuickJS-ng translation units, all six HLSL
shaders, `platform/gdk/ac_platform_gdk.cpp`, and `gdk/gdk_smoke.cpp`.

**Verified without a Windows compiler**, on this machine: `ac_platform.hpp`
compiles standalone (clang, C++17) with `AC_DEV_LIVE_PIECE` both set and unset —
the first time it has been compiled at all. `QuickJsEngine.cpp` compiles clean
against clang too, which is a second data point on the claim that the whole
interpreter binding is WinRT-free. The two new Windows sources compile clean at
`-Wall -Wextra` against hand-written stub headers whose GDK signatures were
copied out of the real `XSystem.h`, `XPersistentLocalStorage.h`, `XNetworking.h`
and `GameInput.h`. That checks structure and conformance to `ac_platform.hpp`;
it does not check the D3D11/D2D/DWrite calls, which are copied from the working
`App.cpp` but unproven in this arrangement. The clock arithmetic was compiled
and run.

**Needs a CI round-trip to confirm** — nothing below has ever run on Windows:

- MSBuild accepts `NativeBiosGdk.vcxproj` with `GDKCrossPlatformPath` supplied
  on the command line, and finds `XGameRuntime.h`.
- `FxCompile` writes the six `.cso` files into `$(OutDir)`, which the smoke
  harness reads back by name.
- The D3D11 device comes up on a runner with no GPU (WARP), and `CreateWindowExW`
  plus `CreateSwapChainForHwnd` work in that session. If the window is what
  fails, `--no-window` demotes that check and the rest still gates.
- `XGameRuntimeInitialize` on an unpackaged process: expected to fail, treated
  as advisory off console, and the fallback to `%LOCALAPPDATA%` covers it.
- Whether QuickJS-ng compiles under `WINAPI_FAMILY_GAMES` — the informational
  step. A pass is real news; a fail names the first thing the console port
  trips on.

**Changed in this pass:**

- `App.cpp:1783` — the second monotonic-clock overflow, the one the first pass
  missed.
- `platform/gdk/ac_platform_gdk.cpp` — rewritten from annotated stubs into a
  working backend for `Window`, `Paths`, `Clock`, `Input`, `SystemInfo` and
  `AudioDevice`; the rest still `TODO(gdk)` and now compiled.
- `gdk/NativeBiosGdk.vcxproj` and `gdk/gdk_smoke.cpp` — new.
- `.github/workflows/xbox-gdk-desktop.yml` — new.

**Changed in the previous pass, still not compiler-checked** (the UWP lane
builds on AppVeyor, so `main` being green covers these, but no one has looked):
the `App.cpp:1806` clock fix, boot reading `oskiewar.js` out of the package, the
`#if AC_DEV_LIVE_PIECE` guards, and `NativeBios.vcxproj`'s `AcDevLivePiece`
property and `..\live\oskiewar.js` Content entry.

**Deliberately left alone:** `smoke_piece.js` and its Content entry, which are
dead; the `xbox/ota/` lane; `xbox/live/**`; and the 2,678 lines of `App.cpp`
that a sweeping unverifiable refactor would have touched. Extracting the WinRT
backend behind `ac_platform.hpp` is still the right move and still wants someone
with a Windows machine in front of them.
