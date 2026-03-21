# Aesthetic Computer Xbox App

A UWP/WinUI3 app that wraps aesthetic.computer for Xbox and Windows Store.

## Prerequisites

- Windows 10/11
- Visual Studio 2022 with:
  - .NET Desktop Development workload
  - Universal Windows Platform development workload
  - Windows 10/11 SDK (10.0.22621.0 or later)
- Xbox in Developer Mode ($19 one-time fee at [partner.microsoft.com](https://partner.microsoft.com))

## Setup

### 1. Open in Visual Studio

```
Open xbox/AestheticComputer.sln in Visual Studio 2022
```

### 2. Generate Assets

You'll need these icon sizes in `Assets/`:
- `StoreLogo.png` - 50x50
- `Square44x44Logo.png` - 44x44
- `Square71x71Logo.png` - 71x71 (SmallTile)
- `Square150x150Logo.png` - 150x150
- `Square310x310Logo.png` - 310x310
- `Wide310x150Logo.png` - 310x150
- `SplashScreen.png` - 620x300

Use your purple pal logo. VS can also auto-generate these from a single source.

### 3. Configure Signing

For testing:
- Right-click project → Properties → Package → Create test certificate

For Store:
- Associate with your Partner Center app
- Project → Publish → Associate App with the Store

## Deploy to Xbox

### Enable Xbox Dev Mode

1. On Xbox: **Settings → System → Console info**
2. Activate **Developer mode** (needs Partner Center registration)
3. Xbox restarts into Dev Mode

### Find Xbox IP

**Settings → General → Network settings → Advanced settings**

### Deploy from Visual Studio

1. **Debug → Remote Machine**
2. Enter Xbox IP address
3. First time: enter PIN shown on Xbox Dev Home
4. Press F5 to deploy and run

### Deploy via Device Portal

1. Open `https://<xbox-ip>:11443` in browser
2. Accept certificate warning
3. Go to **My games & apps → Add**
4. Upload the `.msix` package

### Command Line Deploy

```powershell
# Build first, then:
WinAppDeployCmd install -file "AestheticComputer.msix" -ip <xbox-ip> -pin <pin>
```

## Store Submission

### 1. Partner Center Setup

1. Go to [partner.microsoft.com](https://partner.microsoft.com)
2. Create new app: "Aesthetic Computer"
3. Reserve the name

### 2. Associate App

In Visual Studio:
- **Project → Publish → Associate App with the Store**
- Sign in and select your app

### 3. Create Package

- **Project → Publish → Create App Packages**
- Select "Microsoft Store (new app)" 
- Choose x64 and ARM64 architectures
- Upload `.msixupload` to Partner Center

### 4. Xbox Certification

For Xbox, your app must:
- ✅ Work with gamepad only (no mouse required)
- ✅ Support 1080p and 4K
- ✅ Handle TV safe zones
- ✅ Pass Xbox Requirements Tests (XR)

Run **Windows App Cert Kit** before submitting.

## Gamepad Support

The app injects `window.isXboxApp = true` into the page. 

In aesthetic.computer, you can check this:
```javascript
if (window.isXboxApp) {
  // Xbox-specific UI adjustments
}
```

Standard gamepad API works - Xbox controller maps to browser Gamepad API.

## Troubleshooting

### "Cannot connect to remote machine"
- Ensure Xbox and PC are on same network
- Check Xbox IP in Dev Home app
- Disable any VPN

### "Package deployment failed"
- Match package architecture to Xbox (usually x64)
- Check certificate is valid
- Ensure Xbox has enough storage

### WebView2 issues
- Xbox includes Edge WebView2 runtime
- If issues, check Xbox OS is updated

## Architecture

```
xbox/
├── AestheticComputer.sln          # Solution file
├── AestheticComputer/
│   ├── AestheticComputer.csproj   # Project file
│   ├── Package.appxmanifest       # App manifest (icons, capabilities)
│   ├── App.xaml(.cs)              # App entry point
│   ├── MainWindow.xaml(.cs)       # WebView2 window
│   ├── app.manifest               # Windows manifest
│   └── Assets/                    # Icons and splash screens
└── README.md
```

## Links

- [Xbox Dev Mode setup](https://learn.microsoft.com/en-us/windows/uwp/xbox-apps/devkit-activation)
- [Partner Center](https://partner.microsoft.com)
- [WebView2 docs](https://learn.microsoft.com/en-us/microsoft-edge/webview2/)
- [Xbox app requirements](https://learn.microsoft.com/en-us/windows/uwp/xbox-apps/system-resource-allocation)
