using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Input;
using Microsoft.Web.WebView2.Core;
using System;
using Windows.Gaming.Input;
using Windows.System;

namespace AestheticComputer;

public sealed partial class MainWindow : Window
{
    private Gamepad? _gamepad;

    public MainWindow()
    {
        this.InitializeComponent();
        
        // Make window fullscreen on Xbox
        var appWindow = this.AppWindow;
        appWindow.SetPresenter(Microsoft.UI.Windowing.AppWindowPresenterKind.FullScreen);
        
        // Initialize WebView2
        InitializeWebView();
        
        // Setup gamepad handling
        SetupGamepad();
    }

    private async void InitializeWebView()
    {
        try
        {
            await webView.EnsureCoreWebView2Async();
            
            // Configure WebView2 for optimal Xbox experience
            var settings = webView.CoreWebView2.Settings;
            settings.IsZoomControlEnabled = false;
            settings.AreDefaultContextMenusEnabled = false;
            settings.IsStatusBarEnabled = false;
            settings.AreBrowserAcceleratorKeysEnabled = false;
            
            // Inject Xbox-specific info
            webView.CoreWebView2.AddScriptToExecuteOnDocumentCreatedAsync(@"
                window.isXboxApp = true;
                window.isDeviceMode = true;
                console.log('🎮 Aesthetic Computer Xbox App');
            ");
            
            // Handle navigation errors
            webView.CoreWebView2.NavigationCompleted += (sender, args) =>
            {
                if (!args.IsSuccess)
                {
                    // Show offline message or retry
                    webView.CoreWebView2.NavigateToString(@"
                        <html>
                        <body style='background:#6600FF;color:white;font-family:sans-serif;display:flex;align-items:center;justify-content:center;height:100vh;margin:0;'>
                            <div style='text-align:center;'>
                                <h1>📡 Connecting...</h1>
                                <p>Please check your internet connection</p>
                                <button onclick='location.reload()' style='padding:20px 40px;font-size:24px;cursor:pointer;'>Retry</button>
                            </div>
                        </body>
                        </html>
                    ");
                }
            };
        }
        catch (Exception ex)
        {
            System.Diagnostics.Debug.WriteLine($"WebView2 init failed: {ex.Message}");
        }
    }

    private void SetupGamepad()
    {
        // Watch for gamepad connections
        Gamepad.GamepadAdded += (sender, gamepad) =>
        {
            _gamepad = gamepad;
        };
        
        Gamepad.GamepadRemoved += (sender, gamepad) =>
        {
            if (_gamepad == gamepad)
                _gamepad = null;
        };
        
        // Check for already connected gamepads
        if (Gamepad.Gamepads.Count > 0)
        {
            _gamepad = Gamepad.Gamepads[0];
        }
    }
    
    // Handle keyboard/gamepad navigation
    private void OnKeyDown(object sender, KeyRoutedEventArgs e)
    {
        // Map Xbox controller buttons to web events
        switch (e.Key)
        {
            case VirtualKey.GamepadMenu:
            case VirtualKey.Escape:
                // Could show an overlay menu
                break;
            case VirtualKey.GamepadView:
                // Toggle fullscreen or info
                break;
        }
    }
}
