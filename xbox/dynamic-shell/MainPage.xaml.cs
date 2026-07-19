using System;
using System.Diagnostics;
using Windows.Foundation;
using Windows.Storage;
using Windows.System;
using Windows.UI.Core;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Input;
using Windows.Web;

namespace Aesthetic.DynamicShell
{
    public sealed partial class MainPage : Page
    {
        const string DefaultEndpoint = "https://aesthetic.computer/fight";
        const string EndpointKey = "AcEndpoint";
        readonly ApplicationDataContainer settings = ApplicationData.Current.LocalSettings;

        public MainPage()
        {
            InitializeComponent();
            Loaded += OnLoaded;
            Window.Current.CoreWindow.Dispatcher.AcceleratorKeyActivated += OnAcceleratorKey;
            SystemNavigationManager.GetForCurrentView().BackRequested += OnBackRequested;
        }

        void OnLoaded(object sender, RoutedEventArgs e)
        {
            EndpointBox.Text = ReadEndpoint();
            Navigate(EndpointBox.Text);
        }

        string ReadEndpoint()
        {
            if (IsHttpEndpoint(App.EndpointOverride)) return App.EndpointOverride;
            string endpoint = settings.Values[EndpointKey] as string;
            return IsHttpEndpoint(endpoint) ? endpoint : DefaultEndpoint;
        }

        internal void ApplyEndpointOverride()
        {
            if (!IsHttpEndpoint(App.EndpointOverride)) return;
            EndpointBox.Text = App.EndpointOverride;
            Navigate(App.EndpointOverride);
        }

        static bool IsHttpEndpoint(string value)
        {
            Uri uri;
            return Uri.TryCreate(value, UriKind.Absolute, out uri) &&
                (uri.Scheme == Uri.UriSchemeHttps || uri.Scheme == Uri.UriSchemeHttp);
        }

        void Navigate(string endpoint)
        {
            if (!IsHttpEndpoint(endpoint)) { Status.Text = "invalid endpoint (http/https required)"; return; }
            settings.Values[EndpointKey] = endpoint;
            Browser.Navigate(new Uri(endpoint));
        }

        void OnNavigationStarting(WebView sender, WebViewNavigationStartingEventArgs e)
        {
            Status.Text = "loading " + e.Uri;
            Debug.WriteLine("[AC NAV] -> " + e.Uri);
        }

        async void OnNavigationCompleted(WebView sender, WebViewNavigationCompletedEventArgs e)
        {
            Status.Text = e.IsSuccess ? "connected — Menu opens endpoint" : "navigation failed: " + e.WebErrorStatus;
            Debug.WriteLine("[AC NAV] " + (e.IsSuccess ? "ok" : e.WebErrorStatus.ToString()));
            if (!e.IsSuccess) return;
            try
            {
                await Browser.InvokeScriptAsync("eval", new[] { @"
                    (function () {
                      window.isXboxApp = true;
                      window.isDeviceMode = true;
                      if (window.__acShellTelemetry) return;
                      window.__acShellTelemetry = true;
                      ['log','info','warn','error'].forEach(function(level) {
                        var original = console[level];
                        console[level] = function() {
                          var text = Array.prototype.map.call(arguments, function(x) {
                            try { return typeof x === 'string' ? x : JSON.stringify(x); }
                            catch (_) { return String(x); }
                          }).join(' ');
                          try { window.external.notify('console|' + level + '|' + text.slice(0, 1500)); } catch (_) {}
                          return original.apply(console, arguments);
                        };
                      });
                      window.addEventListener('error', function(e) {
                        try { window.external.notify('error|' + e.message + ' @ ' + e.filename + ':' + e.lineno); } catch (_) {}
                      });
                      window.external.notify('shell|ready|' + location.href);
                    })();" });
            }
            catch (Exception ex) { Debug.WriteLine("[AC JS inject] " + ex.Message); }
        }

        void OnScriptNotify(object sender, NotifyEventArgs e)
        {
            Debug.WriteLine("[AC WEB] " + e.Value);
            if (e.Value.StartsWith("error|") || e.Value.StartsWith("console|error|")) Status.Text = e.Value;
        }

        void OnBackRequested(object sender, BackRequestedEventArgs e)
        {
            e.Handled = true;
            HandleBack();
        }

        void OnAcceleratorKey(CoreDispatcher sender, AcceleratorKeyEventArgs e)
        {
            if (e.EventType != CoreAcceleratorKeyEventType.KeyDown && e.EventType != CoreAcceleratorKeyEventType.SystemKeyDown) return;
            if (e.VirtualKey == VirtualKey.GamepadMenu) { e.Handled = true; ToggleSettings(); }
            else if (e.VirtualKey == VirtualKey.GamepadB || e.VirtualKey == VirtualKey.Escape) { e.Handled = true; HandleBack(); }
            else if (SettingsPanel.Visibility == Visibility.Visible && (e.VirtualKey == VirtualKey.GamepadA || e.VirtualKey == VirtualKey.Enter))
            { e.Handled = true; SettingsPanel.Visibility = Visibility.Collapsed; Navigate(EndpointBox.Text.Trim()); Browser.Focus(FocusState.Programmatic); }
        }

        void HandleBack()
        {
            if (SettingsPanel.Visibility == Visibility.Visible) { SettingsPanel.Visibility = Visibility.Collapsed; Browser.Focus(FocusState.Programmatic); }
            else
            {
                // Capture B/back at the shell boundary: never let it terminate the app.
                Browser.InvokeScriptAsync("eval", new[] { "window.dispatchEvent(new CustomEvent('ac-shell-back'));" });
                Status.Text = "B/back captured";
            }
        }

        void ToggleSettings()
        {
            SettingsPanel.Visibility = SettingsPanel.Visibility == Visibility.Visible ? Visibility.Collapsed : Visibility.Visible;
            if (SettingsPanel.Visibility == Visibility.Visible) EndpointBox.Focus(FocusState.Programmatic); else Browser.Focus(FocusState.Programmatic);
        }
    }
}
