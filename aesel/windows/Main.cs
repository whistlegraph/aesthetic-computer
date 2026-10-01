using System.Diagnostics;
using System.IO;
using System.Net.Http;
using System.Net.Http.Json;
using System.Reflection;
using System.Text.Json;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.Wpf;

namespace Aesel;

internal static class Program
{
    [STAThread]
    public static int Main(string[] args)
    {
        var smoke = args.Length == 2 && args[0] == "--smoke-test" ? Path.GetFullPath(args[1]) : null;
        using var instance = new Mutex(true, smoke == null ? @"Local\AestheticComputer.Aesel.Windows" : @"Local\Aesel.Smoke", out var first);
        if (!first) { MessageBox.Show("Aesel is already open.", "Aesel"); return 0; }
        var app = new Application();
        return app.Run(new MainWindow(smoke));
    }
}

internal sealed class MainWindow : Window
{
    const string Home = "https://aesel.app/try/index.html";
    readonly WebView2 view = new();
    readonly CancellationTokenSource closed = new();
    readonly string data;
    readonly string? smokeDirectory;
    readonly TextBlock status = new() { Text = "Opening Aesel…", FontSize = 18, Margin = new Thickness(28), TextWrapping = TextWrapping.Wrap };
    readonly NativeAuth auth;
    bool busy;

    internal MainWindow(string? smoke)
    {
        smokeDirectory = smoke;
        data = smoke == null ? Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "Aesthetic Computer", "Aesel") : Path.Combine(smoke, "profile");
        Directory.CreateDirectory(data);
        auth = new NativeAuth(data);
        Title = "Aesel"; Width = 1200; Height = 820; MinWidth = 740; MinHeight = 580;
        Background = new SolidColorBrush(Color.FromRgb(244, 237, 219));
        var grid = new Grid(); grid.Children.Add(view); grid.Children.Add(status); Content = grid;
        Loaded += async (_, _) => await Open();
        Closing += (_, e) => {
            if (busy && smoke == null && MessageBox.Show("A piece is still being made or published. Close Aesel anyway?", "Aesel", MessageBoxButton.YesNo) != MessageBoxResult.Yes) e.Cancel = true;
        };
        Closed += (_, _) => { closed.Cancel(); view.Dispose(); };
    }

    internal static bool IsApp(string url) => Uri.TryCreate(url, UriKind.Absolute, out var uri) && uri.Scheme == "https" && uri.Host == "aesel.app" && uri.IsDefaultPort && uri.UserInfo.Length == 0 && (uri.AbsolutePath == "/try/" || uri.AbsolutePath == "/try/index.html");

    static void OpenExternal(string url)
    {
        if (Uri.TryCreate(url, UriKind.Absolute, out var uri) && uri.Scheme == "https" && uri.UserInfo.Length == 0)
            Process.Start(new ProcessStartInfo(uri.AbsoluteUri) { UseShellExecute = true });
    }

    async Task Open()
    {
        try {
            var environment = await CoreWebView2Environment.CreateAsync(null, Path.Combine(data, "WebView2"));
            await view.EnsureCoreWebView2Async(environment);
            var core = view.CoreWebView2;
            core.Settings.AreHostObjectsAllowed = false;
            core.Settings.AreDevToolsEnabled = smokeDirectory != null;
            core.Settings.IsStatusBarEnabled = false;
            core.SetVirtualHostNameToFolderMapping("aesel.app", Path.Combine(AppContext.BaseDirectory, "www"), CoreWebView2HostResourceAccessKind.Deny);
            core.NavigationStarting += (_, e) => {
                if (!IsApp(e.Uri)) { e.Cancel = true; if (e.IsUserInitiated && smokeDirectory == null) OpenExternal(e.Uri); }
            };
            core.NewWindowRequested += (_, e) => { e.Handled = true; if (e.IsUserInitiated && smokeDirectory == null) OpenExternal(e.Uri); };
            core.PermissionRequested += (_, e) => { e.State = CoreWebView2PermissionState.Deny; };
            core.WebMessageReceived += async (_, e) => {
                if (!IsApp(e.Source) || !IsApp(core.Source) || smokeDirectory != null) return;
                long id = 0;
                try {
                    using var request = JsonDocument.Parse(e.WebMessageAsJson);
                    var root = request.RootElement;
                    if (root.TryGetProperty("busy", out var value)) { busy = value.GetBoolean(); return; }
                    id = root.GetProperty("id").GetInt64();
                    var result = await auth.Call(root.GetProperty("method").GetString() ?? "", closed.Token);
                    if (!closed.IsCancellationRequested && IsApp(core.Source)) core.PostWebMessageAsJson(JsonSerializer.Serialize(new { id, result }));
                } catch (Exception error) {
                    if (!closed.IsCancellationRequested && IsApp(core.Source)) core.PostWebMessageAsJson(JsonSerializer.Serialize(new { id, error = error is OperationCanceledException ? "Sign-in timed out. Try again." : error.Message }));
                }
            };
            // The web workflow disables Sign out for the full inference/upload turn.
            await core.AddScriptToExecuteOnDocumentCreatedAsync("if(window===top && location.origin==='https://aesel.app') addEventListener('DOMContentLoaded',()=>{const b=document.getElementById('logout'); if(b) new MutationObserver(()=>chrome.webview.postMessage({busy:b.disabled})).observe(b,{attributes:true,attributeFilter:['disabled']});});");
            core.ProcessFailed += (_, _) => { status.Text = "Aesel's view stopped. Close and reopen Aesel; saved pieces remain on this PC."; status.Visibility = Visibility.Visible; };
            if (smokeDirectory != null) { SmokeTest.Attach(core); await SmokeTest.Prepare(core); }
            var loaded = new TaskCompletionSource<bool>();
            core.NavigationCompleted += (_, e) => loaded.TrySetResult(e.IsSuccess);
            core.Navigate(Home);
            if (!await loaded.Task.WaitAsync(TimeSpan.FromSeconds(30))) throw new InvalidOperationException("The bundled Aesel workspace could not open.");
            status.Visibility = Visibility.Collapsed;
            if (smokeDirectory != null) {
                await SmokeTest.Run(core, smokeDirectory, auth);
                Application.Current.Shutdown(0);
            } else { _ = LaunchPing(); }
        } catch (Exception error) {
            if (smokeDirectory != null) {
                Directory.CreateDirectory(smokeDirectory); File.WriteAllText(Path.Combine(smokeDirectory, "error.txt"), error.ToString()); Application.Current.Shutdown(1); return;
            }
            status.Text = error is WebView2RuntimeNotFoundException ? "Install Microsoft Edge WebView2 Runtime, then reopen Aesel." : "Aesel could not open: " + error.Message;
            status.Visibility = Visibility.Visible;
            if (error is WebView2RuntimeNotFoundException && MessageBox.Show("Aesel needs Microsoft's WebView2 Runtime. Open Microsoft's download page?", "Aesel", MessageBoxButton.YesNo) == MessageBoxResult.Yes)
                OpenExternal("https://developer.microsoft.com/microsoft-edge/webview2/");
        }
    }

    async Task LaunchPing()
    {
        try {
            if (File.Exists(Path.Combine(data, "disable-launch-ping"))) return;
            var path = Path.Combine(data, "install-id");
            var fresh = !File.Exists(path);
            if (fresh) File.WriteAllText(path, Guid.NewGuid().ToString());
            using var http = new HttpClient { Timeout = TimeSpan.FromSeconds(10) };
            var version = Assembly.GetExecutingAssembly().GetName().Version!;
            await http.PostAsJsonAsync("https://aesthetic.computer/api/app-open", new { app = "aesel", version = $"{version.Major}.{version.Minor}.{version.Build}", platform = "windows", install = File.ReadAllText(path).Trim(), fresh });
        } catch { /* Launch counting never blocks the workspace. */ }
    }
}
