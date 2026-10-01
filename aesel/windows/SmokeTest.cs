using System.IO;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using Microsoft.Web.WebView2.Core;

namespace Aesel;

// Explicit test mode uses a disposable profile and intercepts every network
// request. It never signs in, spends credits, publishes or sends a launch ping.
internal static class SmokeTest
{
    static int rounds;
    static string uploaded = "";
    const string Source = "export function paint({ wipe }) { wipe(\"purple\"); }\n";
    sealed class AuthTransport : HttpMessageHandler
    {
        internal int Refreshes;
        protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancel)
        {
            if (request.RequestUri?.AbsoluteUri == "https://hi.aesthetic.computer/oauth/token") {
                using var body = JsonDocument.Parse(await request.Content!.ReadAsStringAsync(cancel));
                if (body.RootElement.GetProperty("refresh_token").GetString() != "fixture-refresh") throw new Exception("Wrong refresh credential.");
                Refreshes++;
                await Task.Delay(30, cancel);
                return new(HttpStatusCode.OK) { Content = new StringContent("{\"access_token\":\"native-smoke-token\",\"refresh_token\":\"rotated-fixture\",\"expires_in\":3600}") };
            }
            throw new Exception("Unexpected native auth request: " + request.RequestUri);
        }
    }
    internal static void Attach(CoreWebView2 core)
    {
        core.AddWebResourceRequestedFilter("*", CoreWebView2WebResourceContext.All);
        core.WebResourceRequested += (_, e) => {
            var uri = new Uri(e.Request.Uri);
            if (uri.Host == "aesel.app") return; // Packaged assets; no network.
            var body = "{}"; var type = "application/json";
            if (uri.AbsolutePath == "/userinfo") body = "{\"sub\":\"auth0|windows-fixture\"}";
            else if (uri.AbsolutePath == "/handle") body = "{\"handle\":\"windows-fixture\"}";
            else if (uri.AbsolutePath == "/api/easel-credits") body = "{\"remaining\":500,\"purchased\":0}";
            else if (uri.AbsolutePath == "/api/easel-inference") {
                rounds++; type = "text/event-stream";
                object[] events = rounds == 1 ? [
                    new { type = "content_block_start", index = 0, content_block = new { type = "tool_use", id = "write-1", name = "write_piece" } },
                    new { type = "content_block_delta", index = 0, delta = new { type = "input_json_delta", partial_json = JsonSerializer.Serialize(new { source = Source }) } },
                    new { type = "content_block_stop", index = 0 },
                    new { type = "message_delta", delta = new { stop_reason = "tool_use" } }
                ] : [
                    new { type = "content_block_delta", index = 0, delta = new { type = "text_delta", text = "A purple piece. <img src=x onerror=alert(1)>" } },
                    new { type = "message_delta", delta = new { stop_reason = "end_turn" } }
                ];
                body = string.Join("", events.Select(item => "data: " + JsonSerializer.Serialize(item) + "\n\n"));
            } else if (uri.AbsolutePath.Contains("/presigned-upload-url/")) body = "{\"uploadURL\":\"https://upload.test/piece\"}";
            else if (uri.Host == "upload.test" && e.Request.Method != "OPTIONS") {
                using var reader = new StreamReader(e.Request.Content); uploaded = reader.ReadToEnd(); body = "";
            } else if (uri.AbsolutePath.EndsWith(".mjs")) { body = uploaded; type = "text/javascript"; }
            else if (uri.AbsolutePath.StartsWith("/@windows-fixture/")) { body = "<body style='background:#673c89;color:#f4c0d7;font:32px monospace;display:grid;place-items:center'>Windows preview fixture</body>"; type = "text/html"; }
            var headers = "Content-Type: " + type + "\r\nAccess-Control-Allow-Origin: *\r\nAccess-Control-Allow-Methods: GET, POST, PUT, OPTIONS\r\nAccess-Control-Allow-Headers: *\r\n";
            e.Response = core.Environment.CreateWebResourceResponse(new MemoryStream(Encoding.UTF8.GetBytes(body)), 200, "OK", headers);
        };
    }

    internal static async Task Prepare(CoreWebView2 core)
    {
        await core.AddScriptToExecuteOnDocumentCreatedAsync("""
            if(window===top && location.origin==='https://aesel.app') {
              Object.defineProperty(window,'auth0',{value:{Auth0Client:class {
                async checkSession(){} async isAuthenticated(){return !localStorage.getItem('signedOut');}
                async getUser(){return {sub:localStorage.getItem('fixtureAccount')||'auth0|windows-fixture'};}
                async getTokenSilently(){return 'windows-smoke-token';}
                async logout(){localStorage.setItem('signedOut','yes');location.reload();}
                async loginWithRedirect(){localStorage.removeItem('signedOut');location.reload();}
              }}});
            }
            """);
    }

    static async Task Until(CoreWebView2 core, string expression)
    {
        var deadline = DateTime.UtcNow.AddSeconds(30);
        while (DateTime.UtcNow < deadline) {
            if (await core.ExecuteScriptAsync(expression) == "true") return;
            await Task.Delay(100);
        }
        var state = await core.ExecuteScriptAsync("document.body.innerText");
        throw new Exception("Timed out: " + expression + "\n" + state);
    }

    internal static async Task Run(CoreWebView2 core, string directory, NativeAuth auth)
    {
        if (NativeAuth.ParseCallback("/callback?state=good&code=ok", "good")?["code"] != "ok" ||
            NativeAuth.ParseCallback("/callback?state=wrong&code=ok", "good") != null ||
            NativeAuth.ParseCallback("/callback?state=good&state=good&code=ok", "good") != null ||
            NativeAuth.ParseCallback("/callback?state=wrong&error=denied", "good") != null ||
            NativeAuth.ParseCallback("https://evil.test/callback?state=good&code=ok", "good") != null ||
            MainWindow.IsApp("https://aesel.app.evil.test/try/") || MainWindow.IsApp("https://aesel.app:123/try/") ||
            MainWindow.IsApp("https://aesel.app/anything") || MainWindow.IsApp("https://user@aesel.app/try/")) throw new Exception("Callback/origin boundary failed.");
        var secret = Encoding.UTF8.GetBytes("test-only-credential");
        if (!ProtectedData.Unprotect(ProtectedData.Protect(secret, null, DataProtectionScope.CurrentUser), null, DataProtectionScope.CurrentUser).SequenceEqual(secret)) throw new Exception("Windows credential encryption failed.");
        var transport = new AuthTransport();
        using var http = new HttpClient(transport);
        var credentialDirectory = Path.Combine(directory, "credential-check");
        Directory.CreateDirectory(credentialDirectory);
        var credentials = new NativeAuth(credentialDirectory, http);
        credentials.SeedSmokeCredential();
        var renewed = await Task.WhenAll(Enumerable.Range(0, 6).Select(_ => credentials.Call("getTokenSilently", CancellationToken.None)));
        if (transport.Refreshes != 1 || renewed.Any(token => (string?)token != "native-smoke-token")) throw new Exception("Concurrent token renewal failed.");
        var envelope = File.ReadAllBytes(Path.Combine(credentialDirectory, "account.dat"));
        if (Encoding.UTF8.GetString(envelope).Contains("fixture")) throw new Exception("Unencrypted credential.");
        var decoded = Encoding.UTF8.GetString(ProtectedData.Unprotect(envelope, null, DataProtectionScope.CurrentUser));
        if (!decoded.Contains("rotated-fixture")) throw new Exception("Rotated refresh token was not saved.");
        await credentials.Call("logout", CancellationToken.None);
        if ((bool)(await credentials.Call("isAuthenticated", CancellationToken.None))! || File.Exists(Path.Combine(credentialDirectory, "account.dat"))) throw new Exception("Sign-out retained credentials.");
        await Until(core, "!!document.getElementById('workspace') && !document.getElementById('workspace').hidden");
        await core.ExecuteScriptAsync("document.getElementById('input').value='Make a purple piece.';document.getElementById('send').click();");
        await Until(core, "!!document.querySelector('#log .answer') && !document.getElementById('preview').hidden && !document.getElementById('send').disabled");
        var uploadDeadline = DateTime.UtcNow.AddSeconds(15);
        while (uploaded != Source && DateTime.UtcNow < uploadDeadline) await Task.Delay(100);
        if (rounds != 2 || uploaded != Source) throw new Exception($"Generation/upload failed: {rounds} requests, {JsonSerializer.Serialize(uploaded)}.");
        await Until(core, "document.querySelectorAll('#log img').length===0 && !JSON.stringify(localStorage).includes('windows-smoke-token')");
        var id = await core.ExecuteScriptAsync("document.getElementById('history').value");
        core.Reload();
        await Until(core, "!!document.querySelector('#log .answer') && document.getElementById('history').value===" + id);
        await core.ExecuteScriptAsync("document.getElementById('new').click();");
        await Until(core, "document.getElementById('history').value!==" + id + " && document.querySelectorAll('#log li').length===0");
        await core.ExecuteScriptAsync("document.getElementById('history').value=" + id + ";document.getElementById('history').dispatchEvent(new Event('change'));");
        await Until(core, "!!document.querySelector('#log .answer')");
        await Task.Delay(500);
        using (var picture = File.Create(Path.Combine(directory, "workspace.png"))) await core.CapturePreviewAsync(CoreWebView2CapturePreviewImageFormat.Png, picture);
        await core.ExecuteScriptAsync("document.getElementById('logout').click();");
        await Until(core, "!document.getElementById('gate').hidden && !document.getElementById('login').disabled");
        using (var picture = File.Create(Path.Combine(directory, "signin.png"))) await core.CapturePreviewAsync(CoreWebView2CapturePreviewImageFormat.Png, picture);
        await core.ExecuteScriptAsync("localStorage.setItem('fixtureAccount','auth0|another-fixture');localStorage.removeItem('signedOut');location.reload();");
        await Until(core, "!document.getElementById('workspace').hidden && document.querySelectorAll('#log li').length===0 && document.querySelectorAll('#history option').length===1");
        File.WriteAllText(Path.Combine(directory, "result.json"), JsonSerializer.Serialize(new { passed = true, runtime = core.Environment.BrowserVersionString, checks = new[] { "packaged app launch", "Windows DPAPI", "OAuth callback state/duplicates/origin boundaries", "generation and upload (mocked)", "preview", "escaped assistant text", "token exclusion from notebooks", "reload persistence", "thread switching", "sign out", "account isolation" } }, new JsonSerializerOptions { WriteIndented = true }));
    }
}
