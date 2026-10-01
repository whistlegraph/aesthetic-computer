using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Net.Sockets;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

namespace Aesel;

// One app process and one serialized credential owner. Rotating refresh tokens
// stay in a CurrentUser DPAPI envelope; only access tokens enter the WebView.
internal sealed class NativeAuth(string directory)
{
    internal const string ClientId = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
    internal const string Callback = "http://localhost:44233/callback";
    static readonly HttpClient Http = new() { Timeout = TimeSpan.FromSeconds(30) };
    readonly SemaphoreSlim gate = new(1, 1);
    readonly string file = Path.Combine(directory, "account.dat");
    sealed record Credentials(string Access, string? Refresh, long Expires);

    Credentials? Read()
    {
        if (!File.Exists(file)) return null;
        try { return JsonSerializer.Deserialize<Credentials>(ProtectedData.Unprotect(File.ReadAllBytes(file), null, DataProtectionScope.CurrentUser)); }
        catch (CryptographicException) { throw new InvalidOperationException("Windows could not unlock your saved sign-in. Sign out and sign in again."); }
    }

    void Save(Credentials credentials)
    {
        var bytes = ProtectedData.Protect(JsonSerializer.SerializeToUtf8Bytes(credentials), null, DataProtectionScope.CurrentUser);
        File.WriteAllBytes(file + ".new", bytes);
        File.Move(file + ".new", file, true);
    }

    async Task<Credentials> Exchange(object body, string? previousRefresh = null)
    {
        using var response = await Http.PostAsJsonAsync("https://hi.aesthetic.computer/oauth/token", body);
        using var json = JsonDocument.Parse(await response.Content.ReadAsStringAsync());
        if (!response.IsSuccessStatusCode)
            throw new InvalidOperationException("AC sign-in could not renew. Sign in again; your saved pieces are safe.");
        var root = json.RootElement;
        var access = root.GetProperty("access_token").GetString();
        if (string.IsNullOrEmpty(access)) throw new InvalidOperationException("AC did not return an access token.");
        var refresh = root.TryGetProperty("refresh_token", out var value) ? value.GetString() : previousRefresh;
        var expires = DateTimeOffset.UtcNow.ToUnixTimeSeconds() + root.GetProperty("expires_in").GetInt64();
        return new(access, refresh, expires);
    }

    async Task<string> Token()
    {
        var saved = Read() ?? throw new InvalidOperationException("Sign in with Aesthetic Computer.");
        if (saved.Expires > DateTimeOffset.UtcNow.ToUnixTimeSeconds() + 60) return saved.Access;
        if (string.IsNullOrEmpty(saved.Refresh)) throw new InvalidOperationException("Sign in again to renew your AC session.");
        var next = await Exchange(new { grant_type = "refresh_token", client_id = ClientId, refresh_token = saved.Refresh }, saved.Refresh);
        Save(next);
        return next.Access;
    }

    internal static string Base64Url(byte[] bytes) => Convert.ToBase64String(bytes).TrimEnd('=').Replace('+', '-').Replace('/', '_');

    internal static Dictionary<string, string>? ParseCallback(string target, string state)
    {
        if (target.Length > 8192 || !target.StartsWith("/callback?", StringComparison.Ordinal) || target.Contains('#')) return null;
        var query = new Dictionary<string, string>();
        foreach (var part in target[10..].Split('&'))
        {
            var pair = part.Split('=', 2);
            if (pair.Length != 2 || !query.TryAdd(Uri.UnescapeDataString(pair[0]), Uri.UnescapeDataString(pair[1]))) return null;
        }
        if (!query.TryGetValue("state", out var returned) || returned != state) return null;
        if (!query.ContainsKey("code") && !query.ContainsKey("error")) return null;
        return query;
    }

    async Task SignIn(CancellationToken closed)
    {
        using var timeout = CancellationTokenSource.CreateLinkedTokenSource(closed);
        timeout.CancelAfter(TimeSpan.FromMinutes(5));
        var cancel = timeout.Token;
        var verifier = Base64Url(RandomNumberGenerator.GetBytes(32));
        var state = Base64Url(RandomNumberGenerator.GetBytes(32));
        var parameters = new Dictionary<string, string> {
            ["response_type"] = "code", ["client_id"] = ClientId, ["redirect_uri"] = Callback,
            ["scope"] = "openid profile email offline_access", ["state"] = state, ["prompt"] = "login",
            ["code_challenge_method"] = "S256", ["code_challenge"] = Base64Url(SHA256.HashData(Encoding.UTF8.GetBytes(verifier)))
        };
        using var listener = new TcpListener(IPAddress.Loopback, 44233);
        listener.Server.ExclusiveAddressUse = true;
        try { listener.Start(8); }
        catch (SocketException) { throw new InvalidOperationException("Another AC sign-in is using port 44233. Finish it, then try again."); }
        Process.Start(new ProcessStartInfo("https://hi.aesthetic.computer/authorize?" + string.Join('&', parameters.Select(p => Uri.EscapeDataString(p.Key) + "=" + Uri.EscapeDataString(p.Value)))) { UseShellExecute = true });
        Dictionary<string, string>? grant = null;
        while (grant == null)
        {
            using var client = await listener.AcceptTcpClientAsync(cancel);
            using var requestTimeout = CancellationTokenSource.CreateLinkedTokenSource(cancel);
            requestTimeout.CancelAfter(TimeSpan.FromSeconds(3));
            var stream = client.GetStream();
            var bytes = new byte[8192];
            var length = 0;
            string request = "";
            try {
                while (length < bytes.Length && !request.Contains("\r\n\r\n")) {
                    var count = await stream.ReadAsync(bytes.AsMemory(length), requestTimeout.Token);
                    if (count == 0) break;
                    length += count; request = Encoding.ASCII.GetString(bytes, 0, length);
                }
            } catch (OperationCanceledException) when (!cancel.IsCancellationRequested) { continue; }
            var first = request.Split("\r\n")[0].Split(' ');
            if (request.Contains("\r\n\r\n") && first.Length == 3 && first[0] == "GET") grant = ParseCallback(first[1], state);
            var body = Encoding.UTF8.GetBytes(grant == null ? "Invalid sign-in response." : "Return to Aesel to finish signing in. You can close this tab.");
            var header = Encoding.ASCII.GetBytes($"HTTP/1.1 {(grant == null ? "400 Bad Request" : "200 OK")}\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {body.Length}\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n");
            try { await stream.WriteAsync(header, cancel); await stream.WriteAsync(body, cancel); } catch (IOException) { }
        }
        listener.Stop();
        if (grant.ContainsKey("error") || !grant.TryGetValue("code", out var code) || code.Length == 0)
            throw new InvalidOperationException("Sign-in was cancelled or refused.");
        var tokens = await Exchange(new { grant_type = "authorization_code", client_id = ClientId, redirect_uri = Callback, code_verifier = verifier, code });
        closed.ThrowIfCancellationRequested();
        Save(tokens);
    }

    internal async Task<object?> Call(string method, CancellationToken closed)
    {
        await gate.WaitAsync(closed);
        try {
            switch (method) {
                case "checkSession": return null;
                case "isAuthenticated": return Read() != null;
                case "getTokenSilently": return await Token();
                case "getUser":
                    using (var request = new HttpRequestMessage(HttpMethod.Get, "https://hi.aesthetic.computer/userinfo")) {
                        request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", await Token());
                        using var response = await Http.SendAsync(request, closed);
                        response.EnsureSuccessStatusCode();
                        return await response.Content.ReadFromJsonAsync<JsonElement>(closed);
                    }
                case "loginWithRedirect": await SignIn(closed); return null;
                case "logout": File.Delete(file); return null;
                default: throw new InvalidOperationException("Unknown sign-in operation.");
            }
        } finally { gate.Release(); }
    }
}
