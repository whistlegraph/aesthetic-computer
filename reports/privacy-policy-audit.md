# Privacy Policy Audit Report

**Generated:** February 3, 2026  
**Author:** GitHub Copilot

---

## Executive Summary

Both Aesthetic Computer and Sotce Net privacy policies were last meaningfully updated on **September 7, 2024** (AC) and **September 25, 2024** (Sotce Net). Since then, significant features have been added that should be reflected in updated privacy policies.

---

## Current State

### Aesthetic Computer Privacy Policy
**Location:** [system/public/privacy-policy.html](../system/public/privacy-policy.html)  
**Last Edited:** September 7, 2024  
**Access:** `aesthetic.computer/privacy-policy` or type `pp` in the prompt

#### Current Content:
- ✅ Finished artworks stored on remote server for sharing
- ✅ Email association with `@handle` for artwork/realtime activity
- ✅ Camera access for `camera` command and hand-tracking (`handtime`)
- ✅ Microphone access for recording (`baktok`)
- ✅ Realtime multiplayer servers (`pond`, `field`)
- ✅ No sale/exchange of data with third parties
- ✅ Account deletion via `delete-erase-and-forget-me` command
- ✅ Contact via `help` command

---

### Sotce Net Privacy Policy
**Location:** Embedded in [system/netlify/functions/sotce-net.mjs](../system/netlify/functions/sotce-net.mjs) (lines 6686-6760)  
**Last Edited:** September 25, 2024  
**Access:** `sotce.net/privacy-policy`

#### Current Content:
- ✅ Pages stored on remote server for subscriber viewing
- ✅ Email association with `@handle` for identity
- ✅ No sale/exchange of data with third parties
- ✅ Partnership disclosure (Sotce + Aesthetic Computer)
- ✅ Active subscriber count display
- ✅ Contact via `mail@sotce.net`

---

## Missing/Outdated Information

### Both Platforms Should Add:

| Category | What to Disclose | Notes |
|----------|------------------|-------|
| **Analytics** | "We collect anonymous usage data" | Generic - don't name Google Analytics |
| **Authentication** | "We use secure third-party authentication" | Generic - don't name Auth0 |
| **Payments** | "Payments processed by secure payment provider" | Generic - don't name Stripe |
| **Realtime Features** | "We connect to servers for multiplayer/chat" | Already covered, keep generic |
| **Cross-Platform Identity** | Shared handles/emails between AC and Sotce Net | Important for transparency |
| **Data Retention** | How long data is kept | User rights clarity |
| **Cookies** | "We use cookies to keep you logged in" | Required disclosure |

### Aesthetic Computer Specific:

| Category | What to Disclose | Notes |
|----------|------------------|-------|
| **AI Features** | "Some features use AI services to process your requests" | Generic - don't name OpenAI |
| **Voice Features** | "Voice/speech features may use external services" | Generic |
| **Mobile/Desktop Apps** | "Apps may collect crash reports" | Standard app store language |
| **User Programs** | "Code you write may be stored to enable sharing" | For KidLisp |

### Sotce Net Specific:

| Category | What to Disclose | Notes |
|----------|------------------|-------|
| **Subscription** | "We track subscription status for access control" | Standard |
| **Chat** | "Messages in chat are visible to other subscribers" | Already implied |

---

## Recommendations

### High Priority (Legal/Compliance):

1. **Add Generic Third-Party Services Disclosure**
   - "We use third-party services for authentication, analytics, and payments"
   - Don't name specific vendors (keeps tech stack private)

2. **Add Data Retention Policy**
   - How long paintings/artworks are stored
   - How long user accounts persist after deletion request

3. **Add Cookie/Local Storage Disclosure**
   - "We use cookies to keep you logged in and remember your preferences"

4. **Add User Rights Section**
   - Right to access data
   - Right to deletion (already have this!)
   - Contact for privacy requests

### Medium Priority (Transparency):

5. **Update Feature Lists**
   - AC: Add AI features, voice features, code storage
   - Sotce: Add chat feature

6. **Cross-Platform Data Sharing**
   - Explain how handles/emails are shared between AC and Sotce Net

### Low Priority (Optional):

7. **Add Version History/Last Updated Date**
   - Track policy changes

---

## Proposed Updated Policies

### Aesthetic Computer - Recommended New Policy

```html
<html>
<head>
  <title>Aesthetic Computer's Privacy Policy</title>
  <style>
    body {
      font-family: sans-serif;
      background-color: rgb(235, 235, 235);
      -webkit-text-size-adjust: none;
      max-width: 800px;
      margin: 0 auto;
      padding: 20px;
    }
    code { font-weight: bold; }
    h2 { margin-top: 1.5em; }
  </style>
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <link rel="icon" type="image/png"
    href="https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/painting-2023.8.21.10.45.png" />
</head>
<body>
  <h1>Aesthetic Computer's Privacy Policy</h1>
  
  <h2>What We Collect</h2>
  <p>
    Aesthetic Computer keeps finished paintings and artworks on a remote
    server so they can be shared with and viewed by users.
  </p>
  <p>
    Aesthetic Computer allows you to associate an email with a <code>@handle</code> to link
    with your artwork and realtime game activity.
  </p>
  <p>
    We collect anonymous usage data to understand how people use the platform.
  </p>

  <h2>Device Permissions</h2>
  <p>
    Aesthetic Computer will request permission to access your device's camera
    in order to take pictures in commands like <code>camera</code> and for playing with
    games that have handtracking, such as <code>handtime</code>.
  </p>
  <p>
    Aesthetic Computer will request permission to access
    your device's microphone in order to record in games like
    <code>baktok</code>.
  </p>

  <h2>AI & Voice Features</h2>
  <p>
    Some features use AI services to process your requests. Voice and speech
    features may use external services for synthesis.
  </p>

  <h2>Realtime & Multiplayer</h2>
  <p>
    Aesthetic Computer connects to realtime multiplayer servers
    for games like <code>pond</code> and <code>field</code>. Your presence
    and chat messages may be visible to other users.
  </p>

  <h2>Third-Party Services</h2>
  <p>
    We use secure third-party services for authentication, analytics, and
    payment processing. These services have their own privacy policies.
  </p>

  <h2>Related Platforms</h2>
  <p>
    Aesthetic Computer shares authentication with <a href="https://sotce.net/privacy-policy">Sotce Net</a>.
    If you use the same email on both platforms, your <code>@handle</code> may be shared between them.
  </p>

  <h2>Cookies</h2>
  <p>
    We use cookies to keep you logged in and remember your preferences.
  </p>

  <h2>Data Sharing</h2>
  <p>
    Aesthetic Computer does not sell or exchange any user data with third
    parties.
  </p>

  <h2>Your Rights</h2>
  <p>
    Delete your Aesthetic Computer account by entering <code>delete-erase-and-forget-me</code> in the prompt.
  </p>
  <p>
    For more information or to request your data, enter <code>help</code> to communicate
    with the author.
  </p>

  <h2>Mobile & Desktop Apps</h2>
  <p>
    The iOS, Android, and desktop apps follow this same policy. Mobile apps may
    collect crash reports through platform services.
  </p>

  <a href="/"><img width="128"
      src="https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/painting-2023.8.21.10.45.png"></a>
  <br><br>
  <sub>Last updated: February 3, 2026</sub>
</body>
</html>
```

### Sotce Net - Recommended New Policy

Update the HTML in `sotce-net.mjs` (lines ~6730-6755) to:

```html
<h1>Sotce Net's Privacy Policy</h1>
<p>
  Sotce Net keeps pages on a remote server so they can be shared
  with and viewed by subscribers.
</p>
<p>
  Sotce Net allows you to associate an email with a
  <code>@handle</code> to represent your identity.
</p>
<p>
  We collect anonymous usage data to understand how people use the platform.
</p>
<p>
  We use secure third-party services for authentication, analytics, and
  payment processing.
</p>
<p>
  We use cookies to keep you logged in and remember your preferences.
</p>
<p>
  Sotce Net does not sell or exchange any user data with third
  parties.
</p>
<p>
  Sotce Net is brought to you by the partnership of <code><a href="https://sotce.com">Sotce</a></code> and
  <code><a href="https://aesthetic.computer/privacy-policy">Aesthetic Computer</code>.</a>
  If you use the same email on both platforms, your <code>@handle</code> may be shared between them.
</p>
${subscribers > 0 ? "<p>Sotce Net has <code>" + subscribers + "</code> active subscriber" + (subscribers > 1 ? "s" : "") + ".</p>" : ""}
<p>
  For more information write to <code>mail@sotce.net</code> to
  communicate with the author.
</p>
<a href="${dev ? "/sotce-net" : "/"}"><img width="128" src="${assetPath + "cookie.png"}" /></a>
<br />
<br />
<sub>Last updated: February 3, 2026</sub>
```

---

## Implementation Checklist

- [ ] Update `system/public/privacy-policy.html` with expanded AC policy
- [ ] Update `system/netlify/functions/sotce-net.mjs` privacy section
- [ ] Add analytics tracking disclosure to both
- [ ] Add third-party services list to both
- [ ] Add cross-platform data sharing notice
- [ ] Update "last edited" dates
- [ ] Test both pages render correctly
- [ ] Consider adding privacy email address
- [ ] Notify existing users of policy update (optional but recommended)

---

## Files to Modify

1. [system/public/privacy-policy.html](../system/public/privacy-policy.html) - Aesthetic Computer policy
2. [system/netlify/functions/sotce-net.mjs](../system/netlify/functions/sotce-net.mjs) - Sotce Net policy (lines 6686-6760)
