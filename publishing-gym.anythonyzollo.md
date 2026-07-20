# Publishing gym.anthonyzollo.com

This site is one client-side HTML file hosted on Aesthetic Computer's `lith`
server. Publishing replaces the live `index.html` atomically.

## One-time DNS setup

In the Cloudflare DNS dashboard for `anthonyzollo.com`, add:

| Type | Name | Target | Proxy status | TTL |
| --- | --- | --- | --- | --- |
| CNAME | `gym` | `lith.aesthetic.computer` | DNS only (gray cloud) | Auto |

Start with **DNS only** so lith can obtain the first HTTPS certificate. After
`https://gym.anthonyzollo.com` works, Cloudflare's orange-cloud proxy may be
enabled if desired; set SSL/TLS mode to **Full (strict)**.

Do not add an A or AAAA record for `gym` alongside this CNAME.

## One-time setup on Anthony's Mac

Jeffrey will send the publish token separately. Do not put it in the HTML, this
file, a prompt, chat, or git. Save it in the macOS Keychain:

```bash
security add-generic-password \
  -a "$USER" \
  -s gym.anthonyzollo.com-publish \
  -w 'PASTE_TOKEN_HERE' \
  -U
```

## Publish

From the folder containing `index.html`:

```bash
GYM_PUBLISH_TOKEN="$(security find-generic-password \
  -a "$USER" \
  -s gym.anthonyzollo.com-publish \
  -w)"

curl --fail-with-body \
  --request PUT \
  --header "Authorization: Bearer $GYM_PUBLISH_TOKEN" \
  --header "Content-Type: text/html; charset=utf-8" \
  --data-binary @index.html \
  https://gym.anthonyzollo.com/api/publish-gym

unset GYM_PUBLISH_TOKEN
```

A successful response looks like:

```json
{"ok":true,"bytes":12345,"revision":"abc1234","url":"https://gym.anthonyzollo.com/","publishedAt":"..."}
```

Then open <https://gym.anthonyzollo.com/> and hard-refresh if necessary.

## Claude instruction

Anthony can give Claude this file and say:

> Update `index.html`, keep the app entirely client-side, and publish it using
> the command in `publishing-gym.anythonyzollo.md`. Never print, read aloud,
> commit, or place the Keychain token in a file.

## What can be published

The endpoint accepts one `index.html` up to 5 MB. CSS and JavaScript should be
inline, or loaded from public HTTPS URLs. Anyone can view the site; only a
holder of the publish token can replace it.

## History and rewind

Every changed publish is committed to a private Git repository on lith. The
repository is outside the public web directory and cannot be downloaded from
the website. To list its latest 50 revisions:

```bash
curl --fail-with-body \
  --header "Authorization: Bearer $GYM_PUBLISH_TOKEN" \
  https://gym.anthonyzollo.com/api/history-gym
```

To restore an earlier version, use a revision returned by that command:

```bash
curl --fail-with-body \
  --request POST \
  --header "Authorization: Bearer $GYM_PUBLISH_TOKEN" \
  --header "Content-Type: application/json" \
  --data '{"revision":"abc1234"}' \
  https://gym.anthonyzollo.com/api/rewind-gym
```

Rewind does not delete newer history. It publishes the older file again as a
new commit, so another rewind can always undo it.

If the token is ever exposed, Jeffrey should replace `GYM_PUBLISH_TOKEN` in the
lith production environment and redeploy lith.
