# NCCd

A Mac app for the NELA Computer Club. It puts the club's Signal chats and
circles, the wiki and the code in one window. The d is for donut.

Download it: https://aesthetic.computer/nccd/

## What you get

- **The donut.** The home page. Each chat is a token on the donut. A larger
  token has more members, and a token that beats had a message a short time
  ago. The newest message of each chat floats beside it. Select one to go in.
- **Chats and circles.** Read and write in the club's Signal groups. Type `@`
  to mention a member. Right-click a chat to give it a window of its own.
- **Wiki and code.** The club wiki and the club's Forgejo, each in its own tab.
- **Search.** One search over the chats and the wiki, in the bar at the bottom.
- **Notifications.** A new message in a club chat shows as a macOS
  notification, so you do not need Signal Desktop open.
- **Updates.** The app updates itself.

## Get started

1. Download the app. It needs macOS 13 or later.
2. Move it into Applications.
3. Open it. If your Mac has [Homebrew](https://brew.sh), NCCd installs its
   Signal engine (`signal-cli`) itself. If your Mac has no Homebrew, install
   Homebrew first.
4. Select **Link Signal** and scan the code with Signal on your phone
   (Settings, then Linked devices). This opens the chats and circles.
5. Select **Sign into NCC** and sign in with your club account. This opens the
   wiki and the code.

The two logins are separate. You can use one without the other.

## Is it safe?

- NCCd is a linked device of your Signal account, as Signal Desktop is.
  `signal-cli` does the encryption with Signal's own library, so your messages
  stay encrypted end to end. No Aesthetic Computer server is in the path.
- A linked device receives all the messages of your account. NCCd keeps and
  shows only the club's groups.
- You type your club password into the club's own login page. NCCd keeps the
  session that the wiki gives you, in your login Keychain.
- The app is signed and notarized by Apple. It has no analytics.
- NCCd is a beta, and it is not affiliated with Signal.

To remove it: in Signal on your phone, remove NCCd from Linked devices. Then
delete the app.

## Build it yourself

You need a Mac with the Xcode command line tools.

```
./build.sh --run
```

This builds `build/NCCd.app` and opens it.

## Help make it better

The club's copy of the code is on our Forgejo:
https://git.nela.computer/jeffrey/nccd

- **Fixes and patches.** We take pull requests there.
- **Ideas and problems.** Open an issue there if you want a change or have a
  suggestion. Feel free.

Two files help you find your way in the code:

- `DEVELOPING.md`: the fast loop, the release, tests and settings.
- `SCHEMA.md`: the map of the code.

We make releases from the Aesthetic Computer monorepo, in `slab/nccd`:
https://github.com/whistlegraph/aesthetic-computer

We carry a change that we take on Forgejo into the monorepo. It is then in the
next release.

Signal is a trademark of Signal Technology Foundation.
