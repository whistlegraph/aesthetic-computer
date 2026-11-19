# NCCd security

How NCCd handles your Signal account, your club login and what it keeps on
your Mac. Each part shows the code that does the work, what it guarantees, and
where it stops. This describes NCCd 0.3.23.

NCCd is a beta by one developer. No outside party has audited it. It is not
affiliated with Signal.

## How to read this

Each claim has one of three marks.

- **Tested.** We ran it and looked at the result. The test is named.
- **In the code.** The named function does it. You can read it. No test runs it yet.
- **Relied on.** It is a property of signal-cli, libsignal or macOS. We did not check it ourselves.

## The short answer

| | Signal Desktop | NCCd 0.3.23 |
| --- | --- | --- |
| End-to-end encryption | libsignal | libsignal, through signal-cli |
| Servers in the path | Signal's | Signal's, no others |
| How it joins your account | linked device | linked device |
| Message history on disk | encrypted, key in the Keychain | encrypted, key in the Keychain |
| Which chats it keeps | all of the account | the club's groups only |
| Disappearing messages | honored | honored |
| Delete for everyone | honored | honored |
| Pictures and files on disk | encrypted, a key for each file | **not encrypted**, folder for your account only |
| Signal keys on disk | in the encrypted database | **not encrypted**, folder for your account only |
| Safety number changes | shown | **not shown** |
| Made by | Signal | one club member, on an unofficial engine |

NCCd matches Signal Desktop on the wire and for the text of the chats. It is
behind for attachments, for the key store and for safety numbers. It keeps
less than Desktop does, and that is the one place where it is ahead today.

## 1. What NCCd is to Signal

NCCd is a linked device, as Signal Desktop is. It does not implement the
Signal protocol. It starts one `signal-cli` process and talks to it over
standard input and output. `signal-cli` uses libsignal, Signal's own library,
for all of the encryption.

`Sources/SignalEngine.swift`:

```swift
p.arguments = ["--config", Config.shared.signalConfigDir.path, "-a", account.number, "jsonRpc", "--ignore-stories"]
```

The link is made with `signal-cli link`. It prints an address, NCCd draws the
address as a code, and you scan the code with your phone.

```swift
p.arguments = ["--config", dir.path, "link", "-n", deviceName]
```

- **In the code.** NCCd opens no socket to Signal itself. Every message goes through the `signal-cli` child.
- **In the code.** NCCd keeps its own signal-cli folder, so it has its own device link. It does not read or change Signal Desktop's keys.
- **Relied on.** signal-cli encrypts and decrypts correctly with libsignal. Signal does not make, review or support signal-cli.
- **Relied on.** You can remove NCCd from your phone at any time: Signal, Settings, Linked devices.

Edge cases:

- A linked device receives every message of your account, not only the club's. Section 2 says what NCCd does with the others.
- signal-cli is a Java program that Homebrew installs. Your trust in the engine is your trust in the signal-cli project and in Homebrew.
- signal-cli can be late to follow a change in Signal's protocol. When that happens it stops working until it is updated. It does not fall back to something weaker.
- When a contact's safety number changes, Signal Desktop tells you. NCCd does not show this.

## 2. What NCCd takes in

Every message comes to one function. A message that is not in one of the
club's groups is dropped, and the files that the engine saved for it are
deleted.

`Sources/App.swift`, `ingest`:

```swift
guard let gid = (dm["groupInfo"] as? [String: Any])?["groupId"] as? String,
      let i = circles.firstIndex(where: { $0.conf.groupId == gid }),
      let ts = (dm["timestamp"] as? NSNumber)?.int64Value else { return discard(dm) }
```

`discard`:

```swift
for id in ids where !id.contains("/") { try? FileManager.default.removeItem(at: dir.appendingPathComponent(id)) }
```

- **In the code.** Your direct messages and your other groups are never shown, logged or searched.
- **In the code.** Pictures and files from those other chats are deleted as they arrive.
- **In the code.** Stories are not received.

Edge cases:

- The delete of these files has no test yet.
- signal-cli writes an attachment to disk before NCCd sees the message. For a moment the file is there in plain form.
- If you point NCCd at a signal-cli folder that other tools share (`signalConfigDir` in the config), NCCd deletes nothing in it. The other tools own those files.
- Contact names and group names of the whole account are in signal-cli's own store.

## 3. The chat log

signal-cli keeps no history, so NCCd keeps its own: one file for each circle,
one record on each line. Each line is sealed with AES-256-GCM before it is
written. The key is in your login Keychain. Signal Desktop does the same with
its database key.

`Sources/Store.swift`, `Vault`:

```swift
static func seal(_ plain: Data, for groupId: String) -> String? {
    guard let key, let box = try? AES.GCM.seal(plain, using: key, authenticating: Data(groupId.utf8)),
          let all = box.combined else { return nil }
    return mark + all.base64EncodedString()
}
```

The key is made once and is never written to a file:

```swift
guard status == errSecItemNotFound else { return nil }   // refused, or locked: never make a second key
let fresh = SymmetricKey(size: .bits256)
```

- **Tested.** We ran the app on a copy of six real logs. All 1,286 lines came out sealed, and no plain line was left. The app read them back, and the files did not change in six more launches.
- **Tested.** The log files are mode `0600` and the folders are `0700`.
- **In the code.** Each line has its own random nonce. The circle's id is bound into the seal, so a line that is moved to another log does not open.
- **In the code.** A changed line does not open. GCM refuses it.
- **In the code.** If the Keychain does not give the key, NCCd writes nothing. It never falls back to a plain log.
- **Relied on.** The Keychain gives the key to the app that made it and asks you before it gives the key to another program.

Edge cases:

- A program that runs as you, while NCCd is open, can read what NCCd has in memory. The same is true of Signal Desktop. The seal protects copies of the files: a backup, a synced folder, another account on the Mac, a program that only collects files.
- A log from before 0.3.23 is plain until 0.3.23 opens it once. Then it is sealed in place.
- If the Keychain item is deleted, the logs cannot be read. You can import the history again.
- The times of the last message you read are in the app's preferences, by group id. They hold no words.
- Search runs in memory over the opened log. NCCd makes no search index on disk.

## 4. Messages that go away

A message with a timer carries the time at which it ends. NCCd takes it out of
the log, with its pictures and files, at launch and once each minute.

`Sources/App.swift`, `ingest`:

```swift
if let life = (dm["expiresInSeconds"] as? NSNumber)?.int64Value, life > 0 { m.expires = ts + life * 1000 }
```

`Sources/Store.swift`:

```swift
func prune(now: Int64 = Int64(Date().timeIntervalSince1970 * 1000)) -> Bool {
    drop { ($0.expires ?? .max) <= now }
}
```

- **Tested.** A record past its time was gone from the file after one launch. A record with time left stayed.
- **In the code.** The timer runs from when the message was sent. Signal runs it from when you read the message. So NCCd is never later than Signal, and it is often earlier.
- **In the code.** A message that its sender deletes for everyone is taken out of the log.
- **In the code.** The history import carries each message's timer, and skips a message that is past it.

Edge cases:

- Delete for everyone and the once-a-minute sweep have no test yet.
- A message that was logged before 0.3.23 has no timer on its record. It stays. Import the history again to bring the timers in, or delete the log.
- If NCCd is closed when a timer ends, the message is on disk, sealed, until the next launch.

## 5. Pictures and files

This is where NCCd is behind Signal Desktop.

signal-cli writes each attachment as a plain file. NCCd shows it from there.
Signal Desktop encrypts each attachment with a key of its own.

- **Tested.** The app's folder and its media folder are mode `0700`.
- **In the code.** The files of a message that ends or is deleted are deleted with it.
- **Not done.** The files are not encrypted. A program that runs as you can read them, and a backup of your home folder holds them.

## 6. Signal's keys

This is the second place where NCCd is behind.

signal-cli keeps the account's keys in its own folder, in plain files and a
plain database. It has no setting to encrypt them. The folder is mode `0700`.
Signal Desktop keeps its keys inside its encrypted database.

- **Relied on.** signal-cli makes the folder for your account only.
- **Not done.** The keys are not wrapped by the Keychain. A program that runs as you can copy them. With them, it can read new messages of your account until you remove the device from your phone.

Full-disk encryption (FileVault) protects all of this when the Mac is off or
locked out. It does not protect it from a program that runs as you.

## 7. The club login, the wiki and the code

The club's own login page shows in a web view. You type your password into
that page. NCCd reads one thing from the web view, the wiki's session cookie.

`Sources/Login.swift`:

```swift
let c = cookies.first(where: { $0.name == "accessToken" && host.hasSuffix($0.domain.trimmingCharacters(in: ["."])) })
```

- **In the code.** NCCd never sees your password. It has no client of its own at the club's identity provider.
- **In the code.** The wiki session is kept in the login Keychain.
- **In the code.** NCCd sends the session only to the wiki's own host. A picture from any other host is asked for with no session.
- **In the code.** Sign out clears the web view's data and the session.

Edge cases:

- NCCd puts a stylesheet into the login page so that it looks like the app. The stylesheet changes colours and hides parts. It does not read the fields.
- A wiki page can hold a picture from another host. When you open the page, that host sees your address, as in a browser.
- The Code tab is a web view of the club's forge. It has the same session as the login.

## 8. History from Signal Desktop

A new link has no past. If Signal Desktop is on the Mac, NCCd can read the
club's groups from it, one time, from the menu. It runs `signal.mjs`, which
opens Signal Desktop's database and only reads it.

- **In the code.** macOS asks you before it gives the key ("Signal Safe Storage"). If you say no, nothing is imported.
- **In the code.** Only the club's groups are asked for, one at a time, by group id.
- **In the code.** The import only reads the database. It changes nothing in Signal Desktop.

Edge cases:

- If you choose Always Allow, the program that asked (node) can open that key later with no question. Choose Allow.
- The import writes the groups' pictures and files into NCCd's media folder as plain files. See section 5.

## 9. Notifications

A new message in a club chat shows as a macOS notification. It has the name
of the chat, the name of the sender and the words.

- **In the code.** Only club chats notify.

Edge case: the words show on a locked screen if macOS is set to show previews
there. Signal Desktop has a setting to hide them. NCCd does not have one yet.
Use System Settings, Notifications, NCCd, Show Previews.

## 10. Updates

NCCd reads a manifest and downloads a newer version. Three checks must pass
before it replaces anything.

`Sources/Updater.swift`:

```swift
guard sha == expected.lowercased() else { return say("update refused: the download is not the published file") }
...
guard Self.signedByThisTeam(fresh) else { return say("update refused: it is not signed by NCCd's developer") }
```

`signedByThisTeam` asks macOS to check the new bundle against a requirement:
Apple's chain, the same Developer ID team as the copy that is running, and the
same bundle id.

- **Tested.** Each release is notarized by Apple, and Gatekeeper accepts the app and the disk image (`spctl`, in `dist.sh`).
- **In the code.** A person who takes control of the download server cannot make NCCd install their program. They do not have the developer's signing key.
- **In the code.** The manifest and the download are asked for over `https` only.

Edge cases:

- The developer's signing key is the root of trust. A person who has it can ship an update to every copy.
- The server can hold back a new version, or offer an old one that is still newer than yours.
- You can build the app yourself and skip updates. A copy that runs from a build folder does not update itself.

## 11. Every place NCCd connects to

| Host | When | What goes |
| --- | --- | --- |
| Signal's servers | always, through signal-cli | your encrypted messages |
| `wiki.nela.computer` | when you sign into NCC | your session, the pages you open |
| `git.nela.computer` | when you open the Code tab | your session, as a browser |
| `assets.aesthetic.computer` | each half hour | a request for the manifest, nothing about you |
| `stream.kpbj.fm` | the radio, in the Netlabel tab | a request for the stream |
| other hosts | a picture on a wiki page | a request for the picture |

- **In the code.** There is no analytics, no crash report and no account at Aesthetic Computer.
- **In the code.** NCCd does not fetch a link that is in a message. A link preview shows what the sender attached. A click opens the link in your browser.
- **In the code.** NCCd sends text and mentions. It makes no link preview of its own.

The download page counts downloads. It keeps your address only as a keyed hash.

## 12. Where NCCd is ahead, or can be

Today:

- **It keeps less.** Signal Desktop keeps the whole account. NCCd keeps the club's groups and deletes the rest as it arrives.
- **It shows messages as text.** A message is never drawn as a web page. The two web views show the club's login and the club's forge only.
- **It is small.** About 6,400 lines of Swift in 14 files, with Apple's frameworks and no other library. A member can read all of it.
- **You can build it.** `./build.sh` makes the app from the source you read.

With more work:

- Seal each picture and file with the log's key. This closes section 5.
- Keep signal-cli's folder in an encrypted disk image, with its password in the Keychain. This closes section 6 for copies of the files.
- Ask for Touch ID before the Keychain gives the log's key. Signal Desktop does not do this.
- A setting to hide the words in notifications.
- Show safety number changes.
- Delete the log and its key on Unlink.

## 13. Check it yourself

```
# the log is sealed: every line starts with 1:
head -c 60 ~/Library/Application\ Support/NCC\ Circles/log/*.jsonl

# no plain line is left
grep -c '^{' ~/Library/Application\ Support/NCC\ Circles/log/*.jsonl

# the folders are yours only
ls -ld ~/Library/Application\ Support/NCC\ Circles ~/Library/Application\ Support/NCC\ Circles/log

# the key is in the Keychain
security find-generic-password -s computer.aesthetic.ncc-circles.log

# the app is signed and notarized
spctl -a -vv -t exec /Applications/NCCd.app

# the hosts in the code
grep -n 'https://' Sources/*.swift
```

## Report a problem

Open an issue at https://git.nela.computer/jeffrey/nccd, or write to Jeffrey
in the Infra circle. For a problem that puts members at risk, write to him
directly first.
