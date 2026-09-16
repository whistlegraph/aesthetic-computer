# ac-mail

A terminal interface to the same letters as [web /mail](https://aesthetic.computer/mail).
Uses the shared `ac-login` session (`~/.ac-token`) and its existing refresh helper.
No additional dependencies; requires Node 24 and an interactive ANSI terminal.

```sh
node toolchain/mail/install.mjs
ac-mail
```

If needed, run `ac-login` first. `ac-mail login` delegates to that same repository
login implementation; `ac-mail status` delegates to its status command. Nothing
launches a browser automatically. Ensure `~/.local/bin` is on your `PATH`.

The AC fish configuration reserves `ac-mail` for this interface. The older
Gmail/mu4e sync loop is now `ac-mail-sync`. In a terminal that still has the old
function loaded, run `command ac-mail` to open Letters immediately, or start a
new shell to load the updated names.

| View | Keys |
| --- | --- |
| Inbox / Sent | ↑↓ or j/k select, Enter open, Tab switch, c write, r refresh, q quit |
| Letter | ↑↓ or j/k scroll, c reply, Esc back |
| Compose | Tab / Shift+Tab change field, type to append, Backspace erase, Enter newline in body, Ctrl+S review, Esc keep draft |
| Review | y send, Esc edit, ↑↓ scroll |

Opening an unread letter marks it read through `/api/mail`, visible after refreshing
the web client. Drafts stay in memory across views; they disappear on exit. Quitting
with `q` asks before discarding a draft; Ctrl+C exits immediately. Editing is append
and backspace in this first version, not a full text editor.

The API currently returns the latest 50 inbox and 50 sent letters. The client
enforces its 500-character body and 80-character subject limits rather than letting
the server silently truncate a letter. Emoji can count as multiple characters under
these existing API limits. Outside email uses the same backend routing as web mail.

Sending requires review and an explicit `y`. A timeout or server error can mean
delivery succeeded but confirmation was lost: refresh Sent before trying again.
The client never automatically retries a send. A changed shared login requires
reopening the client so a draft is not silently sent from another account.

No message cache, draft files, analytics, or raw error logging. The interface uses
the terminal's alternate screen and restores normal terminal mode on exit. Terminal
recording tools and local screen access can still see displayed letters. Messages
remain readable on the server; this client does not implement encryption.

```sh
ac-mail --demo                         # Synthetic mailbox; no login/network
node --test toolchain/mail/client.test.mjs
python3 toolchain/mail/tui.test.py      # Real PTY interaction test (macOS/Linux)
```
