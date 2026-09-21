# Computer-use releases

Package committed Frame, Puppet, their dependency closure, and Captutor's Frame
integration with the tested Playwright dependency and native build identity:

```sh
node slab/computer-use-release/pack.mjs HEAD /tmp/computer-use-release \
  /path/to/tested/node_modules slab/menubar-swift/.build/release/slab-menubar-swift
```

Copy the directory to `~/.local/share/slab/computer-use/releases/<revision>` on
each Apple Silicon Mac. Back up `~/Applications/SlabMenubar.app` and its launch
agent before using `slab/menubar-swift/deploy-host.sh HOST` to install that same
prebuilt binary. This avoids remote builds and preserves each host's signing
identity. Check for input leases and active recordings before restarting it.

Then run the release's `slab/computer-use-release/install.mjs` with Node 22+
inside the logged-in user's account, followed by its `verify.mjs`. Installation
backs up changed files, configures Frame/Puppet launch agents and HTTP clients,
and updates the Frame module imported by Captutor. Existing Captutor copies are
updated only when they match the known predecessor; custom versions are reported
and preserved. Verification checks release hashes, native build UUID, MCP tools,
session isolation, native capture, Accessibility trust, and Captutor's imported Frame without sending
mouse/keyboard input or printing screen contents. It does not benchmark latency.

`~/.local/share/slab/computer-use/installed.json` records the release and backup.
For rollback, restore the saved native app and changed files (including launch
agents and client configs), remove newly introduced wrappers if no predecessor
exists, and restart those launch agents. Keep older release directories until
the new release is verified; never clear another controller's input lease.

September 21 rollout: installed on Blueberry, Neo, Chicken, Panda, and Poorslice.
Blueberry and Chicken passed capture and Accessibility checks. Neo and Panda
capture correctly but report no Accessibility trust, including after a restart;
their old and new code-signing requirements match. Enable SlabMenubar under
System Settings → Privacy & Security → Accessibility on those seats. Poorslice
has Screen Recording and Accessibility grants but is locked; unlock it before
the final capture check. The legacy `mac-mini` and `jeffrey-macbook` entries were
unreachable. Chicken's older Captutor predates the Frame integration and was
preserved; other existing Captutor copies received the in-process Frame client.
