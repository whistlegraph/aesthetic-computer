#!/usr/bin/env python3
"""Install just the prox MCP release; preserve other daemons and client settings."""
import hashlib
import json
import os
from pathlib import Path
import plistlib
import shutil
import subprocess
import time
import urllib.request

REPO = Path(__file__).resolve().parents[2]
FILES = [
    "slab/bin/prox-mcp.mjs", "slab/bin/prox-inbox.mjs", "slab/bin/inbox-drain.mjs",
    "slab/lib/loopboy-binding.mjs", "slab/lib/loopboy-mode.mjs", "toolchain/mcp/http-front.mjs", "shared/toon.mjs",
]


INSTRUCTIONS = """<!-- slab-peer-inbox:start -->
## Slab peer communication

Use `prox_send` for agent-to-agent messages and `prox_receive` while waiting
for replies. `prox_inbox` supports clients with cached tool lists. Artifact
notifications use `prox_artifact_ready`, which queues an inbox message.
Never communicate by focusing another agent's window, synthesizing keys,
pasting, stuffing a PTY, or pressing Return in its terminal. `prox_wake` is a
deprecated inbox-only alias; direct `/wake` is retired. A queued receipt is
not a read acknowledgement. An idle agent receives on its next inbox tool
call or installed lifecycle hook; do not force it awake with user input.
<!-- slab-peer-inbox:end -->
"""


def install_instructions():
    for relative in [".codex/AGENTS.md", ".claude/CLAUDE.md"]:
        path = Path.home() / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        old = path.read_text() if path.exists() else ""
        start = old.find("<!-- slab-peer-inbox:start -->")
        end = old.find("<!-- slab-peer-inbox:end -->")
        if start >= 0 and end >= start:
            new = old[:start] + INSTRUCTIONS.rstrip() + old[end + len("<!-- slab-peer-inbox:end -->"):]
        else:
            new = old.rstrip() + "\n\n" + INSTRUCTIONS
        if old != new:
            temporary = path.with_name(path.name + ".slab-tmp")
            temporary.write_text(new)
            temporary.replace(path)


def main():
    digest = hashlib.sha256()
    for name in FILES:
        digest.update(name.encode())
        digest.update((REPO / name).read_bytes())
    release = Path.home() / ".local/share/slab/prox/releases" / ("inbox-only-" + digest.hexdigest()[:12])
    for name in FILES:
        dest = release / name
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(REPO / name, dest)
    plist = Path.home() / "Library/LaunchAgents/computer.aesthetic.prox-mcp.plist"
    original = plist.read_bytes()
    config = plistlib.loads(original)
    args = config["ProgramArguments"]
    # Refuse unfamiliar service layouts instead of replacing arbitrary arguments.
    index = next(i for i, arg in enumerate(args) if arg.endswith("/prox-mcp.mjs"))
    args[index] = str(release / "slab/bin/prox-mcp.mjs")
    subprocess.run([args[0], "--check", args[index]], check=True)
    label = config["Label"]
    domain = f"gui/{os.getuid()}"
    port = int(args[args.index("--http") + 1])
    backup = release / "previous-prox.plist"
    if not backup.exists():
        backup.write_bytes(original)
    temporary = plist.with_suffix(".plist.tmp")
    temporary.write_bytes(plistlib.dumps(config))
    temporary.replace(plist)
    subprocess.run(["launchctl", "bootout", f"{domain}/{label}"], capture_output=True)
    try:
        for attempt in range(20):
            result = subprocess.run(["launchctl", "bootstrap", domain, str(plist)], capture_output=True)
            if result.returncode == 0:
                break
            time.sleep(.2)
        else:
            raise RuntimeError(result.stderr.decode())
        request = urllib.request.Request(f"http://127.0.0.1:{port}/mcp", data=json.dumps({
            "jsonrpc": "2.0", "id": 1, "method": "tools/list", "params": {},
        }).encode(), headers={"Content-Type": "application/json"})
        for attempt in range(30):
            try:
                with urllib.request.urlopen(request, timeout=2) as response:
                    tools = {tool["name"]: tool for tool in json.load(response)["result"]["tools"]}
                assert "prox_receive" in tools
                assert "prox_loopboy_emit" in tools
                assert "loopboy" in tools["prox_launch"]["inputSchema"]["properties"]
                assert "inbox" in tools["prox_wake"]["description"]
                assert "Deprecated" in tools["prox_wake"]["description"]
                break
            except Exception:
                if attempt == 29:
                    raise
                time.sleep(.2)
    except Exception:
        temporary.write_bytes(original)
        temporary.replace(plist)
        subprocess.run(["launchctl", "bootout", f"{domain}/{label}"], capture_output=True)
        subprocess.run(["launchctl", "bootstrap", domain, str(plist)], capture_output=True)
        raise
    install_instructions()
    print(json.dumps({"ok": True, "release": str(release), "prox_receive": True, "wake": "inbox-only"}))


if __name__ == "__main__":
    main()
