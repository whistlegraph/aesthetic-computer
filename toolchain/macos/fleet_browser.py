#!/usr/bin/python3
"""Own the lifecycle of a fleet Mac's ordinary and CDP Chrome sessions.

`open` is the default: it opens the host's persistent Chrome profile. CDP
consumers call `acquire` with an owner id and share Chrome's consented
DevToolsActivePort for that same profile. The final `release` drops access but
never closes the user's browser. `reap` expires abandoned access owners.
"""

from __future__ import annotations

import argparse
import contextlib
import datetime as dt
import http.client
import json
import os
import plistlib
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path


VERSION = 1
CHROME_DEFAULT = Path("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
OWNER_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:@/-]{0,126}$")
PORT_RE = re.compile(r"--remote-debugging-port(?:=|\s+)(\d+)")
PROFILE_RE = re.compile(r"--user-data-dir(?:=|\s+)(.*?)(?=\s+--[A-Za-z]|$)")
MAIN_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
_LAUNCHED_CHILDREN: dict[int, subprocess.Popen] = {}


class FleetBrowserError(RuntimeError):
    pass


def now_epoch() -> float:
    return time.time()


def iso(epoch: float | None = None) -> str:
    value = epoch if epoch is not None else now_epoch()
    return dt.datetime.fromtimestamp(value, dt.timezone.utc).isoformat().replace("+00:00", "Z")


def expand(value: str) -> Path:
    return Path(os.path.expandvars(os.path.expanduser(value))).resolve()


def default_state_dir() -> Path:
    return expand(os.environ.get("SLAB_BROWSER_STATE_DIR", "~/.local/share/slab/fleet-browser"))


def default_chrome_root() -> Path:
    return expand(
        os.environ.get(
            "SLAB_BROWSER_CHROME_PROFILE",
            "~/Library/Application Support/Google/Chrome",
        )
    )


def default_chrome() -> Path:
    return expand(os.environ.get("SLAB_BROWSER_CHROME", str(CHROME_DEFAULT)))


def process_marker() -> str:
    return os.environ.get("SLAB_BROWSER_PROCESS_MARKER", MAIN_CHROME)


def state_path() -> Path:
    return default_state_dir() / "lease.json"


def lock_path() -> Path:
    return default_state_dir() / "lease.lock"


def emit(payload: dict, as_json: bool) -> None:
    if as_json:
        print(json.dumps(payload, indent=2, sort_keys=True))
        return
    if payload.get("message"):
        print(payload["message"])
    else:
        print(json.dumps(payload, indent=2, sort_keys=True))


def validate_owner(owner: str) -> str:
    if not OWNER_RE.fullmatch(owner or ""):
        raise FleetBrowserError(
            "owner must be 1-127 safe identifier characters: letters, digits, . _ : @ / -"
        )
    return owner


@contextlib.contextmanager
def lease_lock(timeout: float = 8.0):
    directory = default_state_dir()
    directory.mkdir(parents=True, exist_ok=True)
    lock = lock_path()
    deadline = time.monotonic() + timeout
    while True:
        try:
            lock.mkdir(mode=0o700)
            break
        except FileExistsError:
            try:
                if now_epoch() - lock.stat().st_mtime > 30:
                    lock.rmdir()
                    continue
            except FileNotFoundError:
                continue
            if time.monotonic() >= deadline:
                raise FleetBrowserError(f"timed out waiting for lease lock: {lock}")
            time.sleep(0.05)
    try:
        yield
    finally:
        try:
            lock.rmdir()
        except FileNotFoundError:
            pass


def read_state() -> dict | None:
    try:
        value = json.loads(state_path().read_text())
    except FileNotFoundError:
        return None
    except (OSError, json.JSONDecodeError) as error:
        raise FleetBrowserError(f"invalid lease state: {error}") from error
    if value.get("version") != VERSION:
        raise FleetBrowserError(f"unsupported lease state version: {value.get('version')}")
    return value


def write_state(state: dict) -> None:
    directory = default_state_dir()
    directory.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(prefix="lease-", suffix=".json", dir=directory)
    try:
        with os.fdopen(fd, "w") as handle:
            json.dump(state, handle, indent=2, sort_keys=True)
            handle.write("\n")
        os.chmod(temporary, 0o600)
        os.replace(temporary, state_path())
    finally:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass


def clear_state() -> None:
    try:
        state_path().unlink()
    except FileNotFoundError:
        pass


def process_command(pid: int) -> str:
    result = subprocess.run(
        ["/bin/ps", "-p", str(pid), "-o", "command="],
        check=False,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip() if result.returncode == 0 else ""


def process_alive(pid: int) -> bool:
    if pid <= 0:
        return False
    child = _LAUNCHED_CHILDREN.get(pid)
    if child is not None:
        if child.poll() is not None:
            _LAUNCHED_CHILDREN.pop(pid, None)
            return False
        return True
    try:
        os.kill(pid, 0)
    except (ProcessLookupError, PermissionError):
        return False
    result = subprocess.run(
        ["/bin/ps", "-p", str(pid), "-o", "state="],
        check=False,
        capture_output=True,
        text=True,
    )
    return result.returncode == 0 and not result.stdout.strip().startswith("Z")


def command_is_browser(command: str) -> bool:
    marker = process_marker()
    marker_at_exec = command == marker or command.startswith(f"{marker} ")
    marker_after_interpreter = re.match(rf"^\S+\s+{re.escape(marker)}(?:\s|$)", command)
    return bool(marker_at_exec or marker_after_interpreter) and "Google Chrome Helper" not in command


def parse_chrome_processes(output: str) -> list[dict]:
    rows = []
    for raw in output.splitlines():
        match = re.match(r"\s*(\d+)\s+(.*)$", raw)
        if not match:
            continue
        pid = int(match.group(1))
        command = match.group(2)
        if not command_is_browser(command):
            continue
        port_match = PORT_RE.search(command)
        profile_match = PROFILE_RE.search(command)
        profile = profile_match.group(1).strip().strip("\"'") if profile_match else None
        rows.append(
            {
                "pid": pid,
                "command": command,
                "remoteDebuggingPort": int(port_match.group(1)) if port_match else None,
                "userDataDir": profile,
            }
        )
    return rows


def chrome_processes() -> list[dict]:
    result = subprocess.run(
        ["/bin/ps", "axww", "-o", "pid=,command="],
        check=True,
        capture_output=True,
        text=True,
    )
    return parse_chrome_processes(result.stdout)


def process_elapsed_seconds(pid: int) -> int | None:
    result = subprocess.run(
        ["/bin/ps", "-p", str(pid), "-o", "etime="],
        check=False,
        capture_output=True,
        text=True,
    )
    value = result.stdout.strip()
    if result.returncode != 0 or not value:
        return None
    days = 0
    if "-" in value:
        day_text, value = value.split("-", 1)
        days = int(day_text)
    fields = [int(part) for part in value.split(":")]
    if len(fields) == 3:
        hours, minutes, seconds = fields
    elif len(fields) == 2:
        hours, (minutes, seconds) = 0, fields
    else:
        return None
    return days * 86400 + hours * 3600 + minutes * 60 + seconds


def process_has_established_tcp(pid: int) -> bool:
    lsof = shutil.which("lsof") or "/usr/sbin/lsof"
    result = subprocess.run(
        [lsof, "-nP", "-a", "-p", str(pid), "-iTCP"],
        check=False,
        capture_output=True,
        text=True,
    )
    return "(ESTABLISHED)" in result.stdout


def reap_unmanaged_debug(minutes: float | None, state: dict | None = None) -> list[dict]:
    if minutes is None or minutes <= 0:
        return []
    threshold = minutes * 60
    reaped = []
    for row in unmanaged_debug_browsers(state):
        profile = row.get("userDataDir") or ""
        try:
            temporary_profile = Path(profile).resolve().is_relative_to(Path("/tmp").resolve())
        except (OSError, ValueError):
            temporary_profile = False
        elapsed = process_elapsed_seconds(row["pid"])
        if not temporary_profile or elapsed is None or elapsed < threshold:
            continue
        if process_has_established_tcp(row["pid"]):
            continue
        try:
            os.kill(row["pid"], signal.SIGTERM)
        except ProcessLookupError:
            pass
        reaped.append({**row, "elapsedSeconds": elapsed})
    return reaped


def managed_process_valid(state: dict | None) -> bool:
    if not state:
        return False
    pid = int(state.get("pid") or 0)
    command = process_command(pid)
    if not command:
        return False
    if not command_is_browser(command):
        return False
    try:
        active = read_active_port(Path(state["profileRoot"]))
    except FleetBrowserError:
        return False
    return (
        active["port"] == int(state.get("port") or 0)
        and active["path"] == state.get("webSocketPath")
    )


def unmanaged_debug_browsers(state: dict | None = None) -> list[dict]:
    managed_pid = int(state.get("pid") or 0) if state and managed_process_valid(state) else 0
    return [
        row
        for row in chrome_processes()
        if row["remoteDebuggingPort"] is not None and row["pid"] != managed_pid
    ]


def profile_identity(root: Path, profile_directory: str) -> dict:
    local_state = root / "Local State"
    try:
        data = json.loads(local_state.read_text())
    except FileNotFoundError as error:
        raise FleetBrowserError(f"Chrome profile root is not initialized: {local_state}") from error
    except json.JSONDecodeError as error:
        raise FleetBrowserError(f"invalid Chrome Local State: {local_state}: {error}") from error
    entry = data.get("profile", {}).get("info_cache", {}).get(profile_directory)
    if not entry or not (root / profile_directory).is_dir():
        raise FleetBrowserError(f"Chrome profile does not exist: {root / profile_directory}")
    return {
        "profileDirectory": profile_directory,
        "name": entry.get("gaia_name") or entry.get("name") or "",
        "user": entry.get("user_name") or "",
        "enterpriseManaged": bool(entry.get("is_managed")),
    }


def read_active_port(root: Path) -> dict:
    path = root / "DevToolsActivePort"
    try:
        lines = [line.strip() for line in path.read_text().splitlines() if line.strip()]
    except FileNotFoundError as error:
        raise FleetBrowserError(
            "remote debugging is disabled in ordinary Chrome; open "
            "chrome://inspect/#remote-debugging and enable it once"
        ) from error
    if len(lines) < 2:
        raise FleetBrowserError(f"invalid DevToolsActivePort: {path}")
    try:
        port = int(lines[0])
    except ValueError as error:
        raise FleetBrowserError(f"invalid DevToolsActivePort port: {lines[0]}") from error
    if port < 1 or port > 65535 or not lines[1].startswith("/devtools/browser/"):
        raise FleetBrowserError(f"invalid DevToolsActivePort: {path}")
    return {"port": port, "path": lines[1], "file": str(path)}


def ordinary_browser(root: Path) -> dict | None:
    expected = str(root)
    candidates = [
        row
        for row in chrome_processes()
        if row["remoteDebuggingPort"] is None
        and (row["userDataDir"] == expected or row["userDataDir"] is None)
    ]
    return candidates[0] if candidates else None


def check_expected_user(identity: dict, expected_user: str | None) -> None:
    if expected_user and identity.get("user", "").casefold() != expected_user.casefold():
        raise FleetBrowserError(
            f"selected Chrome profile is {identity.get('user') or 'unsigned'}, expected {expected_user}"
        )


def endpoint_ready(port: int, timeout: float = 0.5) -> dict | None:
    connection = http.client.HTTPConnection("127.0.0.1", port, timeout=timeout)
    try:
        connection.request("GET", "/json/version")
        response = connection.getresponse()
        if response.status != 200:
            return None
        return json.loads(response.read())
    except (OSError, json.JSONDecodeError, http.client.HTTPException):
        return None
    finally:
        connection.close()


def wait_endpoint(port: int, seconds: float) -> dict | None:
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        version = endpoint_ready(port)
        if version:
            return version
        time.sleep(0.2)
    return None


def terminate_managed(state: dict, wait_seconds: float = 8.0) -> bool:
    if not state.get("ownsProcess", True):
        return True
    pid = int(state.get("pid") or 0)
    if not managed_process_valid(state):
        return True
    try:
        os.kill(pid, signal.SIGTERM)
    except ProcessLookupError:
        return True
    deadline = time.monotonic() + wait_seconds
    while time.monotonic() < deadline:
        if not process_alive(pid):
            return True
        time.sleep(0.2)
    return not process_alive(pid)


def reconcile_state() -> dict | None:
    state = read_state()
    if state and not managed_process_valid(state):
        clear_state()
        return None
    return state


def command_open(args: argparse.Namespace) -> dict:
    chrome = default_chrome()
    if not chrome.is_file():
        raise FleetBrowserError(f"Google Chrome not found: {chrome}")
    root = default_chrome_root()
    identity = profile_identity(root, args.profile_directory)
    check_expected_user(identity, args.expected_user)
    chrome_args = [
        str(chrome),
        f"--user-data-dir={root}",
        f"--profile-directory={args.profile_directory}",
    ]
    if args.new_window:
        chrome_args.append("--new-window")
    chrome_args.append(args.url)
    subprocess.Popen(
        chrome_args,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    return {
        "ok": True,
        "mode": "ordinary",
        "profileRoot": str(root),
        "profile": identity,
        "url": args.url,
        "message": f"opened ordinary Chrome as {identity.get('name') or identity.get('user')}",
    }


def command_acquire(args: argparse.Namespace) -> dict:
    owner = validate_owner(args.owner)
    profile_root = expand(args.profile_root)
    identity = profile_identity(profile_root, args.profile_directory)
    check_expected_user(identity, args.expected_user)

    with lease_lock():
        state = reconcile_state()
        if state:
            if state["profileRoot"] != str(profile_root):
                raise FleetBrowserError(
                    "a browser access lease already owns a different profile; release it first"
                )
            touched = now_epoch()
            prior = state.setdefault("owners", {}).get(owner, {})
            state["owners"][owner] = {
                "acquiredAt": prior.get("acquiredAt", touched),
                "acquiredAtIso": prior.get("acquiredAtIso", iso(touched)),
                "lastTouchedAt": touched,
                "lastTouchedAtIso": iso(touched),
            }
            state["lastTouchedAt"] = touched
            state["lastTouchedAtIso"] = iso(touched)
            write_state(state)
            return {
                "ok": True,
                "reused": True,
                "pid": state["pid"],
                "port": state["port"],
                "webSocketPath": state["webSocketPath"],
                "owners": sorted(state["owners"]),
                "profile": state["profile"],
                "message": f"reused ordinary Chrome access for {owner}",
            }

        unmanaged = unmanaged_debug_browsers()
        if unmanaged:
            summary = ", ".join(
                f"pid {row['pid']} port {row['remoteDebuggingPort']} profile {row['userDataDir']}"
                for row in unmanaged
            )
            raise FleetBrowserError(
                f"refusing a competing debug Chrome; release or close unmanaged instance(s): {summary}"
            )

        browser = ordinary_browser(profile_root)
        if not browser:
            raise FleetBrowserError(
                "ordinary Chrome is not running; use fleet-browser open, then acquire access"
            )
        active = read_active_port(profile_root)
        acquired = now_epoch()
        state = {
            "version": VERSION,
            "mode": "ordinary-profile-access",
            "ownsProcess": False,
            "pid": browser["pid"],
            "port": active["port"],
            "webSocketPath": active["path"],
            "profileRoot": str(profile_root),
            "profileDirectory": args.profile_directory,
            "profile": identity,
            "startedAt": acquired,
            "startedAtIso": iso(acquired),
            "lastTouchedAt": acquired,
            "lastTouchedAtIso": iso(acquired),
            "owners": {
                owner: {
                    "acquiredAt": acquired,
                    "acquiredAtIso": iso(acquired),
                    "lastTouchedAt": acquired,
                    "lastTouchedAtIso": iso(acquired),
                }
            },
        }
        write_state(state)
        return {
            "ok": True,
            "reused": False,
            "pid": browser["pid"],
            "port": active["port"],
            "webSocketPath": active["path"],
            "owners": [owner],
            "profile": identity,
            "message": f"acquired ordinary Chrome access for {owner}",
        }


def command_touch(args: argparse.Namespace) -> dict:
    owner = validate_owner(args.owner)
    with lease_lock():
        state = reconcile_state()
        if not state or owner not in state.get("owners", {}):
            raise FleetBrowserError(f"owner does not hold the browser lease: {owner}")
        touched = now_epoch()
        state["owners"][owner]["lastTouchedAt"] = touched
        state["owners"][owner]["lastTouchedAtIso"] = iso(touched)
        state["lastTouchedAt"] = touched
        state["lastTouchedAtIso"] = iso(touched)
        write_state(state)
        return {
            "ok": True,
            "owner": owner,
            "pid": state["pid"],
            "owners": sorted(state["owners"]),
            "message": f"touched ordinary Chrome access for {owner}",
        }


def command_release(args: argparse.Namespace) -> dict:
    owner = validate_owner(args.owner)
    with lease_lock():
        state = reconcile_state()
        if not state:
            return {"ok": True, "stopped": False, "message": "browser access is already released"}
        owners = state.setdefault("owners", {})
        owners.pop(owner, None)
        if owners:
            write_state(state)
            return {
                "ok": True,
                "stopped": False,
                "owners": sorted(owners),
                "message": f"released {owner}; ordinary Chrome access remains leased",
            }
        if not state.get("ownsProcess", True):
            clear_state()
            return {
                "ok": True,
                "stopped": False,
                "released": True,
                "pid": state["pid"],
                "message": "released ordinary Chrome access; browser remains open",
            }
        state["stoppingAtIso"] = iso()
        write_state(state)
        stopped = terminate_managed(state)
        if stopped:
            clear_state()
        return {
            "ok": stopped,
            "stopped": stopped,
            "pid": state["pid"],
            "message": "stopped managed Chrome" if stopped else "managed Chrome did not stop after SIGTERM",
        }


def command_reap(args: argparse.Namespace) -> dict:
    cutoff = now_epoch() - args.max_idle_minutes * 60
    with lease_lock():
        state = reconcile_state()
        reaped_unmanaged = reap_unmanaged_debug(args.unmanaged_idle_minutes, state)
        if not state:
            return {
                "ok": True,
                "stopped": False,
                "unmanaged": unmanaged_debug_browsers(),
                "reapedUnmanaged": reaped_unmanaged,
                "message": (
                    f"reaped {len(reaped_unmanaged)} unmanaged debug Chrome instance(s)"
                    if reaped_unmanaged
                    else "no browser access lease"
                ),
            }
        expired = []
        owners = state.setdefault("owners", {})
        for owner, record in list(owners.items()):
            if float(record.get("lastTouchedAt") or 0) < cutoff:
                expired.append(owner)
                del owners[owner]
        if owners:
            if expired:
                write_state(state)
            return {
                "ok": True,
                "stopped": False,
                "expired": expired,
                "owners": sorted(owners),
                "reapedUnmanaged": reaped_unmanaged,
                "message": "ordinary Chrome access remains leased",
            }
        if not state.get("ownsProcess", True):
            clear_state()
            return {
                "ok": True,
                "stopped": False,
                "released": True,
                "expired": expired,
                "reapedUnmanaged": reaped_unmanaged,
                "pid": state["pid"],
                "message": "expired ordinary Chrome access; browser remains open",
            }
        stopped = terminate_managed(state)
        if stopped:
            clear_state()
        return {
            "ok": stopped,
            "stopped": stopped,
            "expired": expired,
            "reapedUnmanaged": reaped_unmanaged,
            "pid": state["pid"],
            "message": "reaped managed Chrome" if stopped else "managed Chrome did not stop after SIGTERM",
        }


def command_status(_: argparse.Namespace) -> dict:
    with lease_lock():
        state = reconcile_state()
        unmanaged = unmanaged_debug_browsers(state)
        ordinary = [row for row in chrome_processes() if row["remoteDebuggingPort"] is None]
        return {
            "ok": True,
            "accessLease": state,
            "ordinary": ordinary,
            "unmanagedDebug": unmanaged,
            "message": (
                f"ordinary Chrome access leased by {', '.join(sorted(state['owners']))}"
                if state
                else "no browser access lease"
            ),
        }


def command_install_reaper(args: argparse.Namespace) -> dict:
    script = Path(__file__).resolve()
    if str(script).startswith("/tmp/"):
        raise FleetBrowserError("install the fleet-browser executable outside /tmp before installing its reaper")
    label = "computer.slab.fleet-browser-reaper"
    agents = Path.home() / "Library" / "LaunchAgents"
    agents.mkdir(parents=True, exist_ok=True)
    plist_path = agents / f"{label}.plist"
    logs = default_state_dir()
    logs.mkdir(parents=True, exist_ok=True)
    plist = {
        "Label": label,
        "ProgramArguments": [
            "/usr/bin/python3",
            str(script),
            "reap",
            "--max-idle-minutes",
            str(args.max_idle_minutes),
            "--unmanaged-idle-minutes",
            str(args.unmanaged_idle_minutes),
            "--json",
        ],
        "StartInterval": args.interval_seconds,
        "RunAtLoad": True,
        "StandardOutPath": str(logs / "reaper.log"),
        "StandardErrorPath": str(logs / "reaper.error.log"),
        "ProcessType": "Background",
    }
    with plist_path.open("wb") as handle:
        plistlib.dump(plist, handle, sort_keys=True)
    domain = f"gui/{os.getuid()}"
    subprocess.run(["/bin/launchctl", "bootout", domain, str(plist_path)], check=False, capture_output=True)
    result = subprocess.run(
        ["/bin/launchctl", "bootstrap", domain, str(plist_path)],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise FleetBrowserError(f"launchctl bootstrap failed: {result.stderr.strip()}")
    return {
        "ok": True,
        "plist": str(plist_path),
        "maxIdleMinutes": args.max_idle_minutes,
        "unmanagedIdleMinutes": args.unmanaged_idle_minutes,
        "intervalSeconds": args.interval_seconds,
        "message": f"installed {label}",
    }


def parser() -> argparse.ArgumentParser:
    root = argparse.ArgumentParser(prog="fleet-browser")
    root.add_argument("--json", action="store_true", help="emit JSON")
    commands = root.add_subparsers(dest="command", required=True)

    open_parser = commands.add_parser("open", help="open ordinary persistent Chrome (the default path)")
    open_parser.add_argument("--profile-directory", default="Profile 1")
    open_parser.add_argument("--expected-user", default=os.environ.get("SLAB_BROWSER_EXPECTED_USER"))
    open_parser.add_argument("--url", default="chrome://newtab")
    open_parser.add_argument("--new-window", action=argparse.BooleanOptionalAction, default=True)
    open_parser.set_defaults(handler=command_open)

    acquire = commands.add_parser("acquire", help="acquire CDP access to ordinary Chrome")
    acquire.add_argument("--owner", required=True)
    acquire.add_argument("--profile-root", default=str(default_chrome_root()))
    acquire.add_argument("--profile-directory", default="Profile 1")
    acquire.add_argument("--expected-user", default=os.environ.get("SLAB_BROWSER_EXPECTED_USER"))
    acquire.set_defaults(handler=command_acquire)

    touch = commands.add_parser("touch", help="renew one owner")
    touch.add_argument("--owner", required=True)
    touch.set_defaults(handler=command_touch)

    release = commands.add_parser("release", help="release one access owner")
    release.add_argument("--owner", required=True)
    release.set_defaults(handler=command_release)

    reap = commands.add_parser("reap", help="expire abandoned owners")
    reap.add_argument("--max-idle-minutes", type=float, default=30.0)
    reap.add_argument("--unmanaged-idle-minutes", type=float, default=30.0)
    reap.set_defaults(handler=command_reap)

    status = commands.add_parser("status", help="show ordinary Chrome and access leases")
    status.set_defaults(handler=command_status)

    install = commands.add_parser("install-reaper", help="install the per-user idle reaper")
    install.add_argument("--max-idle-minutes", type=float, default=30.0)
    install.add_argument("--unmanaged-idle-minutes", type=float, default=30.0)
    install.add_argument("--interval-seconds", type=int, default=300)
    install.set_defaults(handler=command_install_reaper)
    return root


def main(argv: list[str] | None = None) -> int:
    raw = list(sys.argv[1:] if argv is None else argv)
    json_requested = "--json" in raw
    raw = [item for item in raw if item != "--json"]
    args = parser().parse_args(raw)
    args.json = json_requested or args.json
    try:
        payload = args.handler(args)
        emit(payload, args.json)
        return 0 if payload.get("ok", True) else 1
    except FleetBrowserError as error:
        payload = {"ok": False, "error": str(error)}
        if args.json:
            print(json.dumps(payload, indent=2, sort_keys=True))
        else:
            print(f"fleet-browser: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
