#!/usr/bin/env python3
"""Local macOS pressure sampling and admission. Standard library only."""
import datetime
import fcntl
import json
import math
import os
from pathlib import Path
import plistlib
import re
import shlex
import shutil
import subprocess
import sys
import time

LABEL = "computer.aesthetic.performance-guard"
HERE = Path(__file__).resolve().parent
STATE = Path.home() / ".local/share/slab/performance"
FLOOR = 20 * 1024**3
INTERVAL = 30
MAX_LOG = 5 * 1024**2
VERSION = "2026-10-01.2"


def run(args, timeout=2):
    return subprocess.run(args, capture_output=True, text=True, timeout=timeout).stdout.strip()


def atomic(path, data):
    """Never replace valid state with an incomplete ENOSPC write."""
    temp = path.with_name(path.name + ".next")
    try:
        with temp.open("w", encoding="utf-8") as stream:
            os.chmod(temp, 0o600)
            stream.write(data)
        os.replace(temp, path)
    finally:
        temp.unlink(missing_ok=True)


def read_json(path):
    try:
        value = json.loads(path.read_text())
        return value if isinstance(value, dict) else {}
    except (OSError, ValueError):
        return {}


def previous_sample():
    value = read_json(STATE / "latest.json")
    for key in ("epoch", "last_alert", "breaches", "swapouts"):
        number = value.get(key, 0)
        if not isinstance(number, (int, float)) or not math.isfinite(number) or number < 0:
            return {}
    if not isinstance(value.get("reasons", []), list) or not all(isinstance(r, str) for r in value.get("reasons", [])):
        return {}
    return value


def existing_parent(path):
    path = Path(path).absolute()
    while not path.exists() and path != path.parent:
        path = path.parent
    return path


def metrics(destination=None):
    raw = run(["/usr/sbin/sysctl", "hw.logicalcpu", "kern.memorystatus_level",
               "vm.loadavg", "vm.swapusage", "kern.boottime"])
    values = dict(line.split(": ", 1) for line in raw.splitlines() if ": " in line)
    memory = values.get("kern.memorystatus_level", "")
    if not memory.isdigit():
        found = re.search(r"free percentage:\s*(\d+)%", run(["/usr/bin/memory_pressure", "-Q"]))
        memory = found.group(1) if found else ""
    cores = int(values["hw.logicalcpu"])
    free_pct = int(memory)
    load1 = float(values["vm.loadavg"].strip("{} ").split()[0])
    if cores < 1 or not 0 <= free_pct <= 100:
        raise ValueError("invalid host pressure measurements")
    disks = [shutil.disk_usage(Path.home()).free]
    if destination:
        disks.append(shutil.disk_usage(existing_parent(destination)).free)
    return {"cores": cores, "free_pct": free_pct, "load1": load1,
            "disk_free_bytes": min(disks), "disk_floor_bytes": FLOOR,
            "swapusage": values.get("vm.swapusage", "unknown"),
            "boot": values.get("kern.boottime", "unknown")}


def reasons_for(m):
    reasons = []
    if m["load1"] > m["cores"] * 1.5:
        reasons.append("load")
    if m["free_pct"] < 15:
        reasons.append("memory")
    if m["disk_free_bytes"] < FLOOR:
        reasons.append("disk")
    return reasons


def admit(operation, destination=None):
    if os.environ.get("AC_PERFORMANCE_ALLOW_PRESSURE") == "1":
        print("AC performance guard: explicit pressure override", file=sys.stderr)
        return 0
    try:
        m = metrics(destination)
        reasons = reasons_for(m)
        previous = previous_sample()
        # Keep the sampler's recent swap/display/session signals; stale files
        # never deny work indefinitely after a reboot or a disabled sampler.
        age = time.time() - previous.get("epoch", 0)
        if 0 <= age <= 90 and previous.get("boot") == m["boot"]:
            reasons += [r for r in previous.get("reasons", [])
                        if r not in ("disk", "load", "memory") and r not in reasons]
        if not reasons:
            return 0
        detail = (f"{'+'.join(reasons)} pressure; {m['disk_free_bytes']/1024**3:.1f} GiB free "
                  f"(20 GiB reserve), memory available {m['free_pct']}%, load {m['load1']:.2f}")
    except (OSError, ValueError, KeyError, TypeError, subprocess.TimeoutExpired) as error:
        detail = f"cannot verify host headroom ({error})"
    print(f"AC performance guard: deferred {operation}: {detail}. "
          "Wait for recovery or use a compute host. Exit 75; nothing launched.", file=sys.stderr)
    return 75


def worktree_destination(args, cwd):
    """Interpret Git global options, preserving the original argv for execution."""
    i = 0
    while i < len(args):
        arg = args[i]
        if arg in ("-C", "-c", "--git-dir", "--work-tree", "--namespace", "--config-env"):
            if i + 1 >= len(args):
                return None
            if arg == "-C" and args[i + 1]:
                cwd = os.path.abspath(os.path.join(cwd, args[i + 1]))
            i += 2
        elif arg.startswith("-C") and arg != "-C":
            cwd = os.path.abspath(os.path.join(cwd, arg[2:]))
            i += 1
        elif arg.startswith("-"):
            i += 1
        else:
            break
    if args[i:i + 2] != ["worktree", "add"]:
        return None
    if any(arg in ("-h", "--help") for arg in args[i + 2:]):
        return None
    i += 2
    while i < len(args):
        arg = args[i]
        if arg == "--":
            i += 1
            break
        if arg in ("-b", "-B", "--reason"):
            i += 2
        elif arg.startswith("-"):
            i += 1
        else:
            break
    return os.path.abspath(os.path.join(cwd, args[i])) if i < len(args) else None


def git_main(args):
    real_path = HERE / "real-git"
    real = real_path.read_text().strip() if real_path.exists() else "/usr/bin/git"
    destination = worktree_destination(args, os.getcwd())
    if destination:
        status = admit("git worktree add", destination)
        if status:
            return status
    os.execv(real, [real, *args])


def processes():
    rows = []
    for line in run(["/bin/ps", "-A", "-o", "pid=,ppid=,%cpu=,rss=,comm="]).splitlines():
        bits = line.split(None, 4)
        if len(bits) == 5:
            try:
                rows.append({"pid": int(bits[0]), "ppid": int(bits[1]),
                             "cpu": float(bits[2]), "rss_kib": int(bits[3]), "executable": bits[4]})
            except ValueError:
                pass
    return rows


def safe_git_command(command):
    """Keep command identity, not config values, messages, URLs, or inline code."""
    tokens = shlex.split(command)
    safe = []
    redact_next = False
    for token in tokens:
        if redact_next:
            safe.append("[redacted]")
            redact_next = False
        elif token in ("-c", "--config-env", "-m", "--message"):
            safe.append(token)
            redact_next = True
        elif token == "config":
            safe.extend(["config", "[arguments omitted]"])
            break
        elif "://" in token or "@" in token or re.search(r"(?i)(token|password|secret|authorization|credential|prompt)", token):
            safe.append("[redacted]")
        elif token.startswith(("-c", "--config-env=", "--message=", "-m")):
            safe.append("[redacted]")
        else:
            safe.append(token[:256])
    return shlex.join(safe)[:2048]


def incident_processes(rows):
    by_pid = {p["pid"]: p for p in rows}
    selected = {p["pid"]: dict(p) for p in sorted(rows, key=lambda p: p["rss_kib"], reverse=True)[:8]}
    for p in sorted(rows, key=lambda p: p["cpu"], reverse=True)[:8]:
        selected[p["pid"]] = dict(p)
    git = sorted([p for p in rows if Path(p["executable"]).name == "git"],
                 key=lambda p: p["rss_kib"], reverse=True)[:8]
    deadline = time.monotonic() + 4
    for p in git:
        item = selected.setdefault(p["pid"], dict(p))
        if time.monotonic() >= deadline:
            break
        try:
            # Check start time before and after to avoid attributing a reused PID.
            start = run(["/bin/ps", "-p", str(p["pid"]), "-o", "lstart="])
            command = run(["/bin/ps", "-ww", "-p", str(p["pid"]), "-o", "command="])
            cwd = run(["/usr/sbin/lsof", "-a", "-p", str(p["pid"]), "-d", "cwd", "-Fn"], timeout=0.4)
            if start and start == run(["/bin/ps", "-p", str(p["pid"]), "-o", "lstart="]):
                item.update(started=start, command=safe_git_command(command),
                            cwd=next((s[1:] for s in cwd.splitlines() if s.startswith("n")), None))
        except (OSError, ValueError, subprocess.TimeoutExpired):
            pass
    for p in list(selected.values()):
        chain, parent = [], p["ppid"]
        while parent in by_pid and len(chain) < 6:
            ancestor = by_pid[parent]
            chain.append({k: ancestor[k] for k in ("pid", "ppid", "executable")})
            parent = ancestor["ppid"]
        p["parents"] = chain
    return list(selected.values())


def notify(message):
    # argv-based AppleScript avoids interpolating commands into source.
    script = 'on run argv\ndisplay notification (item 1 of argv) with title "AC performance guard"\nend run'
    try:
        run(["/usr/bin/osascript", "-e", script, message], timeout=3)
    except (OSError, subprocess.TimeoutExpired):
        pass


def append_log(path, line):
    if path.exists() and path.stat().st_size > MAX_LOG:
        with path.open("rb") as stream:
            stream.seek(-1024**2, os.SEEK_END)
            tail = stream.read().split(b"\n", 1)[-1].decode(errors="replace")
        atomic(path, tail)
    with path.open("a") as stream:
        os.chmod(path, 0o600)
        stream.write(line + "\n")


def repair_caddy(rows):
    repo = os.environ.get("AC_REPO")
    if not repo:
        return 0
    matches = []
    for p in rows:
        if Path(p["executable"]).name != "caddy":
            continue
        try:
            argv = shlex.split(run(["/bin/ps", "-ww", "-p", str(p["pid"]), "-o", "command="]))
            cwd = run(["/usr/sbin/lsof", "-a", "-p", str(p["pid"]), "-d", "cwd", "-Fn"])
            if argv[1:] == ["run", "--config", "Caddyfile"] and "n" + repo + "/system" in cwd.splitlines():
                matches.append(p["pid"])
        except (OSError, ValueError, subprocess.TimeoutExpired):
            pass
    # SIGTERM only; never escalate against a potentially reused PID.
    for pid in sorted(matches)[:-1]:
        try:
            os.kill(pid, 15)
        except ProcessLookupError:
            pass
    return max(0, len(matches) - 1)


def sample(repair=False):
    STATE.mkdir(parents=True, exist_ok=True)
    with (STATE / "sample.lock").open("a") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return 0
        previous = previous_sample()
        m = metrics()
        revision_file = HERE / "revision"
        m.update(version=VERSION, revision=revision_file.read_text().strip() if revision_file.exists() else "source",
                 epoch=time.time(), timestamp=datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"))
        rows = processes()
        names = [Path(p["executable"]).name for p in rows]
        m.update(processes=len(rows), codex=names.count("codex"), node=names.count("node"),
                 swift_builds=names.count("swift-build"), caddy=names.count("caddy"))
        vm = run(["/usr/bin/vm_stat"])
        swaps = re.search(r"Swapouts:\s*(\d+)", vm)
        m["swapouts"] = int(swaps.group(1)) if swaps else 0
        elapsed = m["epoch"] - previous.get("epoch", 0)
        delta = max(0, m["swapouts"] - previous.get("swapouts", m["swapouts"]))
        if previous.get("boot") != m["boot"] or not 0 < elapsed <= 300:
            delta = 0
        m["swapout_pages_delta"] = delta
        reasons = reasons_for(m)
        if delta > 4096 * max(elapsed, 1) / INTERVAL:
            reasons.append("swap")
        if m["codex"] > 8:
            reasons.append("sessions")
        if m["swift_builds"] > 1:
            reasons.append("builds")
        display = sum(p["cpu"] for p in rows if Path(p["executable"]).name in ("Terminal", "WindowServer"))
        if display > 125:
            reasons.append("display")
        m["caddy_repaired"] = repair_caddy(rows) if repair else 0
        if m["caddy_repaired"]:
            reasons.append("caddy")
        m.update(pressure=int(bool(reasons)), reasons=reasons, reason="+".join(reasons) or "none")
        recent = 0 < elapsed <= 90 and previous.get("boot") == m["boot"]
        m["breaches"] = (previous.get("breaches", 0) + 1 if recent else 1) if reasons else 0
        m["last_alert"] = previous.get("last_alert", 0)
        if reasons:
            # Publish the admission signal before optional incident enrichment.
            atomic(STATE / "pressure-active", m["reason"] + "\n")
            critical = m["disk_free_bytes"] < 5 * 1024**3 or m["free_pct"] < 5
            if (critical or m["breaches"] >= 3) and m["epoch"] - m["last_alert"] >= 600:
                notify(f"{m['reason']} pressure: {m['disk_free_bytes']/1024**3:.1f} GiB disk free, memory {m['free_pct']}%.")
                m["last_alert"] = m["epoch"]
        else:
            (STATE / "pressure-active").unlink(missing_ok=True)
        atomic(STATE / "latest.json", json.dumps(m) + "\n")
        keys = ("version", "revision", "timestamp", "load1", "cores", "free_pct", "disk_free_bytes", "processes", "codex", "node",
                "swift_builds", "caddy", "caddy_repaired", "swapout_pages_delta", "pressure", "reason")
        text = "\n".join(f"{k}={m[k]}" for k in keys)
        atomic(STATE / "latest.txt", text + "\n")
        if reasons:
            append_log(STATE / "performance-guard.log", text.replace("\n", " "))
            append_log(STATE / "incidents.jsonl", json.dumps({**m, "process_details": incident_processes(rows)}))
        return 0


def ensure_shell_path():
    """macOS path_helper can reorder zsh PATH after .zshenv has run."""
    home = Path.home()
    shell = os.environ.get("SHELL", "/bin/zsh")
    expected = [str(home / ".local/bin/git"), str(home / ".local/bin/swift")]
    if run([shell, "-lc", "command -v git; command -v swift"], timeout=8).splitlines() == expected:
        return
    name = Path(shell).name
    if name == "zsh":
        profile = home / ".zprofile"
        block = '\n# AC performance guard PATH\nexport PATH="$HOME/.local/bin:$PATH"\n# End AC performance guard PATH\n'
    elif name == "bash":
        profile = next((home / p for p in (".bash_profile", ".bash_login", ".profile") if (home / p).exists()), home / ".profile")
        block = '\n# AC performance guard PATH\nexport PATH="$HOME/.local/bin:$PATH"\n# End AC performance guard PATH\n'
    elif name == "fish":
        profile = home / ".config/fish/conf.d/ac-performance-guard.fish"
        block = '\n# AC performance guard PATH\nfish_add_path --path --prepend --move "$HOME/.local/bin"\n# End AC performance guard PATH\n'
    else:
        raise RuntimeError(f"Add ~/.local/bin before system commands in {shell}")
    profile = profile.resolve()
    profile.parent.mkdir(parents=True, exist_ok=True)
    old = profile.read_text() if profile.exists() else ""
    atomic(profile, old.replace(block, "").rstrip() + "\n" + block)
    if run([shell, "-lc", "command -v git; command -v swift"], timeout=8).splitlines() != expected:
        raise RuntimeError(f"Shell still bypasses guards after updating {profile}")


def install(preserve_git=False):
    home = Path.home()
    target = home / ".local/lib/ac-performance-guard"
    bindir = home / ".local/bin"
    shims = {"git": "git-guard.sh", "swift": "swift-guard.sh", "ac-performance-guard": "performance-guard.sh"}
    existing_git = None
    for name, source in shims.items():
        link = bindir / name
        if link.exists() or link.is_symlink():
            resolved = link.resolve()
            if resolved != target / source and not (link.is_symlink() and resolved.name == source):
                if name == "git" and preserve_git and link.is_file() and not link.is_symlink():
                    existing_git = link
                else:
                    raise RuntimeError(f"Refusing to replace unrelated command: {link}")
    real_git_file = target / "real-git"
    real_git = real_git_file.read_text().strip() if real_git_file.exists() else shutil.which("git")
    if not real_git or Path(real_git).resolve() == (target / "git-guard.sh").resolve():
        raise RuntimeError("Cannot resolve underlying Git")
    for directory in (target, bindir, STATE, home / "Library/LaunchAgents"):
        directory.mkdir(parents=True, exist_ok=True)
    if existing_git:
        backup = bindir / "git.before-ac-guard"
        if backup.exists():
            raise RuntimeError(f"Git wrapper backup already exists: {backup}")
        shutil.copy2(existing_git, backup)
        real_git = str(backup)
    for name in ("performance-guard.sh", "performance_guard.py", "git-guard.sh", "swift-guard.sh", "build-lock.sh"):
        source = HERE / name
        if name == "build-lock.sh" and not source.exists():
            source = HERE.parent.parent / "slab/bin/build-lock.sh"
        if source.resolve() != (target / name).resolve():
            atomic(target / name, source.read_text())
        (target / name).chmod(0o755)
    atomic(real_git_file, real_git + "\n")
    if (HERE / "revision").exists() and HERE != target:
        atomic(target / "revision", (HERE / "revision").read_text())
    for name, source in shims.items():
        link = bindir / name
        replacement = bindir / (name + ".guard-next")
        replacement.unlink(missing_ok=True)
        replacement.symlink_to(target / source)
        os.replace(replacement, link)
    ensure_shell_path()
    repo = os.environ.get("AC_REPO", str(home / "aesthetic-computer"))
    atomic(target / "repo-path", repo + "\n")
    config = {"Label": LABEL, "ProgramArguments": ["/bin/bash", str(target / "performance-guard.sh"), "--once", "--repair"],
              "RunAtLoad": True, "StartInterval": INTERVAL, "ProcessType": "Background", "LowPriorityIO": True,
              "EnvironmentVariables": {"AC_REPO": repo, "PATH": f"{bindir}:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"},
              "StandardOutPath": str(STATE / "launchd.out"), "StandardErrorPath": str(STATE / "launchd.err")}
    plist = home / "Library/LaunchAgents" / (LABEL + ".plist")
    atomic(plist, plistlib.dumps(config).decode())
    subprocess.run(["launchctl", "bootout", f"gui/{os.getuid()}/{LABEL}"], capture_output=True)
    subprocess.run(["launchctl", "bootstrap", f"gui/{os.getuid()}", str(plist)], check=True)
    print(f"Installed {LABEL}, Git and Swift guards. Put {bindir} first on PATH.")


def main(args):
    action = args[0] if args else "--once"
    if action == "--git":
        return git_main(args[1:])
    if action == "--admit":
        return admit(args[1] if len(args) > 1 else "work", args[2] if len(args) > 2 else None)
    if action == "--install":
        install("--preserve-git-wrapper" in args)
    elif action == "--status":
        print((STATE / "latest.txt").read_text() if (STATE / "latest.txt").exists() else "no performance sample yet")
    elif action in ("--once", "--watch"):
        while True:
            try:
                sample("--repair" in args)
            except (OSError, ValueError, KeyError, TypeError, subprocess.TimeoutExpired) as error:
                print(f"AC performance guard sample failed: {error}", file=sys.stderr)
                try:
                    atomic(STATE / "pressure-active", "measurement-unavailable\n")
                except OSError:
                    pass
                if action == "--once":
                    return 1
            if action == "--once":
                break
            time.sleep(INTERVAL)
    elif action == "--uninstall":
        subprocess.run(["launchctl", "bootout", f"gui/{os.getuid()}/{LABEL}"], capture_output=True)
        (Path.home() / "Library/LaunchAgents" / (LABEL + ".plist")).unlink(missing_ok=True)
        for name in ("git", "swift", "ac-performance-guard"):
            link = Path.home() / ".local/bin" / name
            if link.is_symlink() and link.resolve().parent == HERE:
                link.unlink()
                backup = link.with_name("git.before-ac-guard")
                if name == "git" and backup.exists():
                    os.replace(backup, link)
        (STATE / "pressure-active").unlink(missing_ok=True)
    else:
        print("usage: performance-guard.sh [--once [--repair] | --watch | --status | --install | --uninstall | --admit OPERATION [PATH]]")
        return 0 if action in ("-h", "--help") else 2
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
