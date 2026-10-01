#!/usr/bin/env python3
"""Deploy a committed, small guard bundle without pulling a host's dirty repo.

Usage: deploy-performance-guard.py --local neo panda chicken frisbee poorslice
Unreachable targets remain in a private receipt for --retry-pending.
"""
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import plistlib
import shlex
import subprocess
import sys
import tempfile
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
STATE = Path.home() / ".local/share/slab/performance-rollout"
LABEL = "computer.aesthetic.performance-guard-rollout"
FILES = ["performance-guard.sh", "performance_guard.py", "git-guard.sh", "swift-guard.sh"]

# Only this bounded installer is executed remotely; hostnames are SSH argv,
# never shell interpolation. The bundle has an exact filename allowlist.
INSTALL = r'''
import hashlib,json,os,pathlib,subprocess,sys,tempfile
bundle=json.load(sys.stdin)
allowed={'performance-guard.sh','performance_guard.py','git-guard.sh','swift-guard.sh','build-lock.sh','revision'}
if set(bundle['files'])!=allowed:raise ValueError('unexpected bundle files')
home=pathlib.Path.home()
env={**os.environ,'PATH':str(home/'.local/bin')+':/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'}
with tempfile.TemporaryDirectory(prefix='ac-guard-') as temp:
 for name,content in bundle['files'].items():
  p=pathlib.Path(temp)/name;p.write_text(content);p.chmod(0o700)
 flags=[]
 previous=home/'.local/bin/git'
 # Preserve the inspected portable-Git launcher used by older fleet setups.
 if previous.is_file() and not previous.is_symlink():
  text=previous.read_text()
  if 'export GIT_EXEC_PATH=' in text and 'export GIT_TEMPLATE_DIR=' in text and 'exec "$HOME/' in text:
   flags=['--preserve-git-wrapper']
 r=subprocess.run(['/bin/bash',temp+'/performance-guard.sh','--install',*flags],env=env,capture_output=True,text=True,timeout=25)
 if r.returncode:raise RuntimeError(r.stderr or r.stdout)
target=home/'.local/lib/ac-performance-guard'
hashes={name:hashlib.sha256((target/name).read_bytes()).hexdigest() for name in bundle['files']}
expected={name:hashlib.sha256(content.encode()).hexdigest() for name,content in bundle['files'].items()}
if hashes!=expected:raise RuntimeError('installed content mismatch')
guard=str(target/'performance-guard.sh')
r=subprocess.run(['/bin/bash',guard,'--once'],env=env,capture_output=True,text=True,timeout=20)
if r.returncode:raise RuntimeError(r.stderr or 'sampler failed')
state=home/'.local/share/slab/performance/latest.json'
import time
for attempt in range(20):
 try:
  sample=json.loads(state.read_text())
  if sample.get('revision')==bundle['revision']:break
 except (OSError,ValueError):pass
 time.sleep(0.25)
else:raise RuntimeError('no sample from deployed revision')
loaded=subprocess.run(['launchctl','print',f'gui/{os.getuid()}/computer.aesthetic.performance-guard'],capture_output=True).returncode==0
if not loaded:raise RuntimeError('launch agent not loaded')
# Check resolution in the user's actual login shell, not just installer PATH.
shell=os.environ.get('SHELL','/bin/zsh')
paths=subprocess.run([shell,'-lc','command -v git; command -v swift'],capture_output=True,text=True,timeout=8)
resolved=paths.stdout.strip().splitlines()
expected_paths=[str(home/'.local/bin/git'),str(home/'.local/bin/swift')]
if resolved!=expected_paths:raise RuntimeError('shell PATH bypasses guards: '+repr(resolved))
probe=subprocess.run(['/bin/bash',guard,'--admit','deployment verification'],env=env,capture_output=True,text=True,timeout=8)
if probe.returncode not in (0,75):raise RuntimeError('admission probe failed')
print(json.dumps({'revision':bundle['revision'],'loaded':loaded,'paths':resolved,'sample':{k:sample[k] for k in ['timestamp','version','revision','reason','disk_free_bytes']},'admission_exit':probe.returncode,'admission_message':probe.stderr.strip(),'verified_files':len(hashes)}))
'''


def save(path, data):
    temp = path.with_suffix(".next")
    temp.write_text(json.dumps(data, indent=2) + "\n")
    temp.chmod(0o600)
    os.replace(temp, path)


def deploy(host, bundle):
    command = [sys.executable, "-c", INSTALL] if host == "--local" else [
        "ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=5", host,
        "python3 -c " + shlex.quote(INSTALL)]
    try:
        result = subprocess.run(command, input=json.dumps(bundle), capture_output=True, text=True, timeout=65)
        if result.returncode:
            return {"host": host, "ok": False, "error": result.stderr[-1800:] or result.stdout[-1800:]}
        return {"host": host, "ok": True, **json.loads(result.stdout)}
    except (OSError, ValueError, subprocess.TimeoutExpired) as error:
        return {"host": host, "ok": False, "error": str(error)}


def schedule_retry():
    # Pin both the bundle and retry script. A future repository edit cannot
    # silently change the already-authorized rollout.
    script = STATE / "deploy-performance-guard.py"
    if Path(__file__).resolve() != script:
        script.write_text(Path(__file__).read_text())
        script.chmod(0o700)
    plist = Path.home() / "Library/LaunchAgents" / (LABEL + ".plist")
    plist.parent.mkdir(parents=True, exist_ok=True)
    config = {"Label": LABEL, "ProgramArguments": [sys.executable, str(script), "--retry-pending"],
              "StartInterval": 300, "ProcessType": "Background", "LowPriorityIO": True,
              "EnvironmentVariables": {"PATH": f"{Path.home()}/.local/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"},
              "StandardOutPath": str(STATE / "retry.out"), "StandardErrorPath": str(STATE / "retry.err")}
    plist.write_bytes(plistlib.dumps(config))
    subprocess.run(["launchctl", "bootout", f"gui/{os.getuid()}/{LABEL}"], capture_output=True)
    subprocess.run(["launchctl", "bootstrap", f"gui/{os.getuid()}", str(plist)], check=True)


def main(args):
    STATE.mkdir(parents=True, exist_ok=True)
    os.chmod(STATE, 0o700)
    receipt_path = STATE / "receipt.json"
    retry = args == ["--retry-pending"]
    if retry:
        receipt = json.loads(receipt_path.read_text())
        hosts = receipt["pending"]
        if not hosts or time.time() > receipt["retry_until"]:
            (Path.home() / "Library/LaunchAgents" / (LABEL + ".plist")).unlink(missing_ok=True)
            subprocess.run(["launchctl", "bootout", f"gui/{os.getuid()}/{LABEL}"], capture_output=True)
            return 0
        bundle = json.loads((STATE / "bundle.json").read_text())
    else:
        hosts = args
        if not hosts or any(h != "--local" and (h.startswith("-") or not all(c.isalnum() or c in "@._-" for c in h)) for h in hosts):
            raise SystemExit("usage: deploy-performance-guard.py [--local] HOST ... | --retry-pending")
        revision = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", "HEAD"], text=True).strip()
        paths = ["toolchain/macos/" + name for name in FILES] + ["slab/bin/build-lock.sh"]
        # git show selects the exact committed release, even in a dirty checkout.
        contents = {Path(path).name: subprocess.check_output(["git", "-C", str(ROOT), "show", revision + ":" + path], text=True) for path in paths}
        contents["revision"] = revision + "\n"
        bundle = {"revision": revision, "files": contents}
        save(STATE / "bundle.json", bundle)
        receipt = {"revision": revision, "results": {}, "pending": hosts, "retry_until": time.time() + 86400}
    # Small script transfers only, at most three hosts concurrently.
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        results = list(pool.map(lambda host: deploy(host, bundle), hosts))
    for result in results:
        receipt["results"][result["host"]] = result
        print(json.dumps(result), flush=True)
    receipt["pending"] = [h for h, r in receipt["results"].items() if not r["ok"]]
    receipt["updated_at"] = time.time()
    save(receipt_path, receipt)
    if receipt["pending"] and not retry:
        schedule_retry()
    return 0 if not receipt["pending"] else 75


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
