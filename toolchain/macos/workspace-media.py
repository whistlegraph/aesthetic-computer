#!/usr/bin/env python3
"""Park ignored workspace media privately in Spaces, or restore it by directory.

Default is an inventory. park uploads and verifies every selected file before
removing any local copy. restore never overwrites a file. Source and sidecars stay.
"""
import argparse
import base64
import fcntl
from concurrent.futures import ThreadPoolExecutor
from contextlib import closing
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
ALLOWED = {"marketing/klokkentales/out/reels", "pop/sailor-song/src", "pop/samples",
           "tmp/ac-os-darsh-tv", "xbox/steam/store-page/assets/shots"}
MEDIA = {".mp3", ".mp4", ".wav", ".mov", ".m4a", ".ogg", ".oga", ".flac", ".aif", ".aiff", ".png", ".jpg", ".jpeg", ".webm"}
BUILD_ARCHIVES = {".gz", ".zip", ".vmlinuz"}
BUCKET = "assets-aesthetic-computer"
PREFIX = "private/workspace-media/v1"
STATE = Path.home() / ".local/share/ac-workspace-media"
CLIENTS = threading.local()


def hashes(path):
    sha, md5 = hashlib.sha256(), hashlib.md5()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            sha.update(chunk); md5.update(chunk)
    return sha.hexdigest(), md5.hexdigest()


def checked_path(relative, root=ROOT):
    parts = relative.split("/")
    if any(p in ("", ".", "..") for p in parts) or "\\" in relative:
        raise ValueError("Invalid relative media path")
    path = root.joinpath(*parts)
    if not path.resolve().is_relative_to(root.resolve()):
        raise ValueError("Media path escapes workspace")
    for p in [path, *path.parents]:
        if p == root: break
        if p.is_symlink(): raise ValueError("Symlinked media refused")
    return path


def aws(*args):
    # The AWS CLI's Python environment includes botocore. Reuse connections there
    # instead of starting two CLI processes per small sample; ordinary Python
    # keeps the CLI fallback and needs no additional installed dependencies.
    try:
        from botocore.session import get_session
        from botocore.config import Config
    except ImportError:
        try:
            from awscli.botocore.session import get_session
            from awscli.botocore.config import Config
        except ImportError:
            get_session = None
    if get_session is not None:
        if not hasattr(CLIENTS, "s3"):
            CLIENTS.s3 = get_session().create_client(
                "s3", endpoint_url="https://sfo3.digitaloceanspaces.com", region_name="sfo3",
                config=Config(connect_timeout=15, read_timeout=120,
                              retries={"mode": "standard", "max_attempts": 4}))
        operation = args[1]
        options = dict(zip(args[2::2], args[3::2]))
        common = {"Bucket": options["--bucket"], "Key": options["--key"]}
        try:
            if operation == "put-object":
                with open(options["--body"], "rb") as body:
                    return CLIENTS.s3.put_object(**common, Body=body, ACL="private",
                        ContentMD5=options["--content-md5"],
                        Metadata={"sha256": options["--metadata"].split("=", 1)[1]})
            if operation == "head-object":
                return CLIENTS.s3.head_object(**common)
            if operation == "get-object":
                response = CLIENTS.s3.get_object(**common)
                with closing(response["Body"]) as body, open(args[-1], "wb") as output:
                    shutil.copyfileobj(body, output)
                return {}
            raise ValueError("Unsupported media storage operation")
        except Exception:
            raise RuntimeError("Private media storage request failed; local files kept") from None
    result = subprocess.run([shutil.which("aws") or "/opt/homebrew/bin/aws", *args,
                             "--endpoint-url", "https://sfo3.digitaloceanspaces.com", "--region", "sfo3"],
                            capture_output=True, text=True, timeout=900)
    if result.returncode:
        raise RuntimeError("Private media storage request failed; local files kept")
    return json.loads(result.stdout) if result.stdout.strip() else {}


def upload(path, key, sha, md5):
    aws("s3api", "put-object", "--bucket", BUCKET, "--key", key, "--body", str(path),
        "--acl", "private", "--content-md5", base64.b64encode(bytes.fromhex(md5)).decode(),
        "--metadata", "sha256=" + sha)
    head = aws("s3api", "head-object", "--bucket", BUCKET, "--key", key)
    if (head.get("ETag", "").strip('"') != md5 or head.get("ContentLength") != path.stat().st_size
            or head.get("Metadata", {}).get("sha256") != sha):
        raise RuntimeError("Remote checksum/size mismatch; local files kept")


def opened_files():
    result = subprocess.run(["/usr/sbin/lsof", "-nP", "-u", str(os.getuid()), "-Fn"],
                            capture_output=True, text=True, timeout=30)
    if result.returncode or result.stderr.strip():
        raise RuntimeError("Cannot verify open files; local media kept")
    return result.stdout


def removable(path, record, opened):
    return (path.is_file() and not path.is_symlink() and str(path) not in opened
            and path.stat().st_mtime_ns == record["mtime_ns"]
            and hashes(path)[0] == record["sha256"])


def load_credentials():
    env = ROOT / "aesthetic-computer-vault/silo/.env"
    if env.exists():
        for line in env.read_text().splitlines():
            key, sep, value = line.partition("=")
            if sep and key in ("SPACES_KEY", "SPACES_SECRET", "DO_SPACES_KEY", "DO_SPACES_SECRET"):
                os.environ.setdefault(key, value.strip().strip('\"\''))
    for target, sources in [("AWS_ACCESS_KEY_ID", ["SPACES_KEY", "DO_SPACES_KEY"]),
                            ("AWS_SECRET_ACCESS_KEY", ["SPACES_SECRET", "DO_SPACES_SECRET"])]:
        for source in sources:
            if os.environ.get(source): os.environ.setdefault(target, os.environ[source])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["report", "park", "restore"])
    parser.add_argument("directory", choices=sorted(ALLOWED))
    parser.add_argument("--file", help="Restore one path relative to the selected directory")
    parser.add_argument("--workers", type=int, choices=range(1, 17), default=3,
                        help="Concurrent uploads (default: 3; SDK transport supports larger batches)")
    args = parser.parse_args()
    directory = checked_path(args.directory)
    STATE.mkdir(parents=True, exist_ok=True)
    if args.command != "report":
        lock = (STATE / "lock").open("a")
        try: fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError: raise SystemExit("Another media operation is running; try again after it finishes")
    manifest_path = STATE / (args.directory.replace("/", "--") + ".json")
    load_credentials()
    if args.command == "restore":
        if not manifest_path.exists():
            temporary_manifest = manifest_path.with_suffix(".download")
            aws("s3api", "get-object", "--bucket", BUCKET, "--key",
                PREFIX + "/manifests/" + manifest_path.name, str(temporary_manifest))
            json.loads(temporary_manifest.read_text())
            os.replace(temporary_manifest, manifest_path)
        manifest = json.loads(manifest_path.read_text())
        if args.file:
            relative = args.directory + "/" + args.file
            checked_path(relative)
            if relative not in manifest: raise SystemExit("File is absent from the restore manifest")
            manifest = {relative: manifest[relative]}
        for relative, record in manifest.items():
            if not relative.startswith(args.directory + "/"):
                raise ValueError("Manifest path outside selected directory")
            path = checked_path(relative)
            if path.exists(): continue
            path.parent.mkdir(parents=True, exist_ok=True)
            fd, name = tempfile.mkstemp(dir=path.parent, prefix=".restore-")
            os.close(fd)
            temporary = Path(name)
            try:
                aws("s3api", "get-object", "--bucket", BUCKET, "--key", record["key"], str(temporary))
                if hashes(temporary)[0] != record["sha256"]:
                    raise RuntimeError("Restored checksum mismatch")
                os.utime(temporary, ns=(record["mtime_ns"], record["mtime_ns"]))
                try: os.link(temporary, path)
                except FileExistsError: pass
            finally: temporary.unlink(missing_ok=True)
        print("Restored:", args.directory)
        return
    # Git decides ownership; untracked but unignored work is never parked.
    raw = subprocess.check_output(["git", "-C", str(ROOT), "ls-files", "--others", "--ignored",
                                   "--exclude-standard", "-z", "--", args.directory])
    opened = opened_files()
    files = [checked_path(p.decode()) for p in raw.split(b"\0") if p]
    extensions = MEDIA | BUILD_ARCHIVES if args.directory == "tmp/ac-os-darsh-tv" else MEDIA
    files = [p for p in files if p.suffix.lower() in extensions and p.is_file()
             and str(p) not in opened and time.time() - p.stat().st_mtime > 86400]
    print(f"{len(files)} idle ignored media files, {sum(p.stat().st_size for p in files):,} bytes", flush=True)
    if args.command == "report": return
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
    def park(path):
        mtime = path.stat().st_mtime_ns
        sha, md5 = hashes(path)
        key = PREFIX + "/objects/" + sha
        upload(path, key, sha, md5)
        print("Verified:", path.relative_to(ROOT), flush=True)
        return str(path.relative_to(ROOT)), dict(key=key, sha256=sha, mtime_ns=mtime)
    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        verified = dict(pool.map(park, files))
    manifest.update(verified)
    staging = manifest_path.with_suffix(".tmp")
    staging.write_text(json.dumps(manifest, indent=2))
    sha, md5 = hashes(staging)
    upload(staging, PREFIX + "/manifests/history/" + sha + ".json", sha, md5)
    upload(staging, PREFIX + "/manifests/" + manifest_path.name, sha, md5)
    os.replace(staging, manifest_path)  # durable restore map before any eviction
    opened = opened_files()
    freed = 0
    for relative, record in verified.items():
        path = checked_path(relative)
        if removable(path, record, opened):
            size = path.stat().st_size
            path.unlink()
            freed += size
    print(f"Parked {freed:,} bytes; restore with: python3 toolchain/macos/workspace-media.py restore {args.directory}")


if __name__ == "__main__":
    main()
