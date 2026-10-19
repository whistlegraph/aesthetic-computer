#!/usr/bin/env python3
"""Small Juke catalog and verified, bounded audio cache. No source/sidecar deletion."""
import argparse
import contextlib
from datetime import datetime
import fcntl
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import tempfile
import time


def atomic_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, name = tempfile.mkstemp(dir=path.parent, prefix=".juke-")
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(value, stream, indent=2)
        os.replace(name, path)
    finally:
        if os.path.exists(name):
            os.unlink(name)


def digest(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


class Cache:
    def __init__(self, pop=None, state=None, runner=subprocess.run):
        self.pop = Path(pop or Path(__file__).resolve().parents[1]).resolve()
        self.state = Path(state or Path.home() / ".local/share/ac-juke-cache")
        self.catalog_path = self.pop / "out/juke-remote.json"
        self.runner = runner
        self.aws = shutil.which("aws") or "/opt/homebrew/bin/aws"

    def path(self, key):
        parts = PurePosixPath(key).parts
        if (not key or key.startswith("/") or "\\" in key or
                any(x in ("", ".", "..") for x in key.split("/")) or
                len(parts) < 3 or parts[-2] != "out" or not parts[-1].endswith(".mp3")):
            raise ValueError("Expected <lane>/out/<track>.mp3")
        path = self.pop.joinpath(*parts)
        if not path.resolve().is_relative_to(self.pop):
            raise ValueError("Track escapes Pop")
        for parent in [path, *path.parents]:
            if parent == self.pop:
                break
            if parent.is_symlink():
                raise ValueError("Symlinked track path refused")
        return path

    def aws_call(self, *args):
        result = self.runner([self.aws, *args, "--endpoint-url",
                              "https://sfo3.digitaloceanspaces.com", "--region", "sfo3"],
                             capture_output=True, text=True, timeout=180)
        if result.returncode:
            raise RuntimeError("Juke storage request failed; local files kept")
        return result.stdout

    def read(self, path, default):
        return json.loads(path.read_text()) if path.exists() else default

    @contextlib.contextmanager
    def locked(self):
        self.state.mkdir(parents=True, exist_ok=True)
        with (self.state / "lock").open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            yield

    def catalog(self):
        data = json.loads(self.aws_call("s3api", "list-objects-v2", "--bucket",
                                       "shelf-sync", "--prefix", "juke/", "--output", "json"))
        tracks = []
        for obj in data.get("Contents", []):
            key = obj["Key"].removeprefix("juke/")
            try:
                path = self.path(key)
            except ValueError:
                continue
            tracks.append({"key": key, "path": str(path), "bytes": obj["Size"],
                           "etag": obj["ETag"], "updated": obj.get("LastModified")})
        atomic_json(self.catalog_path, {"version": 1, "tracks": tracks})
        return len(tracks)

    def entry(self, key):
        self.path(key)
        for entry in self.read(self.catalog_path, {"tracks": []})["tracks"]:
            if entry["key"] == key:
                return entry
        raise ValueError("Track is absent from the catalog; run juke-cache catalog")

    def fetch(self, key, pin=False):
        with self.locked():
            entry = self.entry(key)
            path = self.path(key)
            records = self.read(self.state / "files.json", {})
            record = records.get(key)
            # Existing renders belong to the author, until explicitly adopted.
            if path.exists():
                if record and record.get("sha256") == digest(path):
                    record["used"] = time.time()
                    record["pinned"] = pin or record.get("pinned", False)
                    atomic_json(self.state / "files.json", records)
                return path
            path.parent.mkdir(parents=True, exist_ok=True)
            fd, name = tempfile.mkstemp(dir=path.parent, prefix=".juke-download-")
            os.close(fd)
            temporary = Path(name)
            try:
                # If-Match binds the downloaded bytes to the catalog version.
                self.aws_call("s3api", "get-object", "--bucket", "shelf-sync",
                              "--key", "juke/" + key, "--if-match", entry["etag"], str(temporary))
                if temporary.stat().st_size != entry["bytes"]:
                    raise RuntimeError("Incomplete download; existing audio kept")
                sha = digest(temporary)
                if entry.get("updated"):
                    stamp = datetime.fromisoformat(entry["updated"].replace("Z", "+00:00")).timestamp()
                    os.utime(temporary, (stamp, stamp))
                # Do not overwrite a render created during the download.
                try:
                    os.link(temporary, path)
                except FileExistsError:
                    return path
                records[key] = {"sha256": sha, "etag": entry["etag"],
                                "bytes": entry["bytes"], "used": time.time(), "pinned": pin}
                atomic_json(self.state / "files.json", records)
            finally:
                temporary.unlink(missing_ok=True)
            return path

    def adopt(self):
        """Claim only existing downloads that match an S3 single-part MD5."""
        with self.locked():
            records = self.read(self.state / "files.json", {})
            count = 0
            for entry in self.read(self.catalog_path, {"tracks": []})["tracks"]:
                key = entry["key"]
                path = self.path(key)
                if key in records or not path.is_file() or path.stat().st_size != entry["bytes"]:
                    continue
                etag = entry["etag"].strip('"')
                if len(etag) != 32:  # multipart/unknown checksums stay local
                    continue
                md5 = hashlib.md5()
                sha = hashlib.sha256()
                with path.open("rb") as stream:
                    for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                        md5.update(chunk); sha.update(chunk)
                if md5.hexdigest() != etag:
                    continue
                records[key] = {"sha256": sha.hexdigest(), "etag": entry["etag"],
                                "bytes": entry["bytes"], "used": path.stat().st_mtime, "pinned": False}
                count += 1
            atomic_json(self.state / "files.json", records)
            return count

    def unpin(self, key):
        self.path(key)
        with self.locked():
            records = self.read(self.state / "files.json", {})
            if key in records:
                records[key]["pinned"] = False
                atomic_json(self.state / "files.json", records)

    def prune(self, budget=512 * 1024 * 1024, open_files=None, now=None):
        if budget < 0:
            raise ValueError("Cache budget must not be negative")
        if open_files is None:
            result = self.runner(["/usr/sbin/lsof", "-nP", "-u", str(os.getuid()), "-Fn"],
                                 capture_output=True, text=True, timeout=30)
            if result.returncode or result.stderr.strip():
                raise RuntimeError("Could not check open files; cache kept")
            open_files = result.stdout
        now = time.time() if now is None else now
        with self.locked():
            records = self.read(self.state / "files.json", {})
            entries = {x["key"]: x for x in self.read(self.catalog_path, {"tracks": []})["tracks"]}
            total = sum(self.path(k).stat().st_size for k in records if self.path(k).is_file())
        if total <= budget:
            return 0
        # A fresh listing verifies current remote sizes/ETags in one request,
        # rather than hundreds of per-file checks that stall a small machine.
        listing = json.loads(self.aws_call("s3api", "list-objects-v2", "--bucket", "shelf-sync",
                                           "--prefix", "juke/", "--output", "json"))
        remote = {x["Key"].removeprefix("juke/"): x for x in listing.get("Contents", [])}
        freed = 0
        for key, record in sorted(records.items(), key=lambda pair: pair[1].get("used", 0)):
            if total <= budget:
                break
            path = self.path(key)
            if (not path.is_file() or record.get("pinned") or
                    now - record.get("used", now) < 900 or str(path) in open_files):
                continue
            entry = entries.get(key)
            if not entry or entry["etag"] != record["etag"] or digest(path) != record["sha256"]:
                continue
            head = remote.get(key, {})
            if head.get("ETag") != record["etag"] or head.get("Size") != record["bytes"]:
                continue
            with self.locked():
                current = self.read(self.state / "files.json", {}).get(key)
                if current != record or not path.is_file() or digest(path) != record["sha256"]:
                    continue
                size = path.stat().st_size
                path.unlink()
                total -= size; freed += size
        return freed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["catalog", "fetch", "pin", "unpin", "adopt", "prune", "status"])
    parser.add_argument("key", nargs="?")
    parser.add_argument("--budget-mb", type=int, default=512)
    args = parser.parse_args()
    cache = Cache()
    if args.command in ("fetch", "pin", "unpin") and not args.key:
        parser.error("track key required")
    if args.command == "catalog": print("Catalog tracks:", cache.catalog())
    elif args.command in ("fetch", "pin"): print(cache.fetch(args.key, pin=args.command == "pin"))
    elif args.command == "unpin": cache.unpin(args.key)
    elif args.command == "adopt": print("Verified existing downloads:", cache.adopt())
    elif args.command == "prune": print("Reclaimed bytes:", cache.prune(args.budget_mb * 1024 * 1024))
    else:
        records = cache.read(cache.state / "files.json", {})
        resident = [cache.path(k) for k in records if cache.path(k).is_file()]
        print(json.dumps({"tracked": len(records), "cachedFiles": len(resident),
                          "cachedBytes": sum(p.stat().st_size for p in resident),
                          "pinned": [k for k, v in records.items() if v.get("pinned")]}))


if __name__ == "__main__":
    main()
