import importlib.util
import hashlib
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest

spec = importlib.util.spec_from_file_location("juke_cache", Path(__file__).parents[1] / "bin/juke-cache.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class CacheTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.audio = b"verified remote audio"
        self.key = "test/out/song.mp3"
        self.etag = '"' + hashlib.md5(self.audio).hexdigest() + '"'
        self.fail = False
        self.cache = module.Cache(self.root / "pop", self.root / "state", self.aws)
        module.atomic_json(self.cache.catalog_path, {"tracks": [{"key": self.key,
            "bytes": len(self.audio), "etag": self.etag}]})

    def aws(self, args, **kwargs):
        if self.fail:
            return SimpleNamespace(returncode=1, stdout="", stderr="offline")
        if "get-object" in args:
            Path(args[args.index("--if-match") + 2]).write_bytes(self.audio)
            out = "{}"
        elif "list-objects-v2" in args:
            out = json.dumps({"Contents": [{"Key": "juke/" + self.key,
                                            "ETag": self.etag, "Size": len(self.audio)}]})
        else:
            out = json.dumps({"ETag": self.etag, "ContentLength": len(self.audio)})
        return SimpleNamespace(returncode=0, stdout=out, stderr="")

    def test_fetch_and_evict_keep_notes_and_catalog(self):
        path = self.cache.fetch(self.key)
        sidecar = path.with_suffix(".juke.json")
        sidecar.write_text('{"notes":"keep me"}')
        self.assertEqual(path.read_bytes(), self.audio)
        freed = self.cache.prune(0, open_files="", now=10**12)
        self.assertEqual(freed, len(self.audio))
        self.assertFalse(path.exists())
        self.assertTrue(sidecar.exists())
        self.assertEqual(self.cache.fetch(self.key).read_bytes(), self.audio)

    def test_pins_open_files_and_recent_downloads_survive(self):
        path = self.cache.fetch(self.key, pin=True)
        self.assertEqual(self.cache.prune(0, open_files="", now=10**12), 0)
        self.cache.unpin(self.key)
        self.assertEqual(self.cache.prune(0, open_files=str(path), now=10**12), 0)
        self.assertEqual(self.cache.prune(0, open_files=""), 0)

    def test_modified_download_is_never_deleted(self):
        path = self.cache.fetch(self.key)
        path.write_bytes(b"new original render")
        self.assertEqual(self.cache.prune(0, open_files="", now=10**12), 0)
        self.assertEqual(path.read_bytes(), b"new original render")

    def test_original_file_is_not_claimed_by_fetch(self):
        path = self.cache.path(self.key)
        path.parent.mkdir(parents=True)
        path.write_bytes(b"author original")
        self.cache.fetch(self.key)
        self.assertEqual(self.cache.prune(0, open_files="", now=10**12), 0)
        self.assertEqual(path.read_bytes(), b"author original")

    def test_failed_remote_verification_keeps_audio(self):
        path = self.cache.fetch(self.key)
        self.fail = True
        with self.assertRaises(RuntimeError):
            self.cache.prune(0, open_files="", now=10**12)
        self.assertTrue(path.exists())

    def test_download_failure_leaves_no_partial_audio(self):
        self.fail = True
        with self.assertRaises(RuntimeError):
            self.cache.fetch(self.key)
        self.assertFalse(self.cache.path(self.key).exists())
        self.assertEqual(list(self.cache.path(self.key).parent.glob('.juke-download-*')), [])

    def test_traversal_and_symlinks_rejected(self):
        for key in ["../out/a.mp3", "/tmp/out/a.mp3", "x/../out/a.mp3", "x//out/a.mp3"]:
            with self.assertRaises(ValueError): self.cache.path(key)
        self.cache.pop.mkdir(exist_ok=True)
        (self.cache.pop / "escape").symlink_to(self.root, target_is_directory=True)
        with self.assertRaises(ValueError): self.cache.path("escape/out/a.mp3")

    def test_adopt_requires_remote_checksum(self):
        path = self.cache.path(self.key)
        path.parent.mkdir(parents=True)
        path.write_bytes(b"not the remote audio")
        self.assertEqual(self.cache.adopt(), 0)
        path.write_bytes(self.audio)
        self.assertEqual(self.cache.adopt(), 1)

    def test_pin_during_remote_check_prevents_eviction(self):
        path = self.cache.fetch(self.key)
        call = self.cache.aws_call
        def pin_while_checking(*args):
            self.cache.fetch(self.key, pin=True)
            return call(*args)
        self.cache.aws_call = pin_while_checking
        self.assertEqual(self.cache.prune(0, open_files="", now=10**12), 0)
        self.assertTrue(path.exists())


if __name__ == "__main__": unittest.main()
