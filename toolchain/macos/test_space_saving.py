import importlib.util
import os
from pathlib import Path
import plistlib
import tempfile
import unittest


def module(name):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).parent / (name + ".py"))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


class SpaceSavingTests(unittest.TestCase):
    def test_changed_or_open_media_cannot_be_evicted(self):
        media = module("workspace-media")
        with tempfile.TemporaryDirectory() as root:
            path = Path(root) / "recording.wav"
            path.write_bytes(b"original")
            record = dict(sha256=media.hashes(path)[0], mtime_ns=path.stat().st_mtime_ns)
            self.assertTrue(media.removable(path, record, ""))
            self.assertFalse(media.removable(path, record, str(path)))
            path.write_bytes(b"new work")
            self.assertFalse(media.removable(path, record, ""))

    def test_media_paths_cannot_escape_or_follow_symlinks(self):
        media = module("workspace-media")
        with tempfile.TemporaryDirectory() as root:
            root = Path(root)
            for key in ["../file.wav", "/tmp/file.wav", "x//file.wav"]:
                with self.assertRaises(ValueError): media.checked_path(key, root)
            (root / "link").symlink_to(root, target_is_directory=True)
            with self.assertRaises(ValueError): media.checked_path("link/file.wav", root)

    def test_chrome_keeps_newest_recent_open_and_unknown(self):
        chrome = module("chrome-clones")
        with tempfile.TemporaryDirectory() as root:
            root = Path(root)
            for number in range(5):
                clone = root / f"code_sign_clone.{number}"
                info = clone / "Google Chrome.app.bundle/Contents/Info.plist"
                info.parent.mkdir(parents=True)
                info.write_bytes(plistlib.dumps({"CFBundleIdentifier": "com.google.Chrome"}))
                os.utime(clone, (number + 1, number + 1))
            (root / "code_sign_clone.2/unexpected.txt").write_text("keep")
            os.utime(root / "code_sign_clone.2", (3, 3))
            selected = list(chrome.candidates(root, str(root / "code_sign_clone.1"), now=10000))
            self.assertEqual({p.name for p in selected}, {"code_sign_clone.0", "code_sign_clone.3"})
            self.assertEqual(list(chrome.candidates(root, "", now=100)), [])


if __name__ == "__main__": unittest.main()
