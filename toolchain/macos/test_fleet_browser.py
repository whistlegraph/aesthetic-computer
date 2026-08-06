import importlib.util
import json
import os
import socket
import stat
import subprocess
import tempfile
import time
import unittest
from types import SimpleNamespace
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("fleet_browser.py")
SPEC = importlib.util.spec_from_file_location("fleet_browser", MODULE_PATH)
fleet_browser = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(fleet_browser)


FAKE_CHROME = r'''#!/usr/bin/python3
import http.server
import json
import os
import signal
import sys

port = int(os.environ["FAKE_BROWSER_PORT"])

class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/json/version":
            body = json.dumps({"Browser": "Fake Chrome", "webSocketDebuggerUrl": "ws://127.0.0.1/fake"}).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        else:
            self.send_response(404)
            self.end_headers()
    def log_message(self, *_):
        pass

signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))
http.server.HTTPServer(("127.0.0.1", port), Handler).serve_forever()
'''


class FleetBrowserTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.profile = self.root / "profile"
        (self.profile / "Profile 1").mkdir(parents=True)
        (self.profile / "Local State").write_text(
            json.dumps(
                {
                    "profile": {
                        "info_cache": {
                            "Profile 1": {
                                "gaia_name": "Jeffrey",
                                "user_name": "jeffrey@example.test",
                            }
                        }
                    }
                }
            )
        )
        self.fake = self.root / "fake-chrome"
        self.fake.write_text(FAKE_CHROME)
        self.fake.chmod(self.fake.stat().st_mode | stat.S_IXUSR)
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            self.port = listener.getsockname()[1]
        self.old_env = os.environ.copy()
        os.environ.update(
            {
                "SLAB_BROWSER_STATE_DIR": str(self.root / "state"),
                "SLAB_BROWSER_CHROME_PROFILE": str(self.profile),
                "SLAB_BROWSER_CHROME": str(self.fake),
                "SLAB_BROWSER_PROCESS_MARKER": str(self.fake.resolve()),
            }
        )
        (self.profile / "DevToolsActivePort").write_text(
            f"{self.port}\n/devtools/browser/fake-browser\n"
        )
        fake_env = os.environ.copy()
        fake_env["FAKE_BROWSER_PORT"] = str(self.port)
        self.browser = subprocess.Popen(
            [str(self.fake.resolve()), f"--user-data-dir={self.profile.resolve()}", "--profile-directory=Profile 1"],
            env=fake_env,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        deadline = time.monotonic() + 3
        while time.monotonic() < deadline and not fleet_browser.endpoint_ready(self.port):
            time.sleep(0.05)
        self.assertIsNotNone(fleet_browser.endpoint_ready(self.port))

    def tearDown(self):
        try:
            fleet_browser.main(["release", "--owner", "owner-a", "--json"])
            fleet_browser.main(["release", "--owner", "owner-b", "--json"])
        except Exception:
            pass
        self.browser.terminate()
        self.browser.wait(timeout=3)
        os.environ.clear()
        os.environ.update(self.old_env)
        self.temp.cleanup()

    def test_parse_chrome_processes_distinguishes_debug_instances(self):
        marker = str(self.fake.resolve())
        output = (
            f" 42 {marker} --remote-debugging-port=9444 --user-data-dir=/tmp/managed\n"
            f" 43 {marker} --profile-directory=Profile 1\n"
            f" 44 zsh -c ps | grep '{marker}'\n"
        )
        rows = fleet_browser.parse_chrome_processes(output)
        self.assertEqual(len(rows), 2)
        self.assertEqual(rows[0]["remoteDebuggingPort"], 9444)
        self.assertEqual(rows[0]["userDataDir"], "/tmp/managed")
        self.assertIsNone(rows[1]["remoteDebuggingPort"])

    def test_profile_identity_enforces_expected_user(self):
        identity = fleet_browser.profile_identity(self.profile, "Profile 1")
        self.assertEqual(identity["user"], "jeffrey@example.test")
        fleet_browser.check_expected_user(identity, "JEFFREY@example.test")
        with self.assertRaises(fleet_browser.FleetBrowserError):
            fleet_browser.check_expected_user(identity, "someone@example.test")

    def test_acquire_reuse_and_final_release(self):
        common = [
            "--profile-root",
            str(self.profile),
            "--expected-user",
            "jeffrey@example.test",
            "--json",
        ]
        self.assertEqual(fleet_browser.main(["acquire", "--owner", "owner-a", *common]), 0)
        reused = fleet_browser.command_acquire(
            SimpleNamespace(
                owner="owner-b",
                profile_root=str(self.profile),
                profile_directory="Profile 1",
                expected_user="jeffrey@example.test",
            )
        )
        self.assertTrue(reused["reused"])
        self.assertEqual(reused["webSocketPath"], "/devtools/browser/fake-browser")
        state = fleet_browser.read_state()
        self.assertEqual(sorted(state["owners"]), ["owner-a", "owner-b"])

        self.assertEqual(fleet_browser.main(["release", "--owner", "owner-a", "--json"]), 0)
        self.assertEqual(sorted(fleet_browser.read_state()["owners"]), ["owner-b"])
        self.assertEqual(fleet_browser.main(["release", "--owner", "owner-b", "--json"]), 0)
        self.assertIsNone(fleet_browser.read_state())
        self.assertIsNone(self.browser.poll())


if __name__ == "__main__":
    unittest.main()
