import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("guard", Path(__file__).with_name("performance_guard.py"))
guard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(guard)


class GuardTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.state = Path(self.temp.name)
        self.patcher = patch.object(guard, "STATE", self.state)
        self.patcher.start()
        self.addCleanup(self.patcher.stop)
        self.healthy = dict(cores=6, free_pct=60, load1=2, disk_free_bytes=40 * 1024**3,
                            disk_floor_bytes=guard.FLOOR, boot="boot-1", swapusage="0")

    def check(self, metrics=None):
        with patch.object(guard, "metrics", return_value=metrics or self.healthy), contextlib.redirect_stderr(io.StringIO()):
            return guard.admit("test")

    def test_low_disk_blocks_without_sampler(self):
        self.assertEqual(self.check({**self.healthy, "disk_free_bytes": 4 * 1024**3}), 75)

    def test_playback_blocks_even_with_pressure_override(self):
        guard.atomic(self.state / "playback.json", '{"mode":"on"}')
        with patch.dict(os.environ, {"AC_PERFORMANCE_ALLOW_PRESSURE": "1"}):
            self.assertEqual(self.check(), 75)

    def test_menu_band_updater_passes_playback_but_not_pressure(self):
        guard.atomic(self.state / "playback.json", '{"mode":"on"}')
        with patch.dict(os.environ, {"MENUBAND_UPDATE": "1"}):
            self.assertEqual(self.check(), 0)
            self.assertEqual(self.check({**self.healthy, "disk_free_bytes": 4 * 1024**3}), 75)

    def test_auto_playback_uses_exact_executable_and_fresh_state(self):
        guard.atomic(self.state / "playback.json", '{"mode":"auto"}')
        self.assertTrue(guard.playback_active([{"executable": "/Applications/Menu Band.app/Contents/MacOS/MenuBand"}]))
        self.assertFalse(guard.playback_active([{"executable": "/tmp/MenuBandTests"}]))
        guard.atomic(self.state / "latest.json", json.dumps(dict(epoch=time.time(), boot="boot-1", reasons=["playback"])))
        with patch.object(guard.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "/bin/zsh\n")):
            self.assertEqual(self.check(), 0)

    def test_playback_probe_failure_defers(self):
        guard.atomic(self.state / "playback.json", '{"mode":"auto"}')
        with patch.object(guard.subprocess, "run", side_effect=subprocess.CalledProcessError(1, "ps")):
            self.assertEqual(self.check(), 75)

    def test_playback_sampler_blocks_workers_without_incident_or_notification(self):
        guard.atomic(self.state / "playback.json", '{"mode":"on"}')
        with patch.object(guard, "metrics", return_value=self.healthy), \
                patch.object(guard, "processes", return_value=[]), \
                patch.object(guard, "run", return_value="Swapouts: 0."), patch.object(guard, "notify") as notify:
            for _ in range(4):
                guard.sample()
            notify.assert_not_called()
        self.assertEqual((self.state / "pressure-active").read_text(), "playback\n")
        self.assertFalse((self.state / "incidents.jsonl").exists())

    def test_compute_refusal_never_executes_and_help_preserves_argv(self):
        (self.state / "real-ffmpeg").write_text("/tool/ffmpeg")
        with patch.object(guard, "HERE", self.state), patch.object(guard, "admit", return_value=75) as admission, \
                patch.object(guard.os, "execv") as execute:
            self.assertEqual(guard.compute_main("ffmpeg", ["-i", "source.wav", "result.wav"]), 75)
            execute.assert_not_called()
            admission.reset_mock()
            guard.compute_main("ffmpeg", ["-version"])
            admission.assert_not_called()
            execute.assert_called_once_with("/tool/ffmpeg", ["/tool/ffmpeg", "-version"])

    def test_demucs_runs_with_bounded_threads_and_utility_priority(self):
        (self.state / "real-demucs").write_text("/tool/demucs")
        with patch.object(guard, "HERE", self.state), patch.object(guard, "admit", return_value=0), \
                patch.object(guard.os, "execv") as execute, patch.dict(os.environ, {}, clear=False):
            guard.compute_main("demucs", ["track.wav"])
            self.assertEqual(os.environ["OMP_NUM_THREADS"], "1")
            self.assertEqual(execute.call_args.args[1],
                             ["taskpolicy", "-c", "utility", "/usr/bin/nice", "-n", "10", "/tool/demucs", "track.wav", "-j", "1"])

    def test_compute_install_preserves_commands_and_is_idempotent(self):
        bindir = self.state / ".local/bin"
        target = self.state / ".local/lib/ac-performance-guard"
        bindir.mkdir(parents=True)
        target.mkdir(parents=True)
        (target / "performance-guard.sh").write_text("entrypoint")
        for name in ("ffmpeg", "demucs"):
            (bindir / name).write_text("original " + name)
        with patch.object(guard.Path, "home", return_value=self.state), \
                patch.object(guard.shutil, "which", side_effect=lambda name: str(bindir / name)), \
                contextlib.redirect_stdout(io.StringIO()):
            guard.install_compute()
            guard.install_compute()
        self.assertEqual((bindir / "ffmpeg.before-ac-guard").read_text(), "original ffmpeg")
        self.assertEqual((bindir / "demucs").resolve(), (target / "performance-guard.sh").resolve())

    def test_healthy_host_admitted_without_sampler(self):
        self.assertEqual(self.check(), 0)

    def test_memory_and_cpu_budgets(self):
        self.assertEqual(self.check({**self.healthy, "free_pct": 3}), 75)
        self.assertEqual(self.check({**self.healthy, "load1": 20}), 75)

    def test_stale_pressure_does_not_block_recovery(self):
        guard.atomic(self.state / "pressure-active", "swap")
        guard.atomic(self.state / "latest.json", json.dumps(dict(epoch=time.time()-300, boot="boot-1", reasons=["swap"])))
        self.assertEqual(self.check(), 0)

    def test_recent_swap_pressure_blocks_and_reboot_clears_it(self):
        data = dict(epoch=time.time(), boot="boot-1", reasons=["swap"])
        guard.atomic(self.state / "latest.json", json.dumps(data))
        self.assertEqual(self.check(), 75)
        self.assertEqual(self.check({**self.healthy, "boot": "boot-2"}), 0)

    def test_failed_measurement_defers_work(self):
        with patch.object(guard, "metrics", side_effect=OSError("unavailable")), contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(guard.admit("test"), 75)

    def test_valid_json_with_corrupt_counters_recovers(self):
        guard.atomic(self.state / "latest.json", '{"epoch":"timestamp=broken","breaches":"bad"}')
        self.assertEqual(self.check(), 0)

    def test_global_git_options_and_destination(self):
        cases = [
            (["-C", "/repo", "-C", "sub", "-c", "x=y", "worktree", "add", "-b", "topic", "../new", "HEAD"], "/repo/new"),
            (["-C/repo", "worktree", "add", "--orphan", "new"], "/repo/new"),
            (["worktree", "add", "--lock", "--reason", "a reason", "--", "-new"], "/repo/-new"),
            (["worktree", "add", "--no-checkout", "/other/new", "HEAD"], "/other/new"),
            (["worktree", "list"], None),
            (["show", "worktree", "add"], None),
            (["worktree", "add", "--help"], None),
        ]
        for args, expected in cases:
            with self.subTest(args=args):
                self.assertEqual(guard.worktree_destination(args, "/repo"), expected)

    def test_git_denial_never_executes(self):
        with patch.object(guard, "admit", return_value=75), patch.object(guard.os, "execv") as execute:
            self.assertEqual(guard.git_main(["worktree", "add", "/tmp/example"]), 75)
            execute.assert_not_called()

    def test_git_read_only_preserves_argv_without_admission(self):
        args = ["-C", "/some path", "worktree", "list", "--porcelain"]
        with patch.object(guard, "admit") as admission, patch.object(guard.os, "execv") as execute:
            guard.git_main(args)
            admission.assert_not_called()
            self.assertEqual(execute.call_args.args[1][1:], args)

    def test_redacts_git_credentials_and_messages(self):
        text = guard.safe_git_command('git -c "http.extraHeader=Authorization: Bearer SECRET" fetch https://user:SECRET@example.com/repo')
        self.assertNotIn("SECRET", text)
        self.assertIn("fetch", text)
        self.assertNotIn("private message", guard.safe_git_command('git commit -m "private message"'))
        self.assertIn("/tmp/work", guard.safe_git_command('git -C /tmp/work merge origin/main --no-edit'))

    def test_failed_atomic_replace_preserves_previous_sample(self):
        file = self.state / "latest.json"
        file.write_text('{"healthy":true}')
        with patch.object(guard.os, "replace", side_effect=OSError("disk full")):
            with self.assertRaises(OSError):
                guard.atomic(file, "broken")
        self.assertEqual(file.read_text(), '{"healthy":true}')
        self.assertFalse((self.state / "latest.json.next").exists())

    def test_corrupt_state_recovers_and_records_disk_pressure(self):
        (self.state / "latest.json").write_text("timestamp=corrupted")
        with patch.object(guard, "metrics", return_value={**self.healthy, "disk_free_bytes": 10 * 1024**3}), \
                patch.object(guard, "processes", return_value=[]), \
                patch.object(guard, "run", return_value="Swapouts: 123."), patch.object(guard, "notify"):
            self.assertEqual(guard.sample(), 0)
        data = guard.read_json(self.state / "latest.json")
        self.assertEqual(data["reasons"], ["disk"])
        self.assertEqual(data["swapout_pages_delta"], 0)
        self.assertEqual((self.state / "pressure-active").read_text(), "disk\n")

    def test_swap_rate_and_boot_reset(self):
        previous = dict(epoch=time.time()-30, boot="boot-1", swapouts=100, breaches=1)
        guard.atomic(self.state / "latest.json", json.dumps(previous))
        with patch.object(guard, "metrics", return_value=self.healthy), \
                patch.object(guard, "processes", return_value=[]), \
                patch.object(guard, "run", return_value="Swapouts: 10100."), patch.object(guard, "notify"):
            guard.sample()
            self.assertIn("swap", guard.read_json(self.state / "latest.json")["reasons"])
        with patch.object(guard, "metrics", return_value={**self.healthy, "boot": "boot-2"}), \
                patch.object(guard, "processes", return_value=[]), \
                patch.object(guard, "run", return_value="Swapouts: 10100."):
            guard.sample()
            self.assertFalse((self.state / "pressure-active").exists())

    def test_swift_refusal_never_invokes_compiler(self):
        # Exercise the shell entry point outside the AC checkout, with a stub
        # gate. A stub lock fails too, proving refusal happens before locking.
        source = Path(__file__).with_name("swift-guard.sh")
        (self.state / "swift-guard.sh").write_text(source.read_text())
        (self.state / "performance-guard.sh").write_text('#!/bin/bash\nexit 75\n')
        (self.state / "build-lock.sh").write_text('acquire_build_lock() { exit 99; }\n')
        result = subprocess.run(["/bin/bash", str(self.state / "swift-guard.sh"), "build", "--package-path", str(self.state)], capture_output=True)
        self.assertEqual(result.returncode, 75)

    def test_login_path_repair_preserves_profile(self):
        profile = self.state / ".zprofile"
        profile.write_text("# existing setting\nexport MY_SETTING=yes\n")
        expected = f"{self.state}/.local/bin/git\n{self.state}/.local/bin/swift"
        with patch.object(guard.Path, "home", return_value=self.state), \
                patch.dict(os.environ, {"SHELL": "/bin/zsh"}), \
                patch.object(guard, "run", side_effect=["/usr/bin/git\n/usr/bin/swift", expected]):
            guard.ensure_shell_path()
        self.assertIn("export MY_SETTING=yes", profile.read_text())
        self.assertEqual(profile.read_text().count("# AC performance guard PATH"), 1)


if __name__ == "__main__":
    unittest.main()
