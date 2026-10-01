#!/usr/bin/env python3
"""Exercise the actual macOS locks without changing the live Deskflow seat."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


HELPER = Path(__file__).with_name("deskflow-lock")
WORKER = '''
source "$1"
deskflow_lock_acquire "$2" || exit 75
trap deskflow_lock_release EXIT
printf 'locked\\n'
read -r _
'''


class DeskflowLockTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name) / "claim.lock"
        self.workers = []

    def tearDown(self):
        for worker in self.workers:
            if worker.poll() is None:
                worker.kill()
            worker.communicate(timeout=5)
        self.temp.cleanup()

    def start_worker(self):
        worker = subprocess.Popen(
            ["/bin/bash", "-c", WORKER, "lock-test", str(HELPER), str(self.base)],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True,
        )
        self.workers.append(worker)
        return worker

    def acquire(self):
        worker = self.start_worker()
        self.assertEqual(worker.stdout.readline(), "locked\n")
        return worker

    def release(self, worker):
        worker.communicate("release\n", timeout=5)
        self.assertEqual(worker.returncode, 0)

    def test_live_owner_excludes_contender_then_releases(self):
        owner = self.acquire()
        contender = self.start_worker()
        contender.communicate(timeout=5)
        self.assertEqual(contender.returncode, 75)
        self.release(owner)
        self.assertEqual(len(list(self.base.parent.glob("*.flock"))), 1)
        self.release(self.acquire())

    def test_killed_owner_is_reclaimed(self):
        owner = self.acquire()
        owner.kill()
        owner.communicate(timeout=5)
        self.assertEqual(len(list(self.base.parent.glob("*.flock"))), 1)
        self.release(self.acquire())
        self.assertEqual(len(list(self.base.parent.glob("*.flock"))), 1)

    def test_leftover_file_and_legacy_directory_do_not_block(self):
        self.base.mkdir()
        old = Path(str(self.base) + ".flock")
        old.write_text(str(os.getpid()) + "\n")
        self.release(self.acquire())
        self.assertTrue(old.exists())
        self.assertTrue(self.base.is_dir())


if __name__ == "__main__":
    unittest.main()
