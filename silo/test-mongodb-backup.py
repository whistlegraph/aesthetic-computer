#!/usr/bin/env python3
"""Exercise backup failure/retention behavior with isolated files and fake I/O."""
import gzip
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

SCRIPT = Path(__file__).with_name("mongodb-backup.sh")
TOOLS = r'''#!/usr/bin/env python3
import datetime, fcntl, gzip, hashlib, os, pathlib, shutil, sys, time
name=pathlib.Path(sys.argv[0]).name; args=sys.argv[1:]
root=pathlib.Path(os.environ['FIXTURE']); remote=root/'remote'
if name=='flock':
    try: fcntl.flock(int(args[-1]),fcntl.LOCK_EX|fcntl.LOCK_NB)
    except BlockingIOError: sys.exit(1)
elif name=='date':
    if '-Iseconds' in args: print('2026-11-07T12:00:00+00:00')
    elif '-d' in args: print('20261008')
    else: print('20261107_120000')
elif name=='df':
    print('Filesystem 1024-blocks Used Available Capacity Mounted on')
    print('fixture 20000000 1000 '+os.environ.get('FREE_KB','10000000')+' 1% /')
elif name=='md5sum':
    p=pathlib.Path(args[0]); print(hashlib.md5(p.read_bytes()).hexdigest()+'  '+str(p))
elif name=='mongodump':
    (root/'dump-started').write_text('yes')
    p=pathlib.Path(next(a.split('=',1)[1] for a in args if a.startswith('--archive=')))
    if os.environ.get('DUMP_FAIL'): p.write_bytes(b'partial'); sys.exit(2)
    if os.environ.get('DUMP_LARGE'): p.write_bytes(os.urandom(1024*1024)); sys.exit(0)
    if os.environ.get('DUMP_WAIT'): time.sleep(2)
    p.write_bytes(gzip.compress(b'restored fixture'))
elif name=='curl':
    if os.environ.get('REMOTE_DOWN'): sys.exit(22)
    url=next(a for a in args if a.startswith('https://'))
    p=remote/url.rsplit('/',1)[-1]
    if '-I' in args:
        if not p.exists(): sys.exit(22)
        b=p.read_bytes(); digest=hashlib.md5(b).hexdigest()
        if os.environ.get('BAD_ETAG'): digest='0'*32
        print('HTTP/1.1 200 OK\r\nContent-Length: '+str(len(b))+'\r\nETag: "'+digest+'"\r\n')
    elif '-T' in args:
        if os.environ.get('UPLOAD_FAIL'): sys.exit(22)
        shutil.copyfile(args[args.index('-T')+1],p)
    elif '-X' in args: p.unlink()
    else: print('<ListBucketResult>'+''.join('<Key>mongodb/'+x.name+'</Key>' for x in remote.iterdir())+'</ListBucketResult>')
'''


@unittest.skipUnless(sys.platform.startswith("linux"), "run against the production Linux/bash toolchain")
class BackupTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.local = self.root / "backups"
        self.remote = self.root / "remote"
        self.bin = self.root / "bin"
        for d in [self.local, self.remote, self.bin]:
            d.mkdir()
        for name in ["flock", "date", "df", "md5sum", "mongodump", "curl"]:
            p = self.bin / name
            p.write_text(TOOLS)
            p.chmod(0o755)
        self.config = self.root / "env"
        self.config.write_text("MONGODB_CONNECTION_STRING=mongodb://fixture/test\nSPACES_KEY=fixture\nSPACES_SECRET=fixture\nSPACES_ENDPOINT=https://sfo3.digitaloceanspaces.com\n")
        self.env = dict(os.environ, PATH=str(self.bin)+os.pathsep+os.environ["PATH"],
                        FIXTURE=str(self.root), SILO_BACKUP_ENV_FILE=str(self.config),
                        SILO_BACKUP_DIR=str(self.local), SILO_BACKUP_LOG=str(self.root / "backup.log"))

    def seed(self, count=7):
        for day in range(1, count+1):
            name = f"202611{day:02d}_040001.archive.gz"
            data = gzip.compress(f"backup-{day}".encode())
            (self.local / name).write_bytes(data)
            (self.remote / name).write_bytes(data)

    def run_backup(self, *args, **env):
        return subprocess.run(["bash", str(SCRIPT), *args], env=dict(self.env, **env),
                              capture_output=True, text=True, timeout=15)

    def archives(self):
        return sorted(p.name for p in self.local.glob("*.archive.gz"))

    def test_verified_pruning_keeps_newest_three_and_offsite_history(self):
        self.seed()
        result = self.run_backup("--prune-only")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.archives(), [f"202611{n:02d}_040001.archive.gz" for n in [5, 6, 7]])
        self.assertEqual(len(list(self.remote.iterdir())), 7)
        self.assertFalse((self.root / "dump-started").exists())

    def test_same_size_wrong_content_is_not_deleted(self):
        self.seed()
        (self.remote / self.archives()[0]).write_bytes(b'x' * (self.local / self.archives()[0]).stat().st_size)
        result = self.run_backup("--prune-only")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(self.archives()), 4)
        self.assertIn("20261101_040001.archive.gz", self.archives())

    def test_remote_outage_preserves_all_local_backups(self):
        self.seed()
        result = self.run_backup("--prune-only", REMOTE_DOWN="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(self.archives()), 7)

    def test_low_disk_skips_dump_even_with_remote_outage(self):
        self.seed()
        result = self.run_backup(FREE_KB="1048576", REMOTE_DOWN="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.root / "dump-started").exists())
        self.assertEqual(len(self.archives()), 7)

    def test_dump_failure_cleans_partial_and_keeps_three(self):
        self.seed(3)
        result = self.run_backup(DUMP_FAIL="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(self.archives()), 3)
        self.assertEqual(list(self.local.glob("*.partial")), [])

    def test_upload_failure_keeps_new_local_archive(self):
        self.seed(3)
        result = self.run_backup(UPLOAD_FAIL="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(self.archives()), 4)
        self.assertEqual(len(list(self.remote.iterdir())), 3)

    def test_success_verifies_upload_before_pruning(self):
        self.seed(3)
        result = self.run_backup()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(self.archives()), 3)
        self.assertEqual(len(list(self.remote.iterdir())), 4)
        self.assertIn("20261107_120000.archive.gz", self.archives())

    def test_filename_collision_does_not_remove_existing_partial(self):
        p = self.local / "20261107_120000.archive.gz.partial"
        p.write_bytes(b'existing')
        result = self.run_backup()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(p.read_bytes(), b'existing')

    def test_concurrent_run_is_rejected(self):
        first = subprocess.Popen(["bash", str(SCRIPT)], env=dict(self.env, DUMP_WAIT="1"),
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            import time
            deadline = time.monotonic()+5
            while not (self.root / 'dump-started').exists() and time.monotonic() < deadline:
                time.sleep(.05)
            result = self.run_backup("--prune-only")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("another backup/prune", result.stderr)
            self.assertEqual(first.wait(timeout=10), 0)
        finally:
            if first.poll() is None: first.kill()
            first.communicate()

    @unittest.skipUnless(__import__('sys').platform.startswith('linux'), 'production Linux file-size limit')
    def test_kernel_stops_dump_before_reserved_disk_budget(self):
        result = self.run_backup(FREE_KB=str(2097152+64), DUMP_LARGE="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(list(self.local.glob("*.partial")), [])
        self.assertEqual(self.archives(), [])


if __name__ == "__main__":
    unittest.main()
