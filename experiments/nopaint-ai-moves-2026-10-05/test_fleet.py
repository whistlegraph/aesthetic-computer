"""Exercise the actual streaming protocol without loading model weights."""
import base64
import json
import os
from pathlib import Path
import sys
import tempfile
import threading
import time
import unittest
from PIL import Image
import fleet_run
from fleet_worker import serve, validate
from local_run import HERE, sha, write
from move_control import MoveCancelled, check_cancel


def fixture_worker():
    root = Path(os.environ["NOPAINT_TEST_WORKER_ROOT"])

    def generate(pipe, embed, before, folder, step, strength, seed, observe, cancel, mask=None):
        folder.mkdir(parents=True)
        image = Image.open(before).copy()
        for index in range(3):
            check_cancel(cancel)
            image.putpixel((index, 0), (10+index, 20, 30))
            frame = folder / f"frame-{index}.png"
            image.save(frame)
            observe({"index": index, "step": index, "steps": 2,
                     "image": str(frame.relative_to(root.parent.parent)),
                     "latent_sha256": str(index), "approximate_decode": True})
            time.sleep(.05)
        check_cancel(cancel)
        output = folder / "001.png"
        image.save(output)
        write(output.with_suffix('.json'), {"identity": {
            "model": fleet_run.MODEL, "revision": fleet_run.REVISION,
            "input_sha256": sha(before), "seed": seed, "strength": strength, "size": [256, 256],
            **({"mask_sha256": sha(mask)} if mask else {})},
            "total_seconds": .15, "actual_denoise_steps": 2, "output_sha256": sha(output)})
        return output

    serve(lambda name: (None, None), generate, root, fleet_run.MODEL, fleet_run.REVISION)


class FleetProtocol(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(dir=HERE/'local', prefix='test-fleet-')
        self.root = Path(self.tmp.name)
        self.before = self.root/'before.png'
        Image.new('RGB', (256, 256), (100, 50, 25)).save(self.before)
        os.environ['NOPAINT_TEST_WORKER_ROOT'] = str(self.root/'local'/'fleet')
        self.worker = fleet_run.Worker([sys.executable, '-u', '-c', 'from test_fleet import fixture_worker; fixture_worker()'])

    def tearDown(self):
        self.worker.close()
        self.tmp.cleanup()
        os.environ.pop('NOPAINT_TEST_WORKER_ROOT', None)
        fleet_run.OFFLINE_UNTIL = 0

    def move(self, number, **kwargs):
        return fleet_run.move(self.worker, None, self.before, self.root/'proposals', number, .25, 12, **kwargs)

    def test_full_input_streamed_previews_and_verified_result_reuse_warm_worker(self):
        seen = []
        output = self.move(1, observe=seen.append)
        self.assertEqual(len(seen), 3)
        self.assertTrue(all((HERE/f['image']).is_file() for f in seen))
        self.assertEqual(Image.open(output).getpixel((255,255)), (100,50,25))
        self.assertEqual(Image.open(output).getpixel((0,0)), (10,20,30))
        pid = self.worker.process.pid
        self.move(2)
        self.assertEqual(self.worker.process.pid, pid)
        row = json.loads(output.with_suffix('.json').read_text())
        self.assertEqual(row['remote']['identity']['input_sha256'], sha(self.before))
        self.assertEqual(row['status'], 'complete')

    def test_early_no_discards_rejected_output_and_does_not_poison_next_move(self):
        cancel = threading.Event()
        seen = []
        def reject(frame):
            seen.append(frame)
            cancel.set()
        with self.assertRaises(MoveCancelled):
            self.move(1, observe=reject, cancel=cancel)
        self.assertEqual(len(seen), 1)
        self.assertFalse((self.root/'proposals/001.png').exists())
        self.assertTrue(self.move(2).exists())

    def test_mask_reaches_remote_pipeline_and_receipt_matches(self):
        from paint_region import save_mask
        mask = save_mask(base64.b64encode(bytes([128])+bytes(8191)).decode(), self.root/'masks')
        output = self.move(1, mask=mask)
        row = json.loads(output.with_suffix('.json').read_text())
        self.assertEqual(row['remote']['identity']['mask_sha256'], sha(mask))

    def test_invalid_input_is_rejected_before_model_work(self):
        message = {'id':'12345678-1234-1234-1234-123456789abc','action':'move',
                   'image':base64.b64encode(self.before.read_bytes()).decode(), 'strength':.25,'seed':1}
        self.assertEqual(validate(message), self.before.read_bytes())
        for bad in ({'seed':-1}, {'strength':1}, {'id':'../escape'}, {'image':'invalid'}):
            with self.assertRaises(ValueError):
                validate({**message, **bad})

    def test_worker_disconnect_fails_without_a_candidate(self):
        self.worker = fleet_run.Worker([sys.executable,'-c','pass'])
        with self.assertRaisesRegex(RuntimeError, 'unavailable'):
            self.move(1)
        self.assertFalse((self.root/'proposals/001.png').exists())
        self.assertEqual(json.loads((self.root/'proposals/001.json').read_text())['status'], 'failed')


if __name__ == '__main__':
    unittest.main()
