"""Exercise the remote protocol with no network or paid inference."""
import base64
import json
from pathlib import Path
import shutil
import tempfile
import threading
import unittest
from unittest.mock import patch

from PIL import Image
from local_run import HERE
from move_control import MoveCancelled
from remote_run import move, request


class RemoteMoves(unittest.TestCase):
    def setUp(self):
        self.folder = Path(tempfile.mkdtemp(dir=HERE / "local"))
        self.before = HERE / "starts/base.png"
        self.cancel = threading.Event()
        self.calls = []
        self.handle = {"request_id": "test-request", "status_url": "https://queue.fal.run/status",
                       "response_url": "https://queue.fal.run/response", "cancel_url": "https://queue.fal.run/cancel"}
        self.env = patch.dict("os.environ", {"NOPAINT_ENABLE_FAL": "1", "FAL_KEY": "test-only"})
        self.env.start()

    def tearDown(self):
        self.env.stop()
        shutil.rmtree(self.folder)

    def transport(self, url, method="GET", payload=None, key=None):
        self.calls.append((url, method, payload, key))
        if method == "POST":
            self.assertEqual(base64.b64decode(payload["image_urls"][0].split(",")[1]), self.before.read_bytes())
            self.assertEqual(payload["image_size"], {"width": 256, "height": 256})
            return self.handle
        if method == "PUT":
            return {"status": "CANCELLATION_REQUESTED"}
        if url.endswith("status"):
            return {"status": "COMPLETED"}
        return {"images": [{"url": "data:image/png;base64," + base64.b64encode(self.before.read_bytes()).decode()}]}

    def run_move(self, transport=None):
        return move(None, None, self.before, self.folder, 1, .25, 42,
                    cancel=self.cancel, transport=transport or self.transport, poll_seconds=0)

    def test_full_input_output_and_receipt(self):
        output = self.run_move()
        with Image.open(output) as image:
            self.assertEqual(image.size, (256, 256))
        receipt = json.loads(output.with_suffix(".json").read_text())
        self.assertEqual(receipt["status"], "complete")
        self.assertEqual(receipt["request_id"], "test-request")
        self.assertNotIn("test-only", output.with_suffix(".json").read_text())

    def test_cancel_after_submission_requests_cancel_and_discards_result(self):
        def transport(*args, **kwargs):
            response = self.transport(*args, **kwargs)
            if args[1:2] == ("POST",):
                self.cancel.set()
            return response
        with self.assertRaises(MoveCancelled):
            self.run_move(transport)
        self.assertEqual([call[1] for call in self.calls], ["POST", "PUT"])
        self.assertFalse((self.folder / "001.png").exists())
        self.assertEqual(json.loads((self.folder / "001.json").read_text())["status"], "cancelled")

    def test_unknown_submission_outcome_is_never_retried(self):
        def failed(*args, **kwargs):
            self.calls.append(args)
            raise TimeoutError("Simulated network timeout")
        with self.assertRaises(TimeoutError):
            self.run_move(failed)
        self.assertEqual(len(self.calls), 1)

    def test_disabled_engine_cannot_submit(self):
        with patch.dict("os.environ", {"NOPAINT_ENABLE_FAL": "0"}):
            with self.assertRaises(ValueError):
                self.run_move()
        self.assertEqual(self.calls, [])

    def test_credentials_cannot_be_forwarded_to_result_host(self):
        with self.assertRaises(ValueError):
            request("https://example.com/image", key="test-only")


if __name__ == "__main__":
    unittest.main()
