"""Export must capture exactly one accepted image without advancing or paying."""
import shutil
import threading
import unittest
from unittest.mock import patch

from PIL import Image
from play import Game, Conflict


class UpscaleExport(unittest.TestCase):
    def setUp(self):
        fleet = patch("fleet_run.configuration", return_value=None)
        fleet.start(); self.addCleanup(fleet.stop)
        self.game = Game(generator=lambda *args, **kwargs: self.fail("Export generated a move"), engine="turbo")
        self.game.pool.submit(lambda: None).result(timeout=5)
        self.before = self.game.state()

    def tearDown(self):
        self.game.pool.shutdown(wait=True)
        shutil.rmtree(self.game.folder)

    def test_export_preserves_history_quote_and_billing_and_reports_real_progress(self):
        started, release = threading.Event(), threading.Event()
        def upscale(source, output, scale, progress):
            self.assertEqual(source, self.game.history[-1])
            progress(.5); started.set()
            self.assertTrue(release.wait(5))
            output.parent.mkdir(parents=True, exist_ok=True)
            Image.new("RGB", (256*scale, 256*scale)).save(output)
            return {"seconds": 1.2}
        with patch("upscale.upscale_image", side_effect=upscale):
            try:
                self.game.start_upscale({"accepted": self.before["accepted"], "scale": 4})
                self.assertTrue(started.wait(5))
                running = self.game.state()
                self.assertEqual(running["upscale"]["progress"], .5)
                with self.assertRaises(Conflict):
                    self.game.start_upscale({"accepted": self.before["accepted"], "scale": 2})
                with self.assertRaises(Conflict):
                    self.game.act({"action": "paint", "revision": running["revision"], "quote": running["quote"]["id"]})
                with self.assertRaises(Conflict):
                    self.game.done({})
            finally:
                release.set()
            self.game.pool.submit(lambda: None).result(timeout=5)
        after = self.game.state()
        for key in ("accepted", "before", "accepted_count", "quote", "cost", "candidate", "generation"):
            self.assertEqual(after[key], self.before[key], key)
        self.assertEqual(after["upscale"]["braincells"], 0)
        self.assertFalse(after["upscale"]["busy"])
        self.assertEqual(after["upscale"]["progress"], 1)
        image = self.game.images[after["upscale"]["url"].removeprefix("/image/").removesuffix(".png")]
        with Image.open(image) as result:
            self.assertEqual(result.size, (1024, 1024))

    def test_stale_source_invalid_scale_and_failure_never_touch_canvas(self):
        with self.assertRaises(Conflict):
            self.game.start_upscale({"accepted": "/image/stale.png", "scale": 4})
        for scale in (True, 0, 3, 16, "4"):
            with self.assertRaises(ValueError):
                self.game.start_upscale({"accepted": self.before["accepted"], "scale": scale})
        with patch("upscale.upscale_image", side_effect=RuntimeError("Model unavailable")):
            self.game.start_upscale({"accepted": self.before["accepted"], "scale": 2})
            self.game.pool.submit(lambda: None).result(timeout=5)
        after = self.game.state()
        self.assertEqual(after["accepted"], self.before["accepted"])
        self.assertEqual(after["quote"], self.before["quote"])
        self.assertFalse(after["upscale"]["busy"])
        self.assertEqual(after["upscale"]["error"], "Model unavailable")


class TileSeams(unittest.TestCase):
    def test_tiled_network_matches_whole_image(self):
        import torch
        from upscale import tiled_inference
        from vendor.realesrgan.srvgg_arch import SRVGGNetCompact
        torch.manual_seed(1)
        model = SRVGGNetCompact(num_feat=4, num_conv=32).eval()
        source = torch.rand(1, 3, 83, 97)
        progress = []
        with torch.inference_mode():
            whole = model(source)
            tiled = tiled_inference(model, source, progress.append, tile=48)
        self.assertTrue(torch.allclose(whole, tiled, atol=1e-6))
        self.assertEqual(progress, [index / 6 for index in range(1, 7)])


if __name__ == "__main__":
    unittest.main()
