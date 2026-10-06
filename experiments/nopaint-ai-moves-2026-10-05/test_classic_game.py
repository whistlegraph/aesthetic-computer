"""Classic engines use the real No/Paint and mask paths without AI or billing."""
import shutil
import unittest
from unittest.mock import patch

from PIL import Image, ImageChops, ImageDraw
from paint_region import encode_mask
from play import Game


class ClassicGame(unittest.TestCase):
    def setUp(self):
        fleet = patch("fleet_run.configuration", return_value=None)
        fleet.start(); self.addCleanup(fleet.stop)
        self.game = Game(engine="classic")
        self.wait()

    def tearDown(self):
        self.game.pool.shutdown(wait=True)
        shutil.rmtree(self.game.folder)

    def wait(self):
        self.game.pool.submit(lambda: None).result(timeout=10)
        self.assertIsNone(self.game.state()["error"])

    def act(self, action, **extra):
        state = self.game.state()
        result = self.game.act({"action": action, "revision": state["revision"],
            "accepted": state["accepted"], "generation": state["generation"],
            "quote": (state.get("quote") or {}).get("id"), **extra})
        self.wait()
        return self.game.state()

    def test_each_family_is_free_without_login_and_no_restores_its_input(self):
        for engine in ("classic", "classic-pixels", "classic-color", "classic-primitives"):
            before = self.act("engine", engine=engine)
            self.assertFalse(before["account"]["connected"])
            self.assertEqual(before["quote"]["braincells"], 0)
            after = self.act("paint")
            self.assertNotEqual(before["accepted"], after["accepted"])
            self.assertEqual(after["accepted_count"], before["accepted_count"] + 1)
            self.assertEqual(after["cost"], {"braincells": 0, "status": "free"})
            self.assertIsNone(self.game.pipe)
            self.assertIsNone(self.game.embed)
            restored = self.act("no")
            self.assertEqual(restored["accepted"], before["accepted"])
            self.assertFalse(restored["busy"])

    def test_mask_preserves_every_unpainted_pixel(self):
        mask_path = self.game.folder / "test-mask.png"
        mask = Image.new("1", (256, 256)); ImageDraw.Draw(mask).rectangle((90, 100, 120, 130), fill=1)
        mask.save(mask_path)
        self.act("engine", engine="classic-primitives")
        self.act("mask", mask=encode_mask(mask_path))
        self.game.quote["seed"] = 0  # Hatch intersects the selected region.
        with Image.open(self.game.history[-1]) as original:
            before = original.convert("RGB")
        self.act("paint")
        with Image.open(self.game.history[-1]) as result:
            difference = ImageChops.difference(before, result.convert("RGB"))
        outside = ImageChops.multiply(difference, ImageChops.invert(mask.convert("RGB")))
        self.assertIsNone(outside.getbbox())
        self.assertIsNotNone(difference.getbbox())


if __name__ == "__main__":
    unittest.main()
