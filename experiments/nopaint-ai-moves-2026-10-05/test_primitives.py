"""The free CPU palette evolves real bitmap states without changing the move loop."""
import json
from pathlib import Path
import tempfile
import threading
import unittest

import numpy as np
from PIL import Image

from engines import ENGINES, available, catalog
from local_run import HERE, sha
from move_control import MoveCancelled
from primitives import FAMILIES, OPERATIONS, STRENGTHS, move, states


class Primitives(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(dir=HERE / "local", prefix="test-primitives-")
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        y, x = np.indices((256, 256))
        self.image = Image.fromarray(np.stack((x, (x+y) % 256, (3*x+7*y) % 256), axis=2).astype(np.uint8))
        self.before = self.root / "input.png"
        self.image.save(self.before)

    def test_every_operation_is_deterministic_rgb_and_changes_the_input(self):
        original = self.image.tobytes()
        for operation in OPERATIONS:
            for strength in STRENGTHS:
                with self.subTest(operation=operation, strength=strength):
                    first = list(states(self.image, strength, 1234, operation=operation))
                    second = list(states(self.image, strength, 1234, operation=operation))
                    self.assertGreater(len(first), 1)
                    self.assertEqual([im.tobytes() for _, im in first], [im.tobytes() for _, im in second])
                    previous = original
                    for name, image in first:
                        self.assertEqual(name, operation)
                        self.assertEqual(image.size, (256, 256))
                        self.assertEqual(image.mode, "RGB")
                        self.assertEqual(np.asarray(image).dtype, np.uint8)
                        self.assertNotEqual(image.tobytes(), previous)
                        previous = image.tobytes()
                    self.assertNotEqual(first[-1][1].tobytes(), original)
        self.assertEqual(self.image.tobytes(), original)

    def test_family_selection_stays_inside_palette_and_varies_with_seed(self):
        for family, operations in FAMILIES.items():
            seen = set()
            for seed in range(64):
                name, _ = next(states(self.image, .25, seed, family=family))
                self.assertIn(name, operations)
                seen.add(name)
            self.assertEqual(seen, set(operations))
        a = list(states(self.image, .25, 10, operation="stroke"))[-1][1]
        b = list(states(self.image, .25, 11, operation="stroke"))[-1][1]
        self.assertNotEqual(a.tobytes(), b.tobytes())

    def test_small_moves_have_bounded_spatial_or_color_change(self):
        before = np.asarray(self.image).astype(np.int16)
        for operation in OPERATIONS:
            for seed in range(8):
                with self.subTest(operation=operation, seed=seed):
                    result = list(states(self.image, .25, seed, operation=operation))[-1][1]
                    delta = np.abs(np.asarray(result).astype(np.int16)-before)
                    if operation in FAMILIES["classic-color"]:
                        self.assertLess(delta.mean(), 20)
                    else:
                        self.assertLess(np.any(delta, axis=2).mean(), .1)
        # A larger local footprint, rather than merely a different random seed.
        for operation in ("sort", "diffuse", "hatch"):
            changed = []
            for strength in STRENGTHS:
                result = list(states(self.image, strength, 30, operation=operation))[-1][1]
                changed.append(np.any(np.asarray(result) != np.asarray(self.image), axis=2).sum())
            self.assertLess(changed[0], changed[1])
            self.assertLess(changed[1], changed[2])

    def test_blank_black_and_noise_all_can_evolve(self):
        noise = Image.fromarray(np.random.default_rng(14).integers(0, 256, (256, 256, 3), dtype=np.uint8))
        for source in (Image.new("RGB", (256, 256), "white"), Image.new("RGB", (256, 256), "black"),
                       Image.new("RGB", (256, 256), (127, 128, 127)), noise):
            for operation in OPERATIONS:
                with self.subTest(color=source.getpixel((0, 0)), operation=operation):
                    updates = list(states(source, .25, 0, operation=operation))
                    self.assertNotEqual(updates[-1][1].tobytes(), source.tobytes())
        updates = list(states(Image.new("RGB", (256, 256)), .25, 8, operation="slice"))
        self.assertEqual([name for name, _ in updates], ["seed"])
        self.assertLess(np.any(np.asarray(updates[-1][1]), axis=2).sum(), 40)
        gray = Image.new("RGB", (256, 256), (127, 128, 127))
        seeded = list(states(gray, .25, 8, operation="slice"))[-1][1]
        self.assertEqual(np.abs(np.asarray(seeded).astype(int)-np.asarray(gray)).max(), 96)

    def test_invalid_strength_seed_family_operation_and_size_are_rejected(self):
        for strength in (0, -.25, .1, 1, 2, float("nan"), float("inf"), True):
            with self.assertRaises(ValueError): list(states(self.image, strength, 0))
        for seed in (-1, 2**63, .5, True, None):
            with self.assertRaises(ValueError): list(states(self.image, .25, seed))
        with self.assertRaises(ValueError): list(states(self.image, .25, 0, family="network"))
        with self.assertRaises(ValueError): list(states(self.image, .25, 0, family="classic-color", operation="stroke"))
        with self.assertRaises(ValueError): list(states(Image.new("RGB", (512, 256)), .25, 0))

    def test_observation_preserves_output_and_receipts_identify_actual_states(self):
        for family in FAMILIES:
            frames = []
            observed = move(None, None, self.before, self.root / family, 1, .5, 77, family=family, observe=frames.append)
            unobserved = move(None, None, self.before, self.root / family, 2, .5, 77, family=family)
            self.assertEqual(sha(observed), sha(unobserved))
            self.assertGreater(len(frames), 1)
            self.assertEqual(len({f["state_sha256"] for f in frames}), len(frames))
            self.assertEqual(sha(HERE / frames[-1]["image"]), sha(observed))
            for index, frame in enumerate(frames):
                self.assertEqual(frame["index"], index)
                self.assertEqual(frame["step"], index+1)
                self.assertFalse(frame["approximate_decode"])
            receipt = json.loads(observed.with_suffix(".json").read_text())
            self.assertEqual(receipt["status"], "complete")
            self.assertEqual(receipt["identity"]["model"], ENGINES[family]["model"])
            self.assertEqual(receipt["actual_updates"], len(frames))
            self.assertEqual(receipt["billing"]["braincells"], 0)
            self.assertEqual(receipt["output_sha256"], sha(observed))

    def test_cancellation_before_work_and_from_preview_never_publishes_final(self):
        cancel = threading.Event()
        cancel.set()
        folder = self.root / "pre-cancelled"
        with self.assertRaises(MoveCancelled):
            move(None, None, self.before, folder, 1, .25, 6, cancel=cancel)
        self.assertFalse(folder.exists())
        cancel.clear()
        folder = self.root / "cancel-preview"
        frames = []
        def observe(frame):
            frames.append(frame)
            cancel.set()
        with self.assertRaises(MoveCancelled):
            move(None, None, self.before, folder, 1, .5, 8, observe=observe, cancel=cancel)
        self.assertEqual(len(frames), 1)
        self.assertFalse((folder / "001.png").exists())
        self.assertFalse((folder / "001.tmp.png").exists())
        receipt = json.loads((folder / "001.json").read_text())
        self.assertEqual(receipt["status"], "cancelled")
        self.assertEqual(receipt["actual_updates"], 1)

    def test_existing_move_cannot_be_overwritten(self):
        folder = self.root / "moves"
        path = move(None, None, self.before, folder, 1, .25, 42)
        original = sha(path)
        with self.assertRaises(RuntimeError): move(None, None, self.before, folder, 1, .25, 43)
        self.assertEqual(sha(path), original)

    def test_catalog_is_available_without_account_funding_or_service(self):
        offline = {"connected": False, "stale": True, "remaining": 0, "purchased": 0,
                   "remote_service": {"available": False}}
        rows = {row["id"]: row for row in catalog(offline)}
        for family in FAMILIES:
            self.assertTrue(available(family, offline))
            self.assertTrue(rows[family]["available"])
            self.assertEqual(rows[family]["braincells"], 0)
            self.assertEqual(rows[family]["location"], "local · CPU")


if __name__ == "__main__":
    unittest.main()
