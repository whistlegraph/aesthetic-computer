"""Fresh archives every branch; process resumes do not create new paintings."""
import json
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
from PIL import Image
from painting_tracks import PaintingTracks
from play import Game
from local_run import HERE


class Tracks(unittest.TestCase):
    def test_legacy_import_groups_each_fresh_and_done_once(self):
        with tempfile.TemporaryDirectory() as folder:
            events = [{"action": action, "accepted": f"{i}.png", "at": str(i)}
                      for i, action in enumerate(("ready", "paint", "no", "restart", "paint", "done", "resume"))]
            events[5]["publication"] = {"code": "test"}
            tracks = PaintingTracks(folder, events)
            self.assertEqual([p["paints"] for p in tracks.list()], [0, 1, 1])
            self.assertEqual(tracks.list()[1]["publication"], {"code": "test"})
            first = tracks.list()[-1]
            self.assertEqual(first["final"], "2.png")
            self.assertEqual([e.get("action") for e in tracks.read(first["id"])["events"]], ["ready", "paint", "no", None])
            resumed = PaintingTracks(folder, events)
            self.assertEqual(resumed.list(), tracks.list())
            with self.assertRaises(ValueError): resumed.read("../../session.json")


class GameTracks(unittest.TestCase):
    def setUp(self):
        fleet = patch("fleet_run.configuration", return_value=None)
        fleet.start(); self.addCleanup(fleet.stop)
        def generate(before, folder, number, strength, seed, **kwargs):
            folder.mkdir(parents=True, exist_ok=True)
            output = folder / f"{number:03}.png"
            Image.new("RGB", (256, 256), (number, 0, 0)).save(output)
            kwargs["observe"]({"step": 1, "image": str(output.relative_to(HERE))})
            output.with_suffix(".json").write_text(json.dumps({"prompt": "Turn one tile", "identity": {"seed": seed}}))
            return output
        self.game = Game(generator=generate, engine="turbo")
        self.wait()

    def tearDown(self):
        self.game.pool.shutdown(wait=True)
        shutil.rmtree(self.game.folder)

    def wait(self):
        self.game.pool.submit(lambda: None).result(timeout=5)

    def act(self, action, **extra):
        state = self.game.state()
        self.game.act({"action": action, "revision": state["revision"],
                       "generation": state["generation"], "quote": (state.get("quote") or {}).get("id"), **extra})
        self.wait()
        return self.game.state()

    def test_every_fresh_preserves_results_rejected_branch_and_real_states(self):
        first = self.game.track_list()["paintings"][0]["id"]
        base = self.game.state()["accepted"]
        self.act("paint")
        discarded = self.act("paint")["accepted"]
        self.act("no")
        final = self.act("paint")["accepted"]
        self.act("restart", start="blank")
        self.act("restart", start="noise")
        self.assertEqual(len(self.game.track_list()["paintings"]), 3)
        track = self.game.track_read(first)
        self.assertEqual([s["action"] for s in track["steps"]], ["ready", "paint", "paint", "no", "paint"])
        self.assertEqual(track["steps"][0]["image"], base)
        self.assertEqual(track["steps"][2]["image"], discarded)
        self.assertEqual(track["steps"][-1]["image"], final)
        self.assertEqual(track["steps"][-1]["prompt"], "Turn one tile")
        self.assertEqual(track["painting"]["paints"], 3)
        self.assertEqual(len([e for e in track["events"] if e["kind"] == "state"]), 3)
        confirmations = [e for e in track["events"] if e.get("action") == "confirm"]
        self.assertEqual(len(confirmations), 3)
        self.assertIsInstance(confirmations[0]["move"]["seed"], int)
        self.assertEqual(next(e for e in track["events"] if e.get("action") == "no")["rejected_generation"], 2)
        for step in track["steps"]:
            self.assertTrue(self.game.images[step["image"].split("/")[-1][:-4]].is_file())

    def test_restart_process_keeps_current_track_and_does_not_duplicate_steps(self):
        self.act("paint"); self.act("restart", start="noise"); self.act("paint")
        paintings = self.game.track_list()
        identifier = paintings["paintings"][0]["id"]
        steps = self.game.track_read(identifier)["steps"]
        self.game.pool.shutdown(wait=True)
        self.game = Game(generator=self.game.generator, resume=self.game.folder)
        self.wait()
        self.assertEqual(self.game.track_list(), paintings)
        self.assertEqual(self.game.track_read(identifier)["steps"], steps)

    def test_failed_generation_is_retained_in_history(self):
        def fail(*args, **kwargs): raise RuntimeError("Provider unavailable")
        self.game.generator = fail
        self.act("paint")
        track = self.game.track_read(self.game.tracks.current["id"])
        self.assertEqual(track["steps"][-1]["error"], "Provider unavailable")
        self.assertEqual(track["painting"]["paints"], 0)

    def test_done_closes_old_track_with_publication_and_opens_fresh(self):
        self.game.account.value = {"connected": True, "account_id": "test", "handle": "@test"}
        self.act("paint")
        identifier = self.game.tracks.current["id"]
        before = self.game.state()
        with patch("play.bridge", return_value={"verified": True, "code": "test", "route": "https://aesthetic.computer/#test"}):
            self.game.done({"revision": before["revision"], "accepted": before["accepted"]})
            self.wait()
        old = self.game.track_read(identifier)
        self.assertEqual(old["painting"]["publication"]["code"], "test")
        self.assertEqual(old["steps"][-1]["image"], before["accepted"])
        self.assertEqual(len(self.game.track_list()["paintings"]), 2)
        self.assertEqual(self.game.track_read(self.game.tracks.current["id"])["steps"][0]["action"], "done")


if __name__ == "__main__": unittest.main()
