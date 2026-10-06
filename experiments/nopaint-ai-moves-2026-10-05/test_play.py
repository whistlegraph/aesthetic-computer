"""Confirm exactly one move; No and setup actions never start inference."""
import copy
import json
import shutil
import threading
import unittest
from unittest.mock import patch
from PIL import Image
from play import Conflict, Game
from move_control import MoveCancelled
from local_run import HERE


class DecisionLoop(unittest.TestCase):
    def setUp(self):
        fleet = patch("fleet_run.configuration", return_value=None)
        fleet.start(); self.addCleanup(fleet.stop)
        self.inputs = []
        def generate(before, folder, number, strength, seed, **kwargs):
            self.inputs.append(before)
            folder.mkdir(parents=True, exist_ok=True)
            output = folder / f"{number:03}.png"
            Image.new("RGB", (256, 256), (number, 0, 0)).save(output)
            return output
        self.game = Game(generator=generate, engine="turbo")
        self.wait()

    def tearDown(self):
        self.game.pool.shutdown(wait=True)
        shutil.rmtree(self.game.folder)

    def wait(self, error=False):
        self.game.pool.submit(lambda: None).result(timeout=5)
        self.assertFalse(self.game.state()["busy"])
        if not error: self.assertIsNone(self.game.state()["error"])

    def choose(self, action, **extra):
        state = self.game.state()
        self.game.act({"action": action, "revision": state["revision"],
                       "quote": (state.get("quote") or {}).get("id"),
                       "generation": state["generation"], **extra})
        self.wait()
        return self.game.state()

    def test_initial_quote_never_loads_or_generates(self):
        state = self.game.state()
        self.assertEqual(self.inputs, [])
        self.assertIsNone(self.game.loaded_engine)
        self.assertIsNotNone(state["quote"])
        self.assertIsNone(state["quote"]["estimated_seconds"])
        self.assertFalse(state["can_reject"])
        with self.assertRaises(Conflict): self.choose("no")

    def test_paint_applies_one_move_and_pauses_no_steps_back(self):
        initial = self.game.state()
        first = self.choose("paint")
        self.assertEqual(len(self.inputs), 1)
        self.assertEqual(first["accepted"], first["candidate"]["url"])
        self.assertEqual(first["candidate"]["input"], initial["accepted"])
        self.assertEqual(first["quote"]["input"], first["accepted"])
        self.assertEqual(first["accepted_count"], 1)
        second = self.choose("paint")
        self.assertEqual(len(self.inputs), 2)
        self.assertEqual(second["candidate"]["input"], first["accepted"])
        back = self.choose("no")
        self.assertEqual(back["accepted"], first["accepted"])
        self.assertEqual(len(self.inputs), 2)
        back = self.choose("no")
        self.assertEqual(back["accepted"], initial["accepted"])
        self.assertFalse(back["can_undo"])
        self.assertEqual(len(self.inputs), 2)

    def test_only_exact_quote_can_start_and_double_click_cannot_pay_twice(self):
        initial = self.game.state()
        request = {"action":"paint", "revision":initial["revision"], "quote":initial["quote"]["id"]}
        with self.assertRaises(Conflict): self.game.act({**request, "quote":"stale"})
        with self.assertRaises(Conflict): self.game.act({"action":"paint", "revision":initial["revision"]})
        with self.game.lock:
            self.game.act(request)
            with self.assertRaises(Conflict): self.game.act(request)
        self.wait()
        with self.assertRaises(Conflict): self.game.act(request)
        self.assertEqual(len(self.inputs), 1)

    def test_settings_fresh_and_retry_only_prepare_a_quote(self):
        state = self.choose("strength", strength=.5)
        self.assertEqual(state["quote"]["strength"], .5)
        self.choose("engine", engine="evolve")
        for start in ["noise", "blank", "base"]:
            state = self.choose("restart", start=start)
            self.assertEqual(state["quote"]["input"], state["accepted"])
        self.assertEqual(self.inputs, [])
        self.game.generator = lambda *args, **kwargs: (_ for _ in ()).throw(RuntimeError("Offline"))
        state = self.game.state()
        self.game.act({"action":"paint", "revision":state["revision"], "quote":state["quote"]["id"]})
        self.wait(error=True)
        retry = self.choose("retry")
        self.assertIsNotNone(retry["quote"])
        self.assertEqual(self.inputs, [])

    def test_resume_preserves_accepted_history_but_never_replays_confirmation(self):
        self.choose("paint")
        before = self.choose("strength", strength=.5)
        self.game.pool.shutdown(wait=True)
        self.game = Game(generator=self.game.generator, resume=self.game.folder)
        self.wait()
        after = self.game.state()
        self.assertEqual(after["accepted"], before["accepted"])
        self.assertEqual(after["accepted_count"], before["accepted_count"])
        self.assertEqual(after["strength"], .5)
        self.assertNotEqual(after["quote"]["id"], before["quote"]["id"])
        self.assertEqual(len(self.inputs), 1)

    def blocked_move(self):
        original = self.game.generator
        started, release = threading.Event(), threading.Event()
        self.addCleanup(release.set)
        number = self.game.attempt + 1
        self.late_preview_rejected = False
        def generate(before, folder, step, strength, seed, **kwargs):
            output = original(before, folder, step, strength, seed, **kwargs)
            if step == number:
                frame = {"image":str(output.relative_to(HERE)), "step":1}
                kwargs["observe"](frame)
                started.set()
                if not release.wait(5): raise RuntimeError("Test never released")
                try: kwargs["observe"](frame)
                except MoveCancelled: self.late_preview_rejected = True
            return output
        self.game.generator = generate
        state = self.game.state()
        self.game.act({"action":"paint", "revision":state["revision"], "quote":state["quote"]["id"]})
        self.assertTrue(started.wait(5))
        return self.game.state(), release

    def test_no_during_generation_restores_input_and_fences_late_frames(self):
        self.choose("paint")
        state, release = self.blocked_move()
        try:
            self.assertNotEqual(state["trace"]["latest"]["url"], state["accepted"])
            decision = {"action":"no", "revision":state["revision"], "generation":state["generation"]}
            stopped = self.game.act(decision)
            self.assertEqual(stopped["accepted"], state["accepted"])
            self.assertIsNone(stopped["trace"]["latest"])
            self.assertIsNotNone(stopped["quote"])
            with self.assertRaises(Conflict): self.game.act(decision)
        finally: release.set()
        self.wait()
        self.assertTrue(self.late_preview_rejected)
        self.assertEqual(len(self.inputs), 2)
        self.assertEqual(self.game.state()["accepted"], state["accepted"])

    def test_model_switch_does_not_mix_previous_previews_or_generate(self):
        self.choose("paint")
        state = self.game.state()
        switched = self.choose("engine", engine="evolve")
        self.assertEqual(switched["accepted"], state["accepted"])
        self.assertIsNone(switched["trace"]["latest"])
        self.assertEqual(switched["quote"]["engine"], "evolve")
        self.assertEqual(len(self.inputs), 1)
        generated = self.choose("paint")
        self.assertEqual(generated["candidate"]["engine"], "evolve")
        self.assertEqual(generated["candidate"]["input"], state["accepted"])
        self.assertEqual(len(self.inputs), 2)

    def cloud_account(self):
        offer = {"id":"ac-klein", "name":"Klein", "location":"AC cloud", "model":"fal-ai/flux-2/klein/4b/edit", "previews":False, "available":True, "braincells":4000, "quote":"price-1"}
        self.game.account.value = {"connected":True, "account_id":"account-1", "handle":"@test", "remaining":50000, "purchased":0, "remote":offer, "models":[offer]}
        return offer

    def test_cloud_quote_is_displayed_before_any_work_and_must_be_reconfirmed_if_price_changes(self):
        offer = self.cloud_account()
        state = self.choose("engine", engine="ac-klein")
        self.assertEqual(state["quote"]["braincells"], 4000)
        self.assertEqual(self.inputs, [])
        offer["braincells"] = 8000; offer["quote"] = "price-2"
        with self.assertRaises(Conflict):
            self.game.act({"action":"paint", "revision":state["revision"], "quote":state["quote"]["id"]})
        self.assertEqual(self.inputs, [])
        self.assertEqual(self.game.state()["quote"]["braincells"], 8000)
        self.choose("paint")
        self.assertEqual(len(self.inputs), 1)

    def test_account_change_cannot_confirm_another_accounts_quote(self):
        self.cloud_account()
        state = self.choose("engine", engine="ac-klein")
        self.game.account.value = {**self.game.account.value, "account_id":"account-2"}
        with self.assertRaises(Conflict): self.game.act({"action":"paint", "revision":state["revision"], "quote":state["quote"]["id"]})
        self.assertEqual(self.inputs, [])

    def test_unavailable_remote_can_be_selected_without_work_and_recovers_to_confirmation(self):
        from engines import ENGINES, register_image_models
        model = "test/selectable-image"
        key = "ac-openrouter:" + model
        register_image_models([{"id":model, "name":"Selectable image"}])
        self.addCleanup(lambda: ENGINES.pop(key, None))
        self.cloud_account()
        self.game.account.value["remote_service"] = {"available":False,"code":"provider_funding"}
        before = self.game.state()["accepted"]
        state = self.choose("engine", engine=key)
        self.assertEqual(state["engine"], key)
        self.assertEqual(state["selection"], key)
        self.assertEqual(state["accepted"], before)
        self.assertIsNone(state["quote"])
        self.assertEqual(self.inputs, [])
        with self.assertRaises(Conflict): self.choose("paint")
        self.game.account.value["models"] = [{"id":key,"model":model,"name":"Selectable image","location":"AC cloud", "previews":False,"available":True,"braincells":4000,"quote":"price"}]
        self.game.account.value["remote_service"] = {"available":True,"code":"ready"}
        recovered = self.game.state()
        self.assertEqual(recovered["selection"], key)
        self.assertEqual(recovered["quote"]["braincells"], 4000)
        self.assertEqual(self.inputs, [])
        self.game.account.value["remote_service"] = {"available":False,"code":"provider_funding"}
        self.assertIsNone(self.game.state()["quote"])
        self.assertEqual(self.game.state()["accepted"], before)

    def test_random_choices_are_quoted_before_execution(self):
        with patch("play.secrets.choice", side_effect=["evolve", "turbo"]):
            ready = self.choose("engine", engine="random")
            self.assertEqual(self.inputs, [])
            done = self.choose("paint")
        self.assertEqual(ready["quote"]["engine"], "evolve")
        self.assertEqual(done["candidate"]["engine"], "evolve")
        self.assertEqual(done["quote"]["engine"], "turbo")
        self.assertEqual(len(self.inputs), 1)

    def test_local_actions_never_contact_ac(self):
        with patch("ac_run.bridge") as network:
            self.game.account.update()
            self.choose("paint"); self.choose("no")
            network.assert_not_called()

    def test_done_creates_new_identity_after_each_success_and_fresh_canvas(self):
        import json
        self.game.account.value = {"connected": True, "account_id": "test", "handle": "@test"}
        calls = []
        def publish(data, **kwargs):
            from pathlib import Path
            snapshot = json.loads((Path(data['folder'])/'manifest.json').read_text())
            calls.append(snapshot)
            return {"verified": True, "code": 'abc', "route": 'https://aesthetic.computer/#abc'}
        with patch('play.bridge', side_effect=publish):
            for _ in range(2):
                before = self.game.state()
                request = {"revision": before['revision'], "accepted": before['accepted']}
                with self.game.lock:
                    self.game.done(request)
                    with self.assertRaises(Conflict): self.game.done(request)
                    with self.assertRaises(Conflict): self.game.act({"action": "restart"})
                self.game.pool.submit(lambda:None).result(timeout=5)
                self.wait()
                self.assertNotEqual(self.game.state()['accepted'], before['accepted'])
                self.assertEqual(self.game.state()['accepted_count'], 0)
        self.assertNotEqual(calls[0]['id'], calls[1]['id'])
        self.assertEqual(calls[0]['history'][-1]['sha256'], calls[0]['history'][0]['sha256'])

    def test_brush_uses_accepted_canvas_and_rejects_stale_strokes(self):
        import base64
        bits = base64.b64encode(bytes([128])+bytes(8191)).decode()
        before = self.game.state()
        self.game.act({"action": "mask", "revision": -1, "accepted": before['accepted'], "mask": bits})
        self.wait()
        state = self.game.state()
        self.assertEqual(state['accepted'], before['accepted'])
        self.assertEqual(state['mask'], bits)
        self.assertEqual(state['quote']['input'], before['accepted'])
        painted = self.choose('paint')
        with self.assertRaises(Conflict):
            self.game.act({"action": "mask", "accepted": before['accepted'], "mask": bits})
        fresh = self.choose('restart', start='noise')
        self.assertIsNone(fresh['mask'])

    def test_crop_changes_bitmap_and_next_model_input_and_is_undoable(self):
        from PIL import ImageChops
        from local_run import HERE
        before = self.game.state()
        original = self.game.history[-1]
        box = [32, 64, 160, 192]
        expected = Image.open(original).convert('RGB').crop(box).resize((256,256),Image.Resampling.NEAREST)
        self.game.act({"action": "crop", "accepted": before['accepted'], "box": box})
        self.wait()
        state = self.game.state()
        self.assertNotEqual(state['accepted'], before['accepted'])
        self.assertEqual(state['quote']['input'], state['accepted'])
        self.assertIsNone(ImageChops.difference(Image.open(self.game.history[-1]).convert('RGB'), expected).getbbox())
        self.assertEqual(self.inputs, [])
        self.assertEqual(self.choose('undo')['accepted'], before['accepted'])
        for box in ([0,0,0,10],[-1,0,128,128],[0,0,300,128],[0,0,1.5,128],None):
            with self.assertRaises(ValueError):
                self.game.act({"action": "crop", "accepted": before['accepted'], "box": box})

    def test_fresh_can_leave_failed_done_without_losing_its_recovery_receipt(self):
        self.game.account.value = {"connected": True, "account_id": "test", "handle": "@test"}
        before = self.game.state()
        with patch('play.bridge', side_effect=RuntimeError('Service unavailable')):
            self.game.done({"revision": before['revision'], "accepted": before['accepted']})
            self.wait()
        from local_run import HERE
        folder = HERE / self.game.pending_done
        fresh = self.choose('restart', start='noise')
        self.assertNotEqual(fresh['accepted'], before['accepted'])
        self.assertFalse(fresh['publication']['pending'])
        self.assertTrue((folder/'manifest.json').is_file())

    def test_done_failure_preserves_canvas_and_retries_same_snapshot_after_resume(self):
        self.game.account.value = {"connected": True, "account_id": "test", "handle": "@test"}
        before = self.game.state()
        with patch('play.bridge', side_effect=RuntimeError('Lost response')) as publish:
            self.game.done({"revision": before['revision'], "accepted": before['accepted']})
            self.wait()
            folder = publish.call_args.args[0]['folder']
        self.assertEqual(self.game.state()['accepted'], before['accepted'])
        self.game.pool.shutdown(wait=True)
        self.game = Game(generator=self.game.generator, resume=self.game.folder, account=self.game.account)
        self.wait()
        with patch('play.bridge', return_value={"verified": True, "code": 'abc', "route": 'https://aesthetic.computer/#abc'}) as publish:
            state = self.game.state()
            self.game.done({"revision": state['revision'], "accepted": state['accepted']})
            self.game.pool.submit(lambda:None).result(timeout=5); self.wait()
            self.assertEqual(publish.call_args.args[0]['folder'], folder)


if __name__ == "__main__":
    unittest.main()
