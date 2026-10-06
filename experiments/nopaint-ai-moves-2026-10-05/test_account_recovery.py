import threading
import time
import unittest
from unittest.mock import patch
from ac_run import Account, AccountError
from engines import available


class AccountRecovery(unittest.TestCase):
    def wait(self, account):
        deadline = time.monotonic()+2
        while account.snapshot()["working"] and time.monotonic()<deadline:
            time.sleep(.005)
        self.assertFalse(account.snapshot()["working"])

    def test_timeout_preserves_identity_and_recovers_without_login(self):
        account = Account()
        good = {"connected":True,"handle":"@test","account_id":"owner","remaining":50000,"purchased":100,
                "models":[{"id":"ac-klein","available":True,"braincells":4000}]}
        account.value = good.copy()
        with patch("ac_run.bridge", side_effect=AccountError("Timed out", "offline")):
            account.update(); self.wait(account)
        offline = account.snapshot()
        self.assertTrue(offline["connected"]); self.assertTrue(offline["reconnecting"])
        self.assertEqual(offline["handle"],good["handle"])
        self.assertEqual(offline["remaining"],50000)
        self.assertFalse(available("ac-klein", offline))
        with patch("ac_run.bridge", return_value=good.copy()) as network:
            account.update(); self.wait(account)
            self.assertEqual(network.call_args.args[0], {"action":"status"})
        recovered=account.snapshot()
        self.assertNotIn("error",recovered); self.assertNotIn("stale",recovered)
        self.assertTrue(available("ac-klein",recovered))

    def test_initial_timeout_allows_status_retry_but_explicit_signout_stops_it(self):
        account=Account()
        with patch("ac_run.bridge",side_effect=AccountError("Timed out","offline")):
            account.update(login=True);self.wait(account)
        self.assertTrue(account.snapshot()["reconnecting"])
        with patch("ac_run.bridge",return_value={"connected":True}) as network:
            account.update();self.wait(account);network.assert_called_once()
        account.disconnect()
        with patch("ac_run.bridge") as network:
            account.update();network.assert_not_called()

    def test_rejected_auth_is_not_treated_as_an_offline_connection(self):
        account=Account();account.value={"connected":True,"handle":"@test"}
        with patch("ac_run.bridge",side_effect=AccountError("Sign in again","account-unverified")):
            account.update();self.wait(account)
        self.assertFalse(account.snapshot()["connected"])
        self.assertNotIn("reconnecting",account.snapshot())

    def test_signout_during_refresh_cannot_be_undone_by_late_response(self):
        account=Account();account.value={"connected":True}
        entered=threading.Event();release=threading.Event();finished=threading.Event()
        def request(*args,**kwargs):
            entered.set();release.wait(2);finished.set();return {"connected":True,"handle":"@test"}
        with patch("ac_run.bridge",side_effect=request):
            account.update();self.assertTrue(entered.wait(1));account.disconnect();release.set()
            self.assertTrue(finished.wait(1))
        self.assertFalse(account.snapshot()["connected"])
