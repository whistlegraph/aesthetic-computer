import concurrent.futures
import sys
import threading
import unittest

from account_bridge import AccountBridge, AccountError
from move_control import MoveCancelled


WORKER = """
import json,os,sys,threading,time
sequence=0
lock=threading.Lock()
def reply(envelope,sequence):
    data=envelope['input']
    if data.get('crash'): os._exit(7)
    time.sleep(data.get('delay',0))
    result={'id':envelope['id'],'result':{'pid':os.getpid(),'sequence':sequence,'tag':data.get('tag')}}
    with lock: print(json.dumps(result),flush=True)
for line in sys.stdin:
    sequence+=1
    threading.Thread(target=reply,args=(json.loads(line),sequence),daemon=True).start()
"""


class PersistentAccountBridge(unittest.TestCase):
    def setUp(self):
        self.client = AccountBridge([sys.executable, "-u", "-c", WORKER])
        self.addCleanup(self.client.close)

    def test_calls_keep_the_same_client(self):
        first = self.client.call({"tag": "status"}, timeout=2)
        second = self.client.call({"tag": "move"}, timeout=2)
        self.assertEqual(first["pid"], second["pid"])
        self.assertEqual([first["sequence"], second["sequence"]], [1, 2])

    def test_overlapping_replies_return_to_their_callers(self):
        with concurrent.futures.ThreadPoolExecutor(2) as pool:
            slow = pool.submit(self.client.call, {"tag": "slow", "delay": .2}, 2)
            fast = pool.submit(self.client.call, {"tag": "fast"}, 2)
            self.assertEqual(fast.result()["tag"], "fast")
            self.assertEqual(slow.result()["tag"], "slow")

    def test_no_discards_late_move_without_closing_other_connections(self):
        first = self.client.call({}, timeout=2)
        cancel = threading.Event()
        timer = threading.Timer(.03, cancel.set)
        timer.start()
        try:
            with self.assertRaises(MoveCancelled):
                self.client.call({"tag": "paid", "delay": .3}, timeout=2, cancel=cancel)
        finally:
            timer.cancel()
        after = self.client.call({"tag": "status", "delay": .3}, timeout=2)
        self.assertEqual(after["pid"], first["pid"])
        self.assertEqual(after["tag"], "status")
        self.assertEqual(after["sequence"], 3)

    def test_crash_fails_the_move_and_reconnects_without_replaying_it(self):
        first = self.client.call({}, timeout=2)
        with self.assertRaises(AccountError):
            self.client.call({"crash": True}, timeout=2)
        after = self.client.call({"tag": "status"}, timeout=2)
        self.assertNotEqual(first["pid"], after["pid"])
        self.assertEqual(after["sequence"], 1)
