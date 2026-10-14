import unittest
from web_pairing import allowed, cors, connect_page


class PairingTests(unittest.TestCase):
    def test_origin_and_pairing_both_required(self):
        headers = {"Host": "127.0.0.1:8767", "Origin": "https://nopaint.art"}
        self.assertFalse(allowed(headers, 8767, "secret"))
        headers["X-NoPaint-Pairing"] = "secret"
        self.assertTrue(allowed(headers, 8767, "secret"))
        headers["Origin"] = "https://evil.example"
        self.assertFalse(allowed(headers, 8767, "secret"))
        self.assertEqual(cors(headers), {})
        headers["Origin"] = "https://nopaint.art"
        headers["Host"] = "rebinding.example:8767"
        self.assertFalse(allowed(headers, 8767, "secret"))

    def test_native_and_local_browser_still_work(self):
        self.assertTrue(allowed({"Host": "127.0.0.1:8767"}, 8767, "secret"))
        self.assertTrue(allowed({"Host": "127.0.0.1:8767", "Origin": "http://127.0.0.1:8767"}, 8767, "secret"))

    def test_secret_is_a_fragment_and_no_wildcard_cors(self):
        self.assertIn("/#companion=secret", connect_page("secret"))
        self.assertIn('rel="noreferrer"', connect_page("secret"))
        self.assertEqual(cors({"Origin": "https://nopaint.art"})["Access-Control-Allow-Origin"], "https://nopaint.art")


if __name__ == "__main__":
    unittest.main()
