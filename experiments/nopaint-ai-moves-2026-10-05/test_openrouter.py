import unittest
from openrouter_catalog import ModelBrowser, image_models
from engines import catalog, available

class Discovery(unittest.TestCase):
    def test_image_in_out_and_single_reference_are_required(self):
        base = {"id":"test/image", "name":"Image", "architecture":{"input_modalities":["image"],"output_modalities":["image"]},
                "supported_parameters":{"input_references":{"min":0,"max":1}}}
        self.assertEqual(len(image_models([base])),1)
        for change in [{"architecture":{"input_modalities":["text"],"output_modalities":["image"]}},
                       {"supported_parameters":{"input_references":{"min":2,"max":4}}},
                       {"supported_parameters":{"input_references":{"max":1},"output_format":{"values":["svg"]}}}]:
            self.assertEqual(image_models([{**base,**change}]),[])

    def test_browse_is_cached_read_only_and_rejects_host_escape(self):
        calls=[]
        def fetch(path):
            calls.append(path)
            return {"data":[],"endpoints":[]}
        browser=ModelBrowser(fetch)
        self.assertEqual(browser.read(),{"models":[]});browser.read()
        self.assertEqual(calls,[""])
        browser.read("google/test");self.assertEqual(calls[-1],"/google/test/endpoints")
        for model in ("https://host/", "../secret", "google/test?key=x"):
            with self.assertRaises(ValueError): browser.read(model)

    def test_discovered_catalog_never_authorizes_billing(self):
        key="ac-openrouter:test/image"
        self.assertFalse(available(key))
        offer={"id":key,"name":"Test","model":"test/image","location":"AC cloud","previews":False,"available":True,"braincells":10}
        identity={"models":[offer],"connected":True,"remaining":9,"purchased":0}
        self.assertFalse(next(m for m in catalog(identity) if m["id"]==key)["available"])
        identity["remaining"]=10
        self.assertTrue(available(key,identity))
