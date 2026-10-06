import base64
import json
from pathlib import Path
import tempfile
import unittest
from PIL import Image, ImageDraw, ImageChops
from local_run import HERE
from paint_region import Region, save_mask, encode_mask, decode_mask

class Regions(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(dir=HERE/'local', prefix='test-region-')
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.before = self.root/'before.png'
        im = Image.new('RGB', (256,256), (255,0,0))
        ImageDraw.Draw(im).rectangle((72,88,143,159), fill=(0,100,0))
        im.save(self.before)
        mask = Image.new('1', (256,256))
        draw=ImageDraw.Draw(mask)
        draw.rectangle((80,96,135,151), fill=1)
        draw.rectangle((95,110,110,125), fill=0)
        self.bits=base64.b64encode(mask.tobytes()).decode()
        self.mask=save_mask(self.bits, self.root/'masks')
        self.generated=self.root/'generated.png'
        Image.new('RGB',(256,256),(0,0,255)).save(self.generated)

    def test_mask_roundtrip_and_validation(self):
        self.assertEqual(encode_mask(self.mask),self.bits)
        self.assertEqual(decode_mask(self.bits).getpixel((80,96)),255)
        for value in ('bad!', '', base64.b64encode(b'0'*8191).decode(), True):
            with self.assertRaises(ValueError): decode_mask(value)
        self.assertIsNone(save_mask(base64.b64encode(bytes(8192)).decode(),self.root))
        self.assertIsNone(save_mask(None,self.root))

    def test_native_and_crop_preserve_every_unpainted_pixel_including_holes(self):
        for engine in ('evolve','fleet-evolve','turbo','ac-openrouter:test'):
            region=Region(self.before,self.mask,engine,self.root/engine.replace(':','_'))
            native=engine in ('evolve','fleet-evolve')
            self.assertEqual(region.kwargs,{'mask':self.mask} if native else {})
            if native:self.assertEqual(region.input,self.before)
            else:
                self.assertNotEqual(region.input,self.before)
                with Image.open(region.input) as cropped:
                    self.assertEqual(cropped.size,(256,256))
                    self.assertEqual(cropped.getpixel((0,0)),(0,100,0))
                self.assertEqual(region.box,(72,88,144,160))
            frame=region.preview({'image':str(self.generated.relative_to(HERE)),'index':0})
            result=region.finish(self.generated)
            for path in (HERE/frame['image'],result):
                diff=ImageChops.difference(Image.open(path),Image.open(self.before))
                outside=Image.composite(diff,Image.new('RGB',(256,256)),ImageChops.invert(Image.open(self.mask)))
                self.assertIsNone(outside.getbbox())
                self.assertEqual(Image.open(path).getpixel((80,96)),(0,0,255))
                self.assertEqual(Image.open(path).getpixel((100,115)),(0,100,0))
            receipt=json.loads((region.folder/'region.json').read_text())
            self.assertTrue(receipt['outside_mask_preserved'])

    def test_unmasked_input_and_output_pass_through(self):
        region=Region(self.before,None,'turbo',self.root/'unmasked')
        self.assertEqual(region.input,self.before)
        self.assertEqual(region.finish(self.generated),self.generated)

if __name__=='__main__':unittest.main()
