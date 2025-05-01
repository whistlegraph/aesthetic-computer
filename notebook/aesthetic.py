import sys
import importlib

# Check if the module is already loaded
if "aesthetic" in sys.modules:
    importlib.reload(sys.modules["aesthetic"])

from IPython.display import display, IFrame

def show(piece, width="100%", height=54):
    importlib.reload(importlib.import_module('aesthetic'))
    # Ensure parameters are separated explicitly in the URL
    url = f"https://localhost:8888/{piece}?nolabel&nogap"
    display(IFrame(src=url, width=width, height=height, frameborder="0"))