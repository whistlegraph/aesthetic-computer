import sys
import importlib

# Check if the module is already loaded
if "aesthetic" in sys.modules:
    importlib.reload(sys.modules["aesthetic"])

from IPython.display import display, IFrame, HTML

def show(piece, width="100%", height=54):
    importlib.reload(importlib.import_module('aesthetic'))
    # Ensure parameters are separated explicitly in the URL
    # url = f"https://aesthetic.computer/{piece}?nolabel&nogap"
    url = f"https://localhost:8888/{piece}?nolabel&nogap"
    display(IFrame(src=url, width=width, height=height, frameborder="0"))

def show_side_by_side(*pieces_and_options):
    """
    Display multiple pieces side by side in a horizontal layout.
    
    Args:
        *pieces_and_options: Can be either:
            - Just piece names as strings: show_side_by_side("clock:4", "clock:5")
            - Tuples of (piece, width, height): show_side_by_side(("clock:4", 200, 100), ("clock:5", 150, 75))
            - Mix of both
    """
    importlib.reload(importlib.import_module('aesthetic'))
    
    iframes_html = []
    
    for item in pieces_and_options:
        if isinstance(item, tuple):
            piece, width, height = item
        else:
            piece = item
            width = 200
            height = 100
            
        url = f"https://localhost:8888/{piece}?nolabel&nogap"
        iframe_html = f'<iframe src="{url}" width="{width}" height="{height}" frameborder="0" style="margin: 0; padding: 0; border: none;"></iframe>'
        iframes_html.append(iframe_html)
    
    # Create a container div with flexbox layout and no gaps
    container_html = f'''
    <div style="display: flex; flex-wrap: wrap; align-items: flex-start; gap: 0; margin: 0; padding: 0;">
        {"".join(iframes_html)}
    </div>
    '''
    
    display(HTML(container_html))