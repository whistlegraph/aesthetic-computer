import sys
import importlib
import urllib.parse

# Check if the module is already loaded
if "aesthetic" in sys.modules:
    importlib.reload(sys.modules["aesthetic"])

from IPython.display import display, IFrame, HTML

def show(piece, width="100%", height=54):
    importlib.reload(importlib.import_module('aesthetic'))
    # Ensure parameters are separated explicitly in the URL
    # url = f"https://aesthetic.computer/{piece}?nolabel&nogap"
    url = f"https://localhost:8888/{piece}?nolabel=true"
    display(IFrame(src=url, width=width, height=height, frameborder="0"))

def kidlisp(code, width="100%", height=500, auto_scale=False):
    """
    Run kidlisp code in Aesthetic Computer.
    
    Args:
        code (str): The kidlisp code to execute
        width: Width of the iframe (default: "100%")
        height: Height of the iframe (default: 400)
        auto_scale (bool): If True, iframe will scale to fit content (default: False)
    """
    importlib.reload(importlib.import_module('aesthetic'))
    
    # Use kidlisp-specific encoding with proper quote handling
    # Clean up the code first, then apply encoding
    clean_code = code.strip()
    encoded_code = clean_code.replace('"', '%22').replace(' ', '_').replace('\n', '§')
    
    # Create URL with proper query parameter format
    url = f"https://localhost:8888/{encoded_code}?nolabel=true&nogap=true"
    
    # Always use HTML approach to ensure black background
    if auto_scale:
        iframe_html = f'''
        <iframe src="{url}" 
                width="{width}" 
                height="{height}" 
                frameborder="0"
                style="background-color: black; transform-origin: top left; max-width: 100%; height: auto; aspect-ratio: 1/1;">
        </iframe>
        '''
    else:
        iframe_html = f'''
        <iframe src="{url}" 
                width="{width}" 
                height="{height}" 
                frameborder="0"
                style="background-color: black;">
        </iframe>
        '''
    
    display(HTML(iframe_html))

def kidlisp_display(code, width="100%", height=400, auto_scale=False):
    """
    Run kidlisp code with minimal visual footprint - designed for display-only cells.
    
    Args:
        code (str): The kidlisp code to execute
        width: Width of the iframe (default: "100%")
        height: Height of the iframe (default: 400)
        auto_scale (bool): If True, iframe will scale to fit content (default: False)
    """
    # This function is identical to kidlisp() but with a different name
    # The "display-only" aspect comes from how you use it in your notebook
    return kidlisp(code, width, height, auto_scale)

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
            
        url = f"https://localhost:8888/{piece}?nolabel=true&nogap=true"
        iframe_html = f'<iframe src="{url}" width="{width}" height="{height}" frameborder="0" style="margin: 0; padding: 0; border: none;"></iframe>'
        iframes_html.append(iframe_html)
    
    # Create a container div with flexbox layout and no gaps
    container_html = f'''
    <div style="display: flex; flex-wrap: wrap; align-items: flex-start; gap: 0; margin: 0; padding: 0;">
        {"".join(iframes_html)}
    </div>
    '''
    
    display(HTML(container_html))

# Short aliases for kidlisp function
def k(code, width="100%", height=None, auto_scale=False):
    """Ultra-short alias for kidlisp function"""
    return λ(code, width, height, auto_scale)

def _(code, width="100%", height=None, auto_scale=False):
    """Single underscore alias for kidlisp function"""
    return λ(code, width, height, auto_scale)

# Even shorter - single character functions
def l(code, width="100%", height=None, auto_scale=False):
    """Single letter 'l' for lisp"""
    return λ(code, width, height, auto_scale)

# Lambda symbol alias - perfect for functional programming!
def λ(code, width="100%", height=None, auto_scale=False):
    """
    Lambda symbol alias for kidlisp function - λ()
    
    Usage:
        λ("kidlisp code")                           # Auto height: 1px per character (minimum 1px)
        λ("kidlisp code", height=300)              # Custom height
        λ(("kidlisp code", 500, 300))              # Tuple format: (code, width, height)
    """
    # Handle tuple input for resolution
    if isinstance(code, tuple):
        if len(code) == 3:
            # Format: ("code", width, height)
            code, width, height = code
        elif len(code) == 2:
            # Format: ("code", width) - use width for both dimensions
            code, width = code
            height = width
        else:
            raise ValueError("Tuple must have 2 or 3 elements: (code, width[, height])")
    
    # Auto-calculate height based on character count (1px per character, minimum 1px)
    if height is None:
        char_count = len(code.strip())
        height = max(1, char_count)  # Exactly 1px per character, minimum 1px
    
    return kidlisp(code, width, height, auto_scale)

# Automatic IPython/Jupyter setup - makes λ globally available
def _setup_ipython():
    """Automatically setup λ in IPython/Jupyter environment"""
    try:
        from IPython import get_ipython
        ip = get_ipython()
        if ip is not None:
            # Add λ to the user namespace
            ip.user_ns['λ'] = λ
            
            # Also add to the global namespace for pylance
            ip.user_global_ns['λ'] = λ
            
            # Register as a built-in so it's available everywhere
            import builtins
            builtins.λ = λ
            return True
    except ImportError:
        pass
    return False

# Auto-setup when module is imported
_setup_ipython()

# Export all functions for proper IDE support
__all__ = ['show', 'show_side_by_side', 'kidlisp', 'kidlisp_display', 'k', '_', 'l', 'λ']