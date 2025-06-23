import sys
import importlib
import urllib.parse
import hashlib

# Check if the module is already loaded
if "aesthetic" in sys.modules:
    importlib.reload(sys.modules["aesthetic"])

from IPython.display import display, IFrame, HTML
from IPython.core.magic import Magics, cell_magic, line_magic, magics_class

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
    # Properly encode parentheses, dots, and percent signs which cause URI issues
    # Use ~ instead of § to avoid UTF-8 encoding issues with Netlify CLI
    encoded_code = (clean_code
                   .replace('%', '%25')  # Must encode % first to avoid double-encoding
                   .replace('"', '%22')
                   .replace(' ', '_')
                   .replace('\n', '~')   # Use ~ instead of § to avoid UTF-8 issues
                   .replace('(', '%28')
                   .replace(')', '%29')
                   .replace('.', '%2E'))
    
    # Create URL with proper query parameter format
    url = f"https://localhost:8888/{encoded_code}?nolabel=true&nogap=true"
    
    # Create a stable ID based on content hash to reduce flicker on re-runs
    content_hash = hashlib.md5(f"{clean_code}{width}{height}".encode()).hexdigest()[:8]
    iframe_id = f"ac-iframe-{content_hash}"
    
    # Always use HTML approach with no margins, positioned at top-left
    if auto_scale:
        iframe_html = f'''
        <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
            <iframe id="{iframe_id}" src="{url}" 
                    width="{width}" 
                    height="{height}" 
                    frameborder="0"
                    style="background: transparent; margin: 0; padding: 0; border: none; display: block; transform-origin: top left; max-width: 100%; height: auto; aspect-ratio: 1/1;">
            </iframe>
        </div>
        '''
    else:
        iframe_html = f'''
        <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
            <iframe id="{iframe_id}" src="{url}" 
                    width="{width}" 
                    height="{height}" 
                    frameborder="0"
                    style="background: transparent; margin: 0; padding: 0; border: none; display: block;">
            </iframe>
        </div>
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
def k(*args, **kwargs):
    """Ultra-short alias for kidlisp function"""
    return λ(*args, **kwargs)

def _(*args, **kwargs):
    """Single underscore alias for kidlisp function"""
    return λ(*args, **kwargs)

# Even shorter - single character functions
def l(*args, **kwargs):
    """Single letter 'l' for lisp"""
    return λ(*args, **kwargs)

# Lambda symbol alias - perfect for functional programming!
def λ(*args, **kwargs):
    """
    Lambda symbol alias for kidlisp function - λ()
    
    Usage:
        λ("kidlisp code")                           # Default: 100% width, 32px height
        λ("kidlisp code", 300)                      # Single number = height (width stays 100%)
        λ("kidlisp code", 800, 600)                # Two numbers = width, height
        λ("kidlisp code", (800, 600))              # Resolution tuple
        λ(("kidlisp code", 500, 300))              # Tuple format: (code, width, height)
        λ("kidlisp code", resolution=(800, 600))   # Named resolution tuple
    """
    # Default values
    code = None
    width = "100%"
    height = 30
    auto_scale = kwargs.get('auto_scale', False)
    resolution = kwargs.get('resolution', None)
    
    # Parse positional arguments
    if len(args) >= 1:
        code = args[0]
    if len(args) >= 2:
        # Check if second argument is a resolution tuple
        if isinstance(args[1], tuple) and len(args[1]) == 2:
            resolution = args[1]
        elif isinstance(args[1], (int, float)):
            # Single number = height only (width stays 100%)
            height = args[1]
        else:
            width = args[1]
    if len(args) >= 3:
        # Third argument could be height or resolution tuple
        if isinstance(args[2], tuple) and len(args[2]) == 2:
            resolution = args[2]
        else:
            # If we have 3 args and second was a number, then this is width, height
            if isinstance(args[1], (int, float)):
                width = args[1]
                height = args[2]
            else:
                height = args[2]
    if len(args) >= 4:
        auto_scale = args[3]
    
    # Handle tuple input for resolution in first parameter
    if isinstance(code, tuple):
        if len(code) == 3:
            # Format: ("code", width, height)
            code, width, height = code
        elif len(code) == 2:
            # Format: ("code", height) - width stays 100%
            code, height = code
        else:
            raise ValueError("Tuple must have 2 or 3 elements: (code, height[, width])")
    
    # Handle resolution tuple parameter (overrides other width/height settings)
    if resolution is not None:
        if isinstance(resolution, tuple) and len(resolution) == 2:
            width, height = resolution
        else:
            raise ValueError("Resolution must be a tuple of (width, height)")
    
    return kidlisp(code, width, height, auto_scale)

# IPython Magic Commands for Native Kidlisp Syntax
@magics_class
class AestheticComputerMagics(Magics):
    """IPython magic commands for native kidlisp syntax in Jupyter notebooks"""
    
    @cell_magic
    def ac(self, line, cell):
        """
        Cell magic to execute kidlisp code directly without quotes.
        
        Usage:
            %%ac
            (ink red)
            (line 0 0 100 100)
            
        With size options:
            %%ac 800 600
            (your kidlisp code here)
            
        First number = width, second = height
        """
        # Parse simple arguments: width height
        args = line.strip().split() if line.strip() else []
        width = "100%"
        height = 400
        
        if len(args) >= 1:
            try:
                width = int(args[0])
            except ValueError:
                width = args[0]  # Keep as string like "100%"
        
        if len(args) >= 2:
            try:
                height = int(args[1])
            except ValueError:
                height = 400
                
        return kidlisp(cell.strip(), width, height, False)
    
    @line_magic
    def ac_line(self, line):
        """
        Line magic to execute a single line of kidlisp code.
        
        Usage:
            %ac (ink red) (circle 25 25 10)
        """
        return kidlisp(line.strip(), "100%", 30, False)

# Automatic IPython/Jupyter setup - makes λ globally available
def _setup_ipython():
    """Automatically setup λ and magic commands in IPython/Jupyter environment"""
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
            
            # Register the magic commands class
            magic_instance = AestheticComputerMagics(ip)
            ip.register_magics(AestheticComputerMagics)
            
            # Also register the line magic under the same name 
            ip.register_magic_function(magic_instance.ac_line, 'line', 'ac')
            
            return True
    except ImportError:
        pass
    return False

# Auto-setup when module is imported
_setup_ipython()

# Export all functions for proper IDE support
__all__ = ['show', 'show_side_by_side', 'kidlisp', 'kidlisp_display', 'k', '_', 'l', 'λ', 'AestheticComputerMagics']