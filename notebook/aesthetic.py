import sys
import importlib
import urllib.parse
import hashlib
import subprocess
import os

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

def encode_kidlisp_with_node(code):
    """
    Use Node.js to encode kidlisp code using the same function as kidlisp.mjs
    Falls back to Python implementation if Node.js is not available
    """
    try:
        # Get the directory of this script
        script_dir = os.path.dirname(os.path.abspath(__file__))
        encoder_script = os.path.join(script_dir, 'encode_kidlisp.mjs')
        
        # Run the Node.js encoder
        result = subprocess.run(
            ['node', encoder_script, code],
            capture_output=True,
            text=True,
            timeout=5
        )
        
        if result.returncode == 0:
            return result.stdout.strip()
        else:
            print(f"Node.js encoder failed: {result.stderr}")
            raise subprocess.CalledProcessError(result.returncode, 'node')
            
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError) as e:
        print(f"Falling back to Python encoder: {e}")
        # Fallback to Python implementation with full URL
        encoded = code.replace(' ', '_').replace('\n', '~')
        return f"https://localhost:8888/{encoded}?nolabel=true&nogap=true"

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
    
    # Use Node.js to encode kidlisp code and get the full URL
    clean_code = code.strip()
    url = encode_kidlisp_with_node(clean_code)
    
    # Create a stable ID based on content hash to reduce flicker on re-runs
    content_hash = hashlib.md5(f"{clean_code}{width}{height}".encode()).hexdigest()[:8]
    iframe_id = f"ac-iframe-{content_hash}"
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
            %%ac 320 240*2
            %%ac width/2 height*1.5
            %%ac canvas_width canvas_height
            (your kidlisp code here)
            
        Parameters support math expressions and variables from the current namespace.
        Examples:
            %%ac 400*2 300+50
            %%ac my_width my_height
            %%ac int(800/2) int(600*1.5)
        """
        # Parse and evaluate arguments with math support
        args = line.strip().split() if line.strip() else []
        width = "100%"
        height = 30
        
        def safe_eval(expr, context=None):
            """Safely evaluate math expressions with access to IPython variables"""
            if context is None:
                context = {}
            
            # Handle special cases first
            if expr == "100%" or expr.endswith('%'):
                return expr
            
            # Try simple int conversion first
            try:
                return int(expr)
            except ValueError:
                pass
            
            # Try float conversion
            try:
                return float(expr)
            except ValueError:
                pass
            
            # Set up safe evaluation context with math functions and user variables
            import math
            safe_dict = {
                '__builtins__': {},
                # Basic functions
                'abs': abs, 'min': min, 'max': max, 'round': round,
                'int': int, 'float': float, 'sum': sum, 'len': len,
                'pow': pow,
                # Math module
                'math': math, 'pi': math.pi, 'e': math.e,
                'sin': math.sin, 'cos': math.cos, 'tan': math.tan,
                'sqrt': math.sqrt, 'log': math.log, 'log10': math.log10,
                'exp': math.exp, 'floor': math.floor, 'ceil': math.ceil,
                # Add all variables from the IPython user namespace
                **context
            }
            
            try:
                result = eval(expr, safe_dict)
                # Convert to int if it's a clean number
                if isinstance(result, float) and result.is_integer():
                    return int(result)
                return result
            except Exception as e:
                # If evaluation fails, return the original expression
                print(f"Warning: Could not evaluate '{expr}': {e}")
                return expr
        
        # Get IPython user namespace for variable access
        context = {}
        try:
            from IPython import get_ipython
            ip = get_ipython()
            if ip is not None:
                # Include user variables in the evaluation context
                context = ip.user_ns.copy()
        except:
            pass
        
        if len(args) >= 1:
            width_expr = args[0]
            width = safe_eval(width_expr, context)
        
        if len(args) >= 2:
            height_expr = args[1]
            height = safe_eval(height_expr, context)
                
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