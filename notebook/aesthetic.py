"""
Aesthetic Computer Python Library for Jupyter Notebooks

Usage:
    import aesthetic

    # Toggle between production and localhost:
    aesthetic.USE_PRODUCTION = True   # Use aesthetic.computer (production)
    aesthetic.USE_PRODUCTION = False  # Use localhost:8888 (default)

Examples:
    # Use localhost (default)
    %%ac
    (ink red) (line 0 0 100 100)

    # Switch to production
    aesthetic.USE_PRODUCTION = True

    # Now all pieces load from aesthetic.computer
    %%ac
    prompt

Note: All URLs include ?notebook=true for custom boot animation
"""

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

DEFAULT_DENSITY = 2

# Global configuration for production vs localhost
# Set to True to use aesthetic.computer (production), False for localhost:8888 (default)
USE_PRODUCTION = False

def _get_base_url():
    """Get the base URL based on USE_PRODUCTION setting"""
    return "https://aesthetic.computer" if USE_PRODUCTION else "https://localhost:8888"

def _get_ipython_context():
    try:
        from IPython import get_ipython
        ip = get_ipython()
        if ip is not None:
            return ip.user_ns.copy()
    except Exception:
        pass
    return {}

def _safe_eval(expr, context=None):
    """Safely evaluate math expressions with access to IPython variables"""
    if context is None:
        context = {}

    if expr == "100%" or (isinstance(expr, str) and expr.endswith('%')):
        return expr

    try:
        return int(expr)
    except (ValueError, TypeError):
        pass

    try:
        return float(expr)
    except (ValueError, TypeError):
        pass

    import math
    safe_dict = {
        '__builtins__': {},
        'abs': abs, 'min': min, 'max': max, 'round': round,
        'int': int, 'float': float, 'sum': sum, 'len': len,
        'pow': pow,
        'math': math, 'pi': math.pi, 'e': math.e,
        'sin': math.sin, 'cos': math.cos, 'tan': math.tan,
        'sqrt': math.sqrt, 'log': math.log, 'log10': math.log10,
        'exp': math.exp, 'floor': math.floor, 'ceil': math.ceil,
        **context,
    }

    try:
        result = eval(expr, safe_dict)
        if isinstance(result, float) and result.is_integer():
            return int(result)
        return result
    except Exception as e:
        print(f"Warning: Could not evaluate '{expr}': {e}")
        return expr

def _normalize_density(value, context=None, default=DEFAULT_DENSITY):
    if value is None:
        return default
    if isinstance(value, (int, float)):
        return value
    if isinstance(value, str):
        evaluated = _safe_eval(value, context)
        if isinstance(evaluated, (int, float)):
            return evaluated
    return default

def _scale_dimension(value, density):
    if isinstance(value, (int, float)) and isinstance(density, (int, float)):
        scaled = value * density
        if isinstance(scaled, float) and scaled.is_integer():
            return int(scaled)
        return scaled
    return value

def _compute_iframe_dimensions(width, height, density):
    return _scale_dimension(width, density), _scale_dimension(height, density)

def show(piece, width="100%", height=54, density=None):
    importlib.reload(importlib.import_module('aesthetic'))
    base_url = _get_base_url()
    url = f"{base_url}/{piece}?nolabel=true&nogap=true&notebook=true"
    density_value = _normalize_density(density, _get_ipython_context())
    iframe_width, iframe_height = _compute_iframe_dimensions(width, height, density_value)
    if density_value is not None:
        url += f"&density={density_value}"
    display(IFrame(src=url, width=iframe_width, height=iframe_height, frameborder="0"))

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
        base_url = _get_base_url()
        encoded = code.replace(' ', '_').replace('\n', '~')
        return f"{base_url}/{encoded}?nolabel=true&nogap=true&notebook=true"

def kidlisp(code, width="100%", height=500, auto_scale=False, tv_mode=False, density=None):
    """
    Run kidlisp code in Aesthetic Computer.
    
    Args:
        code (str): The kidlisp code to execute
        width: Virtual width in AC pixels (default: "100%")
        height: Virtual height in AC pixels (default: 400)
        auto_scale (bool): If True, iframe will scale to fit content (default: False)
        tv_mode (bool): If True, disables touch and keyboard input for TV display (default: False)
        density (number|str): Virtual pixel density (default: 2)
    """
    importlib.reload(importlib.import_module('aesthetic'))
    
    # Use Node.js to encode kidlisp code and get the full URL
    clean_code = code.strip()
    url = encode_kidlisp_with_node(clean_code)
    
    # Add TV mode parameter if requested
    if tv_mode:
        separator = "&" if "?" in url else "?"
        url += f"{separator}tv=true"

    density_value = _normalize_density(density, _get_ipython_context())
    if density_value is not None:
        separator = "&" if "?" in url else "?"
        url += f"{separator}density={density_value}"
    
    # Create a stable ID based on content hash to reduce flicker on re-runs
    content_hash = hashlib.md5(f"{clean_code}{width}{height}{tv_mode}{density_value}".encode()).hexdigest()[:8]
    iframe_id = f"ac-iframe-{content_hash}"

    iframe_width, iframe_height = _compute_iframe_dimensions(width, height, density_value)
    
    # Always use HTML approach with no margins, positioned at top-left
    if auto_scale:
        iframe_html = f'''
        <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
            <iframe id="{iframe_id}" src="{url}" 
                    width="{iframe_width}" 
                    height="{iframe_height}" 
                    frameborder="0"
                    style="background: transparent; margin: 0; padding: 0; border: none; display: block; transform-origin: top left; max-width: 100%; height: auto; aspect-ratio: 1/1;">
            </iframe>
        </div>
        '''
    else:
        iframe_html = f'''
        <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
            <iframe id="{iframe_id}" src="{url}" 
                    width="{iframe_width}" 
                    height="{iframe_height}" 
                    frameborder="0"
                    style="background: transparent; margin: 0; padding: 0; border: none; display: block;">
            </iframe>
        </div>
        '''
    
    display(HTML(iframe_html))

def kidlisp_display(code, width="100%", height=400, auto_scale=False, density=None):
    """
    Run kidlisp code with minimal visual footprint - designed for display-only cells.
    
    Args:
        code (str): The kidlisp code to execute
        width: Width of the iframe (default: "100%")
        height: Height of the iframe (default: 400)
        auto_scale (bool): If True, iframe will scale to fit content (default: False)
        density (number|str): Virtual pixel density (default: 2)
    """
    # This function is identical to kidlisp() but with a different name
    # The "display-only" aspect comes from how you use it in your notebook
    return kidlisp(code, width, height, auto_scale, False, density)

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

    base_url = _get_base_url()
    iframes_html = []

    for item in pieces_and_options:
        if isinstance(item, tuple):
            piece, width, height = item
        else:
            piece = item
            width = 200
            height = 100

        url = f"{base_url}/{piece}?nolabel=true&nogap=true&notebook=true"
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
    density = kwargs.get('density', None)
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
    
    return kidlisp(code, width, height, auto_scale, False, density)

def _is_numeric_like(s):
    """Check if a string looks numeric without evaluation (avoids warnings)"""
    if not isinstance(s, str):
        return False
    s = s.strip()
    if not s:
        return False
    try:
        float(s)
        return True
    except (ValueError, TypeError):
        return False

# IPython Magic Commands for Native Kidlisp Syntax
@magics_class
class AestheticComputerMagics(Magics):
    """IPython magic commands for native kidlisp syntax in Jupyter notebooks"""
    
    @cell_magic
    def ac(self, line, cell):
        """
        Cell magic to execute kidlisp code or piece invocations from PRODUCTION (aesthetic.computer).
        Uppercase %%AC loads from development localhost instead.
        
        Usage (Production):
            %%ac
            (ink red)
            (line 0 0 100 100)
            
        Or for piece invocations:
            %%ac
            clock cdefg
            
        With size options:
            %%ac 400          # Height only (width = 100%)
            %%ac 800 600      # Width and height
            %%ac 320 240*2    # Math expressions supported
            (your kidlisp code here)

        Development (use %%AC uppercase for localhost:8888):
            %%AC
            prompt

        Note: Width/height are virtual AC pixels. Iframe size is scaled by density.
            
        Parameters support math expressions and variables from the current namespace.
        Examples:
            %%ac 400*2        # Height = 800, width = 100% (production)
            %%AC 400*2        # Height = 800, width = 100% (development)
            %%ac my_width my_height
            %%ac int(800/2) int(600*1.5)  # Width = 400, height = 900
            %%ac 320 240 density=2        # Virtual size with explicit density
        """
        # Parse and evaluate arguments with math support
        args = line.strip().split() if line.strip() else []
        width = "100%"
        height = 30
        tv_mode = False
        density = None
        
        # Look for tv_mode parameter
        filtered_args = []
        for arg in args:
            if arg.startswith('tv_mode='):
                tv_mode = arg.split('=')[1].lower() in ('true', '1', 'yes')
            elif arg.startswith('density=') or arg.startswith('d='):
                density = arg.split('=', 1)[1]
            else:
                filtered_args.append(arg)
        
        args = filtered_args
        
        context = _get_ipython_context()
        
        if len(args) >= 1:
            # If only one argument, treat it as height and keep width as "100%"
            if len(args) == 1:
                height_expr = args[0]
                height = _safe_eval(height_expr, context)
                # width stays as "100%" (already set above)
            else:
                # If two or more arguments, first is width, second is height
                width_expr = args[0]
                width = _safe_eval(width_expr, context)
                if len(args) >= 2:
                    height_expr = args[1]
                    height = _safe_eval(height_expr, context)

        density_value = _normalize_density(density, context)
        iframe_width, iframe_height = _compute_iframe_dimensions(width, height, density_value)
        
        # Check if the cell content is a piece invocation or kidlisp code
        cell_content = cell.strip()
        
        # Detect if this is a piece invocation vs kidlisp code
        # Piece invocations:
        # - Don't start with ( or ;
        # - Don't contain newlines (single line piece calls)
        # - First word is likely a piece name (not a kidlisp function)
        is_piece_invocation = (
            cell_content and
            not cell_content.startswith('(') and 
            not cell_content.startswith(';') and
            '\n' not in cell_content and
            not cell_content.startswith('~') and  # kidlisp can start with ~
            len(cell_content.split()) >= 1
        )
        
        # Force PRODUCTION mode for lowercase %ac
        global USE_PRODUCTION
        old_production = USE_PRODUCTION
        try:
            USE_PRODUCTION = True
            
            if is_piece_invocation:
                # Handle as piece invocation - construct URL directly
                # Replace spaces with ~ for piece parameters
                piece_url = cell_content.replace(' ', '~')
                base_url = _get_base_url()
                url = f"{base_url}/{piece_url}?nolabel=true&nogap=true&notebook=true"

                # Add TV mode parameter if requested
                if tv_mode:
                    url += "&tv=true"

                if density_value is not None:
                    url += f"&density={density_value}"
                
                # Create iframe directly
                content_hash = hashlib.md5(f"{cell_content}{width}{height}{tv_mode}{density_value}".encode()).hexdigest()[:8]
                iframe_id = f"ac-iframe-{content_hash}"
                
                iframe_html = f'''
                <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
                    <iframe id="{iframe_id}" src="{url}" 
                            width="{iframe_width}" 
                            height="{iframe_height}" 
                            frameborder="0"
                            style="background: transparent; margin: 0; padding: 0; border: none; display: block;">
                    </iframe>
                </div>
                '''
                
                display(HTML(iframe_html))
            else:
                # Handle as kidlisp code
                return kidlisp(cell_content, width, height, False, tv_mode, density_value)
        finally:
            USE_PRODUCTION = old_production
    
    @cell_magic
    def AC(self, line, cell):
        """
        Cell magic to execute kidlisp code or piece invocations from DEVELOPMENT (localhost:8888).
        Lowercase %%ac loads from production aesthetic.computer instead.
        
        Usage (Development):
            %%AC
            prompt
            
        Or for piece invocations:
            %%AC
            clock cdefg
            
        With size options:
            %%AC 400          # Height only (width = 100%)
            %%AC 800 600      # Width and height
            
        Production (use %%ac lowercase for aesthetic.computer):
            %%ac prompt

        Note: Uppercase AC = localhost/dev, lowercase ac = production domain
        """
        # Parse and evaluate arguments with math support
        args = line.strip().split() if line.strip() else []
        width = "100%"
        height = 30
        tv_mode = False
        density = None
        
        # Look for tv_mode parameter
        filtered_args = []
        for arg in args:
            if arg.startswith('tv_mode='):
                tv_mode = arg.split('=')[1].lower() in ('true', '1', 'yes')
            elif arg.startswith('density=') or arg.startswith('d='):
                density = arg.split('=', 1)[1]
            else:
                filtered_args.append(arg)
        
        args = filtered_args
        
        context = _get_ipython_context()
        
        if len(args) >= 1:
            # If only one argument, treat it as height and keep width as "100%"
            if len(args) == 1:
                height_expr = args[0]
                height = _safe_eval(height_expr, context)
                # width stays as "100%" (already set above)
            else:
                # If two or more arguments, first is width, second is height
                width_expr = args[0]
                width = _safe_eval(width_expr, context)
                if len(args) >= 2:
                    height_expr = args[1]
                    height = _safe_eval(height_expr, context)

        density_value = _normalize_density(density, context)
        iframe_width, iframe_height = _compute_iframe_dimensions(width, height, density_value)
        
        # Check if the cell content is a piece invocation or kidlisp code
        cell_content = cell.strip()
        
        # Detect if this is a piece invocation vs kidlisp code
        is_piece_invocation = (
            cell_content and
            not cell_content.startswith('(') and 
            not cell_content.startswith(';') and
            '\n' not in cell_content and
            not cell_content.startswith('~') and
            len(cell_content.split()) >= 1
        )
        
        # Force DEVELOPMENT mode (localhost) for uppercase AC
        global USE_PRODUCTION
        old_production = USE_PRODUCTION
        try:
            USE_PRODUCTION = False
            
            if is_piece_invocation:
                # Handle as piece invocation - construct URL directly
                piece_url = cell_content.replace(' ', '~')
                base_url = _get_base_url()  # This will now use localhost
                url = f"{base_url}/{piece_url}?nolabel=true&nogap=true&notebook=true"

                # Add TV mode parameter if requested
                if tv_mode:
                    url += "&tv=true"

                if density_value is not None:
                    url += f"&density={density_value}"
                
                # Create iframe directly
                content_hash = hashlib.md5(f"{cell_content}{width}{height}{tv_mode}{density_value}".encode()).hexdigest()[:8]
                iframe_id = f"ac-iframe-{content_hash}"
                
                iframe_html = f'''
                <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
                    <iframe id="{iframe_id}" src="{url}" 
                            width="{iframe_width}" 
                            height="{iframe_height}" 
                            frameborder="0"
                            style="background: transparent; margin: 0; padding: 0; border: none; display: block;">
                    </iframe>
                </div>
                '''
                
                display(HTML(iframe_html))
            else:
                # Handle as kidlisp code
                return kidlisp(cell_content, width, height, False, tv_mode, density_value)
        finally:
            USE_PRODUCTION = old_production
    
    @line_magic
    def ac_line(self, line):
        """
        Line magic to execute a single line of kidlisp code or piece invocation.
        
        Usage:
            %ac (ink red) (circle 25 25 10)
            %ac clock cdefg
            %ac 100 clock cdefg          # Height = 100, width = 100%
            %ac 400 200 clock cdefg      # Width = 400, height = 200
            %ac 320 240 density=2 clock  # Virtual size with explicit density

        Note: Width/height are virtual AC pixels. Iframe size is scaled by density.
            
        Note: For piece invocations with special characters like {}, use cell magic %%ac instead
        to avoid Python syntax parsing issues.
        """
        line_content = line.strip()
        
        # Parse potential size parameters from the beginning of the line
        parts = line_content.split()
        width = "100%"
        height = 30
        density = None
        content_start_index = 0

        context = _get_ipython_context()

        filtered_parts = []
        for part in parts:
            if part.startswith('density=') or part.startswith('d='):
                density = part.split('=', 1)[1]
            else:
                filtered_parts.append(part)

        parts = filtered_parts
        
        # Check if first part(s) are numeric parameters
        if len(parts) >= 2:  # Need at least 2 parts to have a size parameter + content
            try:
                # Try to parse first part as a number (height only)
                first_num = _safe_eval(parts[0], context)
                if not isinstance(first_num, (int, float)):
                    raise ValueError
                if len(parts) >= 3:
                    try:
                        # Try to parse second part as a number (width, height)
                        second_num = _safe_eval(parts[1], context)
                        if not isinstance(second_num, (int, float)):
                            raise ValueError
                        width = first_num
                        height = second_num
                        content_start_index = 2
                    except ValueError:
                        # Second part is not a number, so first is height only
                        height = first_num
                        content_start_index = 1
                else:
                    # Only two parts total, first is height, second is content
                    height = first_num
                    content_start_index = 1
            except ValueError:
                # First part is not a number, treat entire line as content
                pass
        
        # Extract the actual content (after size parameters)
        if content_start_index > 0:
            actual_content = ' '.join(parts[content_start_index:])
        else:
            actual_content = line_content
        
        # Detect if this is a piece invocation vs kidlisp code
        is_piece_invocation = (
            actual_content and
            not actual_content.startswith('(') and 
            not actual_content.startswith(';') and
            not actual_content.startswith('~') and
            len(actual_content.split()) >= 1
        )
        
        density_value = _normalize_density(density, context)
        iframe_width, iframe_height = _compute_iframe_dimensions(width, height, density_value)

        if is_piece_invocation:
            # Handle as piece invocation - construct URL directly
            # Replace spaces with ~ for piece parameters
            piece_url = actual_content.replace(' ', '~')
            base_url = _get_base_url()
            url = f"{base_url}/{piece_url}?nolabel=true&nogap=true&notebook=true"

            if density_value is not None:
                url += f"&density={density_value}"
            
            # Create iframe directly
            content_hash = hashlib.md5(f"{actual_content}{width}{height}{density_value}".encode()).hexdigest()[:8]
            iframe_id = f"ac-iframe-{content_hash}"
            
            iframe_html = f'''
            <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
                <iframe id="{iframe_id}" src="{url}" 
                        width="{iframe_width}" 
                        height="{iframe_height}" 
                        frameborder="0"
                        style="background: transparent; margin: 0; padding: 0; border: none; display: block;">
                </iframe>
            </div>
            '''
            
            display(HTML(iframe_html))
        else:
            # Handle as kidlisp code
            return kidlisp(actual_content, width, height, False, False, density_value)
    
    @line_magic
    def AC(self, line):
        """
        Line magic to execute a single line of kidlisp code or piece invocation from DEVELOPMENT.
        
        Usage:
            %AC (ink red) (circle 25 25 10)
            %AC clock cdefg
            %AC 100 clock cdefg          # Height = 100, width = 100%
            %AC 400 200 clock cdefg      # Width = 400, height = 200

        Note: Uppercase AC = localhost/dev, lowercase ac = production domain
        Note: Width/height are virtual AC pixels. Iframe size is scaled by density.
        """
        line_content = line.strip()
        
        # Parse potential size parameters from the beginning of the line
        parts = line_content.split()
        width = "100%"
        height = 30
        density = None
        content_start_index = 0

        context = _get_ipython_context()

        filtered_parts = []
        for part in parts:
            if part.startswith('density=') or part.startswith('d='):
                density = part.split('=', 1)[1]
            else:
                filtered_parts.append(part)

        parts = filtered_parts
        
        # Check if first part(s) are numeric parameters
        if len(parts) >= 2:  # Need at least 2 parts to have a size parameter + content
            try:
                # Try to parse first part as a number (height only)
                first_num = _safe_eval(parts[0], context)
                if not isinstance(first_num, (int, float)):
                    raise ValueError
                if len(parts) >= 3:
                    try:
                        # Try to parse second part as a number (width, height)
                        second_num = _safe_eval(parts[1], context)
                        if not isinstance(second_num, (int, float)):
                            raise ValueError
                        width = first_num
                        height = second_num
                        content_start_index = 2
                    except ValueError:
                        # Second part is not a number, so first is height only
                        height = first_num
                        content_start_index = 1
                else:
                    # Only two parts total, first is height, second is content
                    height = first_num
                    content_start_index = 1
            except ValueError:
                # First part is not a number, treat entire line as content
                pass
        
        # Extract the actual content (after size parameters)
        if content_start_index > 0:
            actual_content = ' '.join(parts[content_start_index:])
        else:
            actual_content = line_content
        
        # Detect if this is a piece invocation vs kidlisp code
        is_piece_invocation = (
            actual_content and
            not actual_content.startswith('(') and 
            not actual_content.startswith(';') and
            not actual_content.startswith('~') and
            len(actual_content.split()) >= 1
        )
        
        density_value = _normalize_density(density, context)
        iframe_width, iframe_height = _compute_iframe_dimensions(width, height, density_value)

        # Force DEVELOPMENT mode (localhost) for uppercase AC
        global USE_PRODUCTION
        old_production = USE_PRODUCTION
        try:
            USE_PRODUCTION = False
            
            if is_piece_invocation:
                # Handle as piece invocation - construct URL directly
                # Replace spaces with ~ for piece parameters
                piece_url = actual_content.replace(' ', '~')
                base_url = _get_base_url()  # This will now use localhost
                url = f"{base_url}/{piece_url}?nolabel=true&nogap=true&notebook=true"

                if density_value is not None:
                    url += f"&density={density_value}"
                
                # Create iframe directly
                content_hash = hashlib.md5(f"{actual_content}{width}{height}{density_value}".encode()).hexdigest()[:8]
                iframe_id = f"ac-iframe-{content_hash}"
                
                iframe_html = f'''
                <div style="margin: -8px -8px 0 -8px; padding: 0; overflow: hidden;">
                    <iframe id="{iframe_id}" src="{url}" 
                            width="{iframe_width}" 
                            height="{iframe_height}" 
                            frameborder="0"
                            style="background: transparent; margin: 0; padding: 0; border: none; display: block;">
                    </iframe>
                </div>
                '''
                
                display(HTML(iframe_html))
            else:
                # Handle as kidlisp code
                return kidlisp(actual_content, width, height, False, False, density_value)
        finally:
            USE_PRODUCTION = old_production

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
            
            # Create and register line magics with production/dev modes forced
            def ac_line_prod(line):
                """Line magic for production (%ac)"""
                global USE_PRODUCTION
                old_production = USE_PRODUCTION
                try:
                    USE_PRODUCTION = True
                    return magic_instance.ac_line(line)
                finally:
                    USE_PRODUCTION = old_production
            
            def AC_line_dev(line):
                """Line magic for development (%AC)"""
                global USE_PRODUCTION
                old_production = USE_PRODUCTION
                try:
                    USE_PRODUCTION = False
                    return magic_instance.ac_line(line)
                finally:
                    USE_PRODUCTION = old_production
            
            # Register the line magics with correct names
            ip.register_magic_function(ac_line_prod, 'line', 'ac')
            ip.register_magic_function(AC_line_dev, 'line', 'AC')
            
            return True
    except ImportError:
        pass
    return False

# Auto-setup when module is imported
_setup_ipython()

# Export all functions and configuration for proper IDE support
__all__ = ['show', 'show_side_by_side', 'kidlisp', 'kidlisp_display', 'k', '_', 'l', 'λ', 'AestheticComputerMagics', 'USE_PRODUCTION']