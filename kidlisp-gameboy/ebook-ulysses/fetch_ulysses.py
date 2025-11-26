#!/usr/bin/env python3
"""
Fetch James Joyce's Ulysses from Project Gutenberg and convert to C header
for Game Boy Color ebook ROM.
"""

import requests
import re
import textwrap

# Project Gutenberg URL for Ulysses
ULYSSES_URL = "https://www.gutenberg.org/cache/epub/4300/pg4300.txt"

def fetch_text():
    """Download Ulysses from Project Gutenberg"""
    print("📚 Downloading Ulysses from Project Gutenberg...")
    response = requests.get(ULYSSES_URL)
    response.raise_for_status()
    return response.text

def clean_text(raw_text):
    """Clean and prepare text for Game Boy display"""
    # Remove Project Gutenberg header/footer
    start_marker = "*** START OF THE PROJECT GUTENBERG EBOOK"
    end_marker = "*** END OF THE PROJECT GUTENBERG EBOOK"
    
    start_idx = raw_text.find(start_marker)
    if start_idx != -1:
        start_idx = raw_text.find("\n", start_idx) + 1
    else:
        start_idx = 0
    
    end_idx = raw_text.find(end_marker)
    if end_idx == -1:
        end_idx = len(raw_text)
    
    text = raw_text[start_idx:end_idx].strip()
    
    # Replace special characters with ASCII equivalents
    replacements = {
        '"': '"', '"': '"', ''': "'", ''': "'",
        '—': '--', '–': '-', '…': '...',
        'æ': 'ae', 'Æ': 'AE', 'œ': 'oe', 'Œ': 'OE',
        '£': 'L', '€': 'E',
        '\r\n': '\n', '\r': '\n',
    }
    for old, new in replacements.items():
        text = text.replace(old, new)
    
    # Keep only printable ASCII
    text = ''.join(c if 32 <= ord(c) < 127 or c == '\n' else ' ' for c in text)
    
    # Normalize whitespace
    text = re.sub(r'[ \t]+', ' ', text)
    text = re.sub(r'\n{3,}', '\n\n', text)
    
    return text.strip()

def paginate_text(text, chars_per_line=18, lines_per_page=16):
    """Split text into Game Boy sized pages"""
    pages = []
    current_page = []
    current_line_count = 0
    
    # Split into paragraphs
    paragraphs = text.split('\n\n')
    
    for para in paragraphs:
        # Wrap paragraph to fit screen width
        wrapped_lines = textwrap.wrap(para.strip(), width=chars_per_line)
        if not wrapped_lines:
            wrapped_lines = ['']  # Empty paragraph = blank line
        
        for line in wrapped_lines:
            if current_line_count >= lines_per_page:
                # Page is full
                pages.append('\n'.join(current_page))
                current_page = []
                current_line_count = 0
            
            current_page.append(line)
            current_line_count += 1
        
        # Add blank line between paragraphs if room
        if current_line_count < lines_per_page - 1:
            current_page.append('')
            current_line_count += 1
    
    # Don't forget last page
    if current_page:
        pages.append('\n'.join(current_page))
    
    return pages

def escape_for_c(text):
    """Escape text for C string literal"""
    result = []
    for c in text:
        if c == '\\':
            result.append('\\\\')
        elif c == '"':
            result.append('\\"')
        elif c == '\n':
            result.append('\\n')
        elif 32 <= ord(c) < 127:
            result.append(c)
        else:
            result.append(' ')
    return ''.join(result)

def generate_header(pages, output_file="book_data.h"):
    """Generate C header file with book data"""
    print(f"📖 Generating {output_file} with {len(pages)} pages...")
    
    # Game Boy ROM size with MBC5 can go up to 8MB
    # With 64KB ROM (4 banks), ~40KB usable for data
    # Average ~200 bytes/page = ~200 pages safely
    # Let's try 300 pages for a good chunk of the book
    max_pages = min(len(pages), 300)  # Fit in 64KB ROM
    
    with open(output_file, 'w') as f:
        f.write("// Auto-generated book data for Game Boy Color\n")
        f.write("// James Joyce - Ulysses (from Project Gutenberg)\n")
        f.write(f"// Total pages: {max_pages}\n\n")
        f.write("#ifndef BOOK_DATA_H\n")
        f.write("#define BOOK_DATA_H\n\n")
        f.write(f"#define TOTAL_PAGES {max_pages}\n")
        f.write("#define CHARS_PER_LINE 18\n")
        f.write("#define LINES_PER_PAGE 16\n\n")
        
        # For GBDK, we need to use banked data for large content
        # Write pages as array of string pointers
        f.write("// Page data\n")
        f.write("const char* const book_pages[] = {\n")
        
        for i, page in enumerate(pages[:max_pages]):
            escaped = escape_for_c(page)
            f.write(f'    "{escaped}"')
            if i < max_pages - 1:
                f.write(',')
            f.write(f'  // Page {i+1}\n')
        
        f.write("};\n\n")
        f.write("#endif // BOOK_DATA_H\n")
    
    print(f"✅ Generated {output_file}")
    return max_pages

def main():
    # Fetch and process
    raw_text = fetch_text()
    clean = clean_text(raw_text)
    
    print(f"📝 Cleaned text: {len(clean)} characters")
    
    # Paginate for Game Boy screen (20x18 visible, using 18x16 for margins)
    pages = paginate_text(clean)
    print(f"📄 Total pages: {len(pages)}")
    
    # Generate header
    num_pages = generate_header(pages)
    
    print(f"\n🎮 Ready to build! Text has been split into {num_pages} pages.")
    print("Run: ../build.fish ulysses.c ulysses")

if __name__ == "__main__":
    main()
