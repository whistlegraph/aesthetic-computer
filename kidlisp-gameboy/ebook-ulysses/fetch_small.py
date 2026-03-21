#!/usr/bin/env python3
"""
Fetch Ulysses from Project Gutenberg and create a compressed Game Boy ROM data file.
Uses a simple compression: store unique words + indices, or just pack tightly.
"""

import requests
import re

def fetch_ulysses():
    """Fetch the text of Ulysses from Project Gutenberg."""
    url = "https://www.gutenberg.org/cache/epub/4300/pg4300.txt"
    print(f"Fetching Ulysses from {url}...")
    response = requests.get(url)
    response.raise_for_status()
    return response.text

def clean_text(text):
    """Clean and prepare the text."""
    # Find the start and end of actual content
    start_marker = "*** START OF THE PROJECT GUTENBERG EBOOK"
    end_marker = "*** END OF THE PROJECT GUTENBERG EBOOK"
    
    start_idx = text.find(start_marker)
    if start_idx != -1:
        start_idx = text.find("\n", start_idx) + 1
    else:
        start_idx = 0
    
    end_idx = text.find(end_marker)
    if end_idx == -1:
        end_idx = len(text)
    
    content = text[start_idx:end_idx]
    
    # Replace fancy quotes and dashes with plain ASCII
    content = content.replace('\u2018', "'")  # left single quote
    content = content.replace('\u2019', "'")  # right single quote
    content = content.replace('\u201c', '"')  # left double quote
    content = content.replace('\u201d', '"')  # right double quote
    content = content.replace('\u2014', '-')  # em dash
    content = content.replace('\u2013', '-')  # en dash
    content = content.replace('\u2026', '...')  # ellipsis
    
    # Convert to ASCII only, drop non-ASCII chars
    content = content.encode('ascii', 'ignore').decode('ascii')
    
    # Normalize whitespace
    content = re.sub(r'\r\n', '\n', content)
    content = re.sub(r'\n{3,}', '\n\n', content)
    content = re.sub(r'[ \t]+', ' ', content)
    
    return content.strip()

def paginate_text(text, chars_per_line=20, lines_per_page=17):
    """Break text into pages that fit the Game Boy screen."""
    pages = []
    words = text.split()
    
    current_page = []
    current_line = ""
    line_count = 0
    
    for word in words:
        # Check if word fits on current line
        test_line = current_line + (" " if current_line else "") + word
        
        if len(test_line) <= chars_per_line:
            current_line = test_line
        else:
            # Word doesn't fit, start new line
            if current_line:
                current_page.append(current_line)
                line_count += 1
            
            # Check if we need a new page
            if line_count >= lines_per_page:
                pages.append("\n".join(current_page))
                current_page = []
                line_count = 0
            
            # Handle words longer than line width
            if len(word) > chars_per_line:
                while len(word) > chars_per_line:
                    current_page.append(word[:chars_per_line])
                    word = word[chars_per_line:]
                    line_count += 1
                    if line_count >= lines_per_page:
                        pages.append("\n".join(current_page))
                        current_page = []
                        line_count = 0
                current_line = word
            else:
                current_line = word
    
    # Don't forget the last line and page
    if current_line:
        current_page.append(current_line)
    if current_page:
        pages.append("\n".join(current_page))
    
    return pages

def generate_c_header(pages, max_pages=50):
    """Generate a C header file with the book data."""
    # Limit pages to fit in ROM
    pages = pages[:max_pages]
    
    header = '''// Auto-generated Ulysses book data
// James Joyce - Project Gutenberg

#ifndef BOOK_DATA_H
#define BOOK_DATA_H

#define TOTAL_PAGES {}

'''.format(len(pages))
    
    # Generate page strings
    header += "const char* const book_pages[] = {\n"
    
    for i, page in enumerate(pages):
        # Escape special characters for C string
        escaped = page.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')
        header += f'    "{escaped}"'
        if i < len(pages) - 1:
            header += ","
        header += f"  // Page {i+1}\n"
    
    header += "};\n\n#endif // BOOK_DATA_H\n"
    
    return header

def main():
    # Fetch and process text
    raw_text = fetch_ulysses()
    clean = clean_text(raw_text)
    
    print(f"Cleaned text: {len(clean)} characters")
    
    # Paginate - 20 chars wide, 17 lines (leaving room for status)
    pages = paginate_text(clean, chars_per_line=20, lines_per_page=17)
    
    print(f"Total pages: {len(pages)}")
    
    # Generate header with limited pages to fit in 32KB ROM
    # Each page is roughly 350 bytes, so 50 pages ~ 17KB
    header = generate_c_header(pages, max_pages=50)
    
    with open("book_data_small.h", "w") as f:
        f.write(header)
    
    print(f"Generated book_data_small.h with 50 pages")
    print(f"First page preview:\n{pages[0][:200]}...")

if __name__ == "__main__":
    main()
