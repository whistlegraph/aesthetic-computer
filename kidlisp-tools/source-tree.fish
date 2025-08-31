#!/usr/bin/env fish
# KidLisp Source Tree Analyzer
# Usage: ./source-tree.fish $cow
# Shows the embedded layer structure and source code of KidLisp pieces

function print_usage
    echo "Usage: source-tree.fish <piece-name>"
    echo "Example: source-tree.fish \$cow"
    echo "         source-tree.fish cow"
    echo ""
    echo "Analyzes KidLisp pieces and shows their embedded layer tree structure."
end

function fetch_source
    set piece_name $argv[1]
    # Remove $ prefix if present
    set piece_name (string replace --regex '^\$' '' $piece_name)
    
    # Fetch from local store-kidlisp API
    set response (curl -s -k "https://localhost:8888/.netlify/functions/store-kidlisp?code=$piece_name" 2>/dev/null)
    
    if test $status -ne 0
        echo "❌ Error: Could not connect to local API (https://localhost:8888)"
        echo "   Make sure the dev server is running with: npm run dev"
        return 1
    end
    
    # Check if we got an error response
    if string match -q '*"error"*' $response
        echo "❌ Error: Piece '\$$piece_name' not found"
        return 1
    end
    
    # Extract source code using string manipulation (since we don't have jq)
    set source (echo $response | string match -r '"source":"([^"]*)"' | string replace '"source":"' '' | string replace '"' '')
    
    if test -z "$source"
        echo "❌ Error: Could not parse source code from response"
        return 1
    end
    
    echo $source
end

function extract_embedded_pieces
    set source $argv[1]
    # Find all $piece references like ($39i ...) or ($r2f ...)
    set raw_matches (echo $source | string match -ra '\(\$[a-zA-Z0-9_-]+')
    set pieces
    for match in $raw_matches
        set piece (echo $match | string replace '($' '')
        if test -n "$piece"
            set pieces $pieces $piece
        end
    end
    # Remove duplicates and return as separate arguments
    for piece in $pieces
        echo $piece
    end | sort -u
end

function print_tree_node
    set piece_name $argv[1]
    set depth $argv[2]
    set prefix $argv[3]
    
    # Create indentation
    set indent ""
    for i in (seq 1 $depth)
        set indent "$indent  "
    end
    
    # Fetch source code
    set source (fetch_source $piece_name)
    if test $status -ne 0
        echo "$indent$prefix❌ \$$piece_name (not found)"
        return
    end
    
    # Clean up source for display (replace \n with actual newlines and unescape)
    set clean_source (echo $source | string replace -a '\\n' '\n' | string unescape)
    
    # Extract embedded pieces
    set embedded_pieces (extract_embedded_pieces $source)
    
    # Print current piece
    if test (count $embedded_pieces) -gt 0
        echo "$indent$prefix📁 \$$piece_name"
    else
        echo "$indent$prefix📄 \$$piece_name"
    end
    
    # Print source code (first few lines)
    set source_lines (echo $clean_source | head -n 3)
    for line in $source_lines
        if test -n "$line"
            set truncated_line (echo $line | string sub -l 60)
            if test (string length $line) -gt 60
                set truncated_line "$truncated_line..."
            end
            echo "$indent   │ $truncated_line"
        end
    end
    
    # If source is longer than 3 lines, show indicator
    set total_lines (echo $clean_source | wc -l)
    if test $total_lines -gt 3
        echo "$indent   │ ... ($total_lines lines total)"
    end
    
    # Process embedded pieces recursively (with depth limit)
    if test $depth -lt 5  # Prevent infinite recursion
        set piece_count (count $embedded_pieces)
        for i in (seq 1 $piece_count)
            set embedded_piece $embedded_pieces[$i]
            if test $i -eq $piece_count
                print_tree_node $embedded_piece (math $depth + 1) "└─ "
            else
                print_tree_node $embedded_piece (math $depth + 1) "├─ "
            end
        end
    else
        if test (count $embedded_pieces) -gt 0
            echo "$indent   └─ ... (max depth reached)"
        end
    end
end

function analyze_performance_features
    set source $argv[1]
    echo ""
    echo "🔍 Performance Analysis:"
    
    # Check for expensive operations
    set expensive_ops
    if string match -q '*blur*' $source
        set expensive_ops $expensive_ops "blur"
    end
    if string match -q '*zoom*' $source
        set expensive_ops $expensive_ops "zoom"
    end
    if string match -q '*contrast*' $source
        set expensive_ops $expensive_ops "contrast"
    end
    if string match -q '*spin*' $source
        set expensive_ops $expensive_ops "spin"
    end
    if string match -q '*flood*' $source
        set expensive_ops $expensive_ops "flood"
    end
    
    if test (count $expensive_ops) -gt 0
        echo "   ⚠️  Expensive operations detected: "(string join ", " $expensive_ops)
    else
        echo "   ✅ No expensive operations detected"
    end
    
    # Check for timing expressions
    if string match -q '*s(*' $source
        echo "   ⏱️  Contains timing expressions (animated)"
    end
    
    # Check for randomness
    if string match -q '*?*' $source
        echo "   🎲 Contains randomness (?)"
    end
    
    # Count embedded layers
    set embedded_count (extract_embedded_pieces $source | count)
    if test $embedded_count -gt 0
        echo "   📁 Embeds $embedded_count layer(s)"
    end
end

# Main script
if test (count $argv) -eq 0
    print_usage
    exit 1
end

set piece_name $argv[1]

# Remove $ prefix if present for display
set display_name $piece_name
if not string match -q '\$*' $display_name
    set display_name "\$$display_name"
end

echo "🌳 KidLisp Source Tree for $display_name"
echo "═══════════════════════════════════════════"

# Build the tree
print_tree_node $piece_name 0 ""

# Get the main source for analysis
set main_source (fetch_source $piece_name)
if test $status -eq 0
    analyze_performance_features $main_source
end

echo ""
echo "💡 Use this analysis to understand performance bottlenecks!"
echo "   Run with different pieces: source-tree.fish \$39i"
