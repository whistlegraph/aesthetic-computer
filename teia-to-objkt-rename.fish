#!/usr/bin/env fish

# TEIA → OBJKT bulk rename script
# This script performs all the necessary renames for the TEIA → OBJKT refactor

set ROOT /workspaces/aesthetic-computer

echo "🔄 Starting TEIA → OBJKT rename..."

# Target specific directories to avoid node_modules and output
set SEARCH_PATHS \
  "$ROOT/system/public/aesthetic.computer" \
  "$ROOT/objkt" \
  "$ROOT/plans" \
  "$ROOT/reference" \
  "$ROOT/tokens" \
  "$ROOT/.gitignore" \
  "$ROOT/TODO.txt"

# Function to replace in files
function replace_in_files
  set pattern $argv[1]
  set replacement $argv[2]
  set description $argv[3]
  
  echo "  → $description"
  
  for path in $SEARCH_PATHS
    if test -f $path
      # Single file
      sed -i "s/$pattern/$replacement/g" $path
    else if test -d $path
      # Directory - find all relevant files
      find $path -type f \( -name "*.mjs" -o -name "*.js" -o -name "*.md" -o -name "*.txt" -o -name "*.fish" \) \
        -not -path "*/node_modules/*" \
        -not -path "*/.git/*" \
        -not -path "*/output/*" \
        -exec sed -i "s/$pattern/$replacement/g" {} \;
    end
  end
end

# Variable renames (order matters - most specific first)
replace_in_files "acTEIA_MATRIX_CHUNKY_GLYPHS" "acOBJKT_MATRIX_CHUNKY_GLYPHS" "acTEIA_MATRIX_CHUNKY_GLYPHS → acOBJKT_MATRIX_CHUNKY_GLYPHS"
replace_in_files "acTEIA_COLOPHON" "acOBJKT_COLOPHON" "acTEIA_COLOPHON → acOBJKT_COLOPHON"
replace_in_files "acTEIA_CREATOR" "acOBJKT_CREATOR" "acTEIA_CREATOR → acOBJKT_CREATOR"
replace_in_files "acTEIA_VIEWER" "acOBJKT_VIEWER" "acTEIA_VIEWER → acOBJKT_VIEWER"
replace_in_files "acTEIA_MODE" "acOBJKT_MODE" "acTEIA_MODE → acOBJKT_MODE"

replace_in_files "teiaKidlispCodes" "objktKidlispCodes" "teiaKidlispCodes → objktKidlispCodes"
replace_in_files "teiaContext" "objktContext" "teiaContext → objktContext"
replace_in_files "isTeiaMode" "isObjktMode" "isTeiaMode → isObjktMode"
replace_in_files "teiaMode" "objktMode" "teiaMode → objktMode"

# Function renames
replace_in_files "getTeiaMode" "getObjktMode" "getTeiaMode() → getObjktMode()"
replace_in_files "setTeiaMode" "setObjktMode" "setTeiaMode() → setObjktMode()"
replace_in_files "checkTeiaMode" "checkObjktMode" "checkTeiaMode() → checkObjktMode()"
replace_in_files "patchForTeia" "patchForObjkt" "patchForTeia() → patchForObjkt()"
replace_in_files "ForTeia" "ForObjkt" "ForTeia → ForObjkt (in function names)"

# Module/file references
replace_in_files "teia-mode\\.mjs" "objkt-mode.mjs" "teia-mode.mjs → objkt-mode.mjs"
replace_in_files "lib/teia-mode" "lib/objkt-mode" "lib/teia-mode → lib/objkt-mode"

# Path references - be careful here!
replace_in_files "\\.\\./\\.\\./\\.\\./teia/" "../../../objkt/" "../../../teia/ → ../../../objkt/"
replace_in_files "'/teia/" "'/objkt/" "'/teia/ → '/objkt/"
replace_in_files '"/teia/' '"/objkt/' '"/teia/ → "/objkt/'
replace_in_files " teia/" " objkt/" " teia/ → objkt/"
replace_in_files "^teia/" "objkt/" "^teia/ → objkt/ (line start)"

# Comment and string replacements (preserve "teia.art" and "for Teia" / "to Teia")
# Use negative lookahead-style patterns where possible
replace_in_files "TEIA mode" "OBJKT mode" "TEIA mode → OBJKT mode"
replace_in_files "Teia mode" "OBJKT mode" "Teia mode → OBJKT mode" 
replace_in_files "teia mode" "objkt mode" "teia mode → objkt mode"

# Standalone TEIA/Teia (but not in teia.art)
replace_in_files " TEIA " " OBJKT " " TEIA → OBJKT (standalone)"
replace_in_files "TEIA:" "OBJKT:" "TEIA: → OBJKT:"
replace_in_files "TEIA package" "OBJKT package" "TEIA package → OBJKT package"
replace_in_files "Teia package" "OBJKT package" "Teia package → OBJKT package"
replace_in_files "teia package" "objkt package" "teia package → objkt package"
replace_in_files "Teia deployment" "OBJKT deployment" "Teia deployment → OBJKT deployment"
replace_in_files "Teia OBJKT" "OBJKT" "Teia OBJKT → OBJKT (redundant)"

# .gitignore specific
replace_in_files "pack-for-teia\\.fish" "pack-for-objkt.fish" "pack-for-teia.fish → pack-for-objkt.fish"
replace_in_files "teia output" "objkt output" "teia output → objkt output"
replace_in_files "teia/output" "objkt/output" "teia/output → objkt/output"

echo "✅ TEIA → OBJKT rename complete!"
echo ""
echo "Note: 'teia.art' platform URLs were preserved"
