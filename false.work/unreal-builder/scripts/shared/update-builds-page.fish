#!/usr/bin/env fish
# Shared function to update builds.false.work page
# Usage: update_builds_page PLATFORM VERSION TIMESTAMP FILE_SIZE START_LEVEL UE_VERSION DOWNLOAD_URL

function update_builds_page
    set platform $argv[1]      # "windows" or "mac"
    set version $argv[2]        # e.g., "2025.11.21-2133"
    set timestamp $argv[3]      # ISO timestamp
    set file_size $argv[4]      # Size in MB
    set start_level $argv[5]    # e.g., "L_VerticalSlice_Demo"
    set ue_version $argv[6]     # e.g., "UE_5.6"
    set download_url $argv[7]   # Full download URL

    # Set platform display
    if test "$platform" = "windows"
        set platform_tag "🪟 Windows"
        set platform_class "platform-windows"
    else if test "$platform" = "mac"
        set platform_tag "🍎 Mac"
        set platform_class "platform-mac"
    else
        echo "Error: Unknown platform $platform"
        return 1
    end

    # Extract date from version (YYYY.MM.DD format)
    set build_date (echo $version | cut -d'-' -f1)
    set today (date +%Y.%m.%d)
    set yesterday (date -d yesterday +%Y.%m.%d 2>/dev/null; or date -v-1d +%Y.%m.%d)

    # Determine date label
    if test "$build_date" = "$today"
        set date_label "Today"
    else if test "$build_date" = "$yesterday"
        set date_label "Yesterday"
    else
        # Format as readable date
        set date_label (date -d (echo $build_date | tr '.' '-') '+%B %d, %Y' 2>/dev/null; or date -j -f '%Y.%m.%d' $build_date '+%B %d, %Y')
    end

    # Use awk to insert/update build in grouped format
    awk -v platform_tag="$platform_tag" \
        -v platform_class="$platform_class" \
        -v version="$version" \
        -v timestamp="$timestamp" \
        -v size="$file_size" \
        -v level="$start_level" \
        -v ue_ver="$ue_version" \
        -v url="$download_url" \
        -v build_date="$build_date" \
        -v date_label="$date_label" '
    BEGIN { found_date_group = 0 }
    
    /<!-- BUILD_LIST_ALL -->/ {
        print
        # Insert new build entry in a date group
        print "      <div class=\"build-group\" data-date=\"" build_date "\">"
        print "        <h3 class=\"date-header\" style=\"color: #fa0; margin: 2rem 0 1rem 0; font-size: 1.2rem;\">" date_label "</h3>"
        print "        <div style=\"background: linear-gradient(135deg, #0a0a0a 0%, #1a1a0a 100%); padding: 1.5rem; border-left: 4px solid #fa0; border-radius: 4px; margin-bottom: 1rem;\">"
        print "          <div class=\"build-header\">"
        print "            <span class=\"platform-tag " platform_class "\">" platform_tag "</span>"
        print "            <span class=\"project-name\">SpiderLily</span>"
        print "            <a href=\"" url "\">" version ".zip</a>"
        print "            <span style=\"margin-left: 0.5rem; color: #666; font-size: 0.9rem;\">(<a href=\"https://assets.aesthetic.computer/false.work/spiderlily-" (platform_class == "platform-windows" ? "windows" : "mac") "-" version ".txt\" style=\"color: #888;\">download log</a>)</span>"
        print "          </div>"
        print "          <div class=\"meta\">" size " MB | " level " | " ue_ver " | <span class=\"build-time\" data-date=\"" timestamp "\">just now</span></div>"
        print "        </div>"
        print "      </div>"
        print "      "
        print "      <h3 class=\"date-header\" style=\"color: #888; margin: 2rem 0 1rem 0; font-size: 1.2rem;\">Previous</h3>"
        
        # Skip all content until we hit the next section or footer
        in_old_builds = 1
        next
    }
    
    in_old_builds && /<h3/ {
        in_old_builds = 0
    }
    
    in_old_builds && /<footer/ {
        in_old_builds = 0
    }
    
    !in_old_builds { print }
    ' /workspaces/aesthetic-computer/system/public/builds.false.work/index.html > /tmp/builds-temp.html
    
    mv /tmp/builds-temp.html /workspaces/aesthetic-computer/system/public/builds.false.work/index.html
end
