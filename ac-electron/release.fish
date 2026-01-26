#!/usr/bin/env fish

# Release script for Aesthetic Computer Electron app
# Usage: ./release.fish [patch|minor|major|<version>]

set -l bump_type $argv[1]

if test -z "$bump_type"
    set bump_type "patch"
end

cd (dirname (status -f))

# Get current version
set -l current_version (node -p "require('./package.json').version")
echo "Current version: $current_version"

# Calculate new version
switch $bump_type
    case patch minor major
        set -l new_version (npm version $bump_type --no-git-tag-version | tr -d 'v')
    case '*'
        # Assume it's a specific version
        npm version $bump_type --no-git-tag-version
        set new_version $bump_type
end

set -l new_version (node -p "require('./package.json').version")
echo "New version: $new_version"

# Commit and tag
git add package.json package-lock.json
git commit -m "chore(electron): bump version to $new_version"
git tag "electron-v$new_version"

echo ""
echo "Version bumped to $new_version"
echo ""
echo "To release, push the tag:"
echo "  git push origin main && git push origin electron-v$new_version"
echo ""
echo "Or trigger a manual release from GitHub Actions."
