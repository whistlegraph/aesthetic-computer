# DEPRECATED: Old Node.js + cli.js bundling path for Claude Code in AC Native OS.
# Replaced by native binary bundling in ac-os (line ~82).
# Archived from fedac/native/scripts/build-and-flash.sh on 2026-03-16.

# ── Node.js + Claude Code CLI ──
NODE_BIN="$(command -v node 2>/dev/null || true)"
if [ -n "$NODE_BIN" ] && [ -f "$NODE_BIN" ]; then
    log "Bundling Node.js for Claude Code CLI..."
    cp "$NODE_BIN" "${INITRAMFS_DIR}/bin/node"
    strip --strip-debug "${INITRAMFS_DIR}/bin/node" 2>/dev/null || true
    chmod +x "${INITRAMFS_DIR}/bin/node"
    for lib in $(ldd "$NODE_BIN" 2>/dev/null | grep -oP '/\S+'); do
        [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
    done
    NODE_SIZE=$(du -sh "${INITRAMFS_DIR}/bin/node" | cut -f1)
    log "  node: ${NODE_SIZE} ($(${NODE_BIN} --version))"

    # Bundle Claude Code (x64-linux only, strip other platforms)
    CLAUDE_PKG="$(npm root -g 2>/dev/null)/@anthropic-ai/claude-code"
    if [ -d "$CLAUDE_PKG" ]; then
        log "Bundling Claude Code CLI..."
        CLAUDE_DIR="${INITRAMFS_DIR}/opt/claude-code"
        mkdir -p "${CLAUDE_DIR}/vendor/ripgrep/x64-linux" \
                 "${CLAUDE_DIR}/vendor/tree-sitter-bash/x64-linux"
        # Core files
        cp "${CLAUDE_PKG}/cli.js" "${CLAUDE_DIR}/"
        cp "${CLAUDE_PKG}/package.json" "${CLAUDE_DIR}/"
        [ -f "${CLAUDE_PKG}/resvg.wasm" ] && cp "${CLAUDE_PKG}/resvg.wasm" "${CLAUDE_DIR}/"
        # Vendor binaries (x64-linux only)
        cp -r "${CLAUDE_PKG}/vendor/ripgrep/x64-linux/"* "${CLAUDE_DIR}/vendor/ripgrep/x64-linux/" 2>/dev/null || true
        cp -r "${CLAUDE_PKG}/vendor/tree-sitter-bash/x64-linux/"* "${CLAUDE_DIR}/vendor/tree-sitter-bash/x64-linux/" 2>/dev/null || true
        # Create claude wrapper
        cat > "${INITRAMFS_DIR}/bin/claude" << 'CLAUDE_WRAPPER'
#!/bin/sh
export NODE_PATH=/opt/claude-code
export HOME="${HOME:-/tmp}"
export TERM="${TERM:-dumb}"
exec /bin/node /opt/claude-code/cli.js "$@"
CLAUDE_WRAPPER
        chmod +x "${INITRAMFS_DIR}/bin/claude"
        CLAUDE_SIZE=$(du -sh "${CLAUDE_DIR}" | cut -f1)
        log "  claude-code: ${CLAUDE_SIZE} (x64-linux only)"
    else
        warn "Claude Code not found globally — run: npm i -g @anthropic-ai/claude-code"
    fi
else
    warn "Node.js not found — Claude Code CLI not available"
fi
