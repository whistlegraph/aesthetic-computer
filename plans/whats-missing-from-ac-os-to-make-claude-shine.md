# What's Missing from AC OS to Make Claude Shine

A prioritized list of tools and improvements needed to make Claude Code
work great on a native AC device (notepat or similar).

---

## Current Binary Situation

`/bin` has 25 entries. Notable **present**: `bash`, `curl`, `grep`, `awk`,
`sed`, `cat`, `head`, `cut`, `ls`, `mkdir`, `chmod`, `which`.

Notable **absent**: everything below.

---

## Tier 1 — Claude Code Breaks Without These

| Tool | Size (static) | Why it matters |
|------|--------------|----------------|
| `git` | ~12MB | CC's primary op: status, diff, log, add, commit, push, clone |
| `find` | ~200KB (busybox) | CC uses it constantly for file discovery |
| `cp` `mv` `rm` `ln` | ~200KB (busybox) | Basic file ops — CC shells out to these routinely |
| `wc` | ~200KB (busybox) | Line/word counts in every pipeline |
| `tail` | ~200KB (busybox) | Reading logs, watching output |

**Recommendation:** Add `busybox` as a single ~1.5MB static binary.
It covers all of the above (and tier 2) in one shot via symlinks.

For `git`: use a statically-linked build from Alpine/musl (~12MB).
It needs HTTPS + credential helper support for GitHub PAT auth.

---

## Tier 2 — CC Works Poorly Without These

All covered by `busybox`:

| Tool | Why |
|------|-----|
| `sort` `uniq` | Data processing in shell pipelines |
| `tr` | Character translation (`tr -d '\r'` etc.) |
| `tee` | Splitting output to file + stdout |
| `xargs` | Building command lines from stdin |
| `diff` | CC diffs files before patching |
| `stat` | File metadata (size, mtime) |
| `ps` | Process listing |

---

## Tier 3 — Nice to Have

| Tool | Size (static) | Why |
|------|--------------|-----|
| `rg` (ripgrep) | ~2MB | CC prefers rg over grep — much faster for large repos |
| `jq` | ~1MB | JSON parsing in scripts, useful everywhere |
| `ssh` / `scp` | ~1MB | Remote access, already referenced in prompt.mjs |
| `tar` / `gzip` | ~200KB (busybox) | Archives |
| `python3` | ~8MB | Scripting, but large |
| `make` | ~400KB | Build tool for C projects |
| `patch` | ~200KB | Applying diffs |

---

## Implementation Plan

### Step 1: Add busybox (covers all of Tier 1 except git, and all Tier 2)

In `fedac/native/initramfs/` staging area:

```bash
# Download static busybox binary
curl -L https://busybox.net/downloads/binaries/1.35.0-x86_64-linux-musl/busybox \
  -o initramfs/bin/busybox
chmod +x initramfs/bin/busybox

# Create symlinks for the applets we need
cd initramfs/bin
for cmd in find cp mv rm ln wc tail sort uniq tr tee xargs diff stat ps tar gzip; do
  ln -sf busybox $cmd
done
```

### Step 2: Add static git

```bash
# Alpine-based static git (musl, x86_64)
# Download from Alpine package mirror, extract git binary + deps
# OR use the pre-built static git from:
# https://github.com/nicowillis/static-git (or similar)
curl -L <static-git-url> -o initramfs/bin/git
chmod +x initramfs/bin/git

# Git needs ca-certs for HTTPS (already present at /etc/pki/tls/certs/ca-bundle.crt)
# Git credential helper: write a simple shell script that reads /github-pat
cat > initramfs/bin/git-credential-ac << 'EOF'
#!/bin/sh
# Serves GitHub PAT from /github-pat for git HTTPS auth
while IFS= read -r line; do
  case "$line" in
    host=github.com) echo "username=whistlegraph"; cat /github-pat | tr -d '\n' | sed 's/^/password=/'; echo; exit 0;;
  esac
done
EOF
chmod +x initramfs/bin/git-credential-ac
```

Configure git in the PTY spawn environment (`pty.c`) to use the credential helper:

```c
setenv("GIT_CREDENTIAL_HELPER", "ac", 1);
// or set globally in child:
// git config --global credential.helper ac
```

### Step 3: Add ripgrep

```bash
# Static ripgrep binary (~2MB)
curl -L https://github.com/BurntSushi/ripgrep/releases/latest/download/ripgrep-x86_64-unknown-linux-musl.tar.gz \
  | tar -xz --strip-components=1 -C initramfs/bin/ ripgrep-*/rg
chmod +x initramfs/bin/rg
```

### Step 4: Add jq

```bash
curl -L https://github.com/jqlang/jq/releases/latest/download/jq-linux-amd64 \
  -o initramfs/bin/jq
chmod +x initramfs/bin/jq
```

---

## Initramfs Size Impact

| Addition | Size |
|----------|------|
| busybox (static) | ~1.5MB |
| git (static) | ~12MB |
| ripgrep | ~2MB |
| jq | ~1MB |
| **Total** | **~16.5MB** |

The current compressed initramfs is baked into the kernel image. 16MB of new
binaries will add ~6-8MB compressed. Should be fine; EFI partition is 512MB.

---

## Also Worth Noting

### Source on the device
Currently the AC source repo is not on the device — Claude has to use the
GitHub API via curl to read/write source files. Adding `git` to the initramfs
would let Claude clone `aesthetic-computer` to `/tmp` and work on it directly
(with full git history, branch support, etc.). Much more natural workflow.

### The git credential situation
`pty.c` already writes `GH_TOKEN` and `GITHUB_TOKEN` to the environment from
`/github-pat`. Git will pick up `GH_TOKEN` natively for HTTPS auth on github.com
as of git 2.39+. So no credential helper should be needed beyond the env var.

### Consider also: `/tmp` size
`/tmp` is tmpfs. A cloned `aesthetic-computer` repo is ~300MB. The tmpfs may
need a size limit set in the kernel cmdline or init (default is usually 50% of RAM).
Check available RAM before cloning large repos.

---

*Written 2026-03-16 by Claude on notepat-391a0e5e*
