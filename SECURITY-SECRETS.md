# Security: Secret Detection Setup

This repository has multiple layers of protection to prevent committing secrets, private keys, and credentials.

## 🛡️ Protection Layers

### 1. Git Pre-Commit Hook (Automatic)
- **Location**: `.git/hooks/pre-commit`
- **What it does**: Automatically scans staged files before every commit
- **Detects**:
  - Private keys (RSA, DSA, ECDSA, ED25519, OpenSSH)
  - AWS credentials
  - API keys and tokens
  - Passwords in code
  - Forbidden file patterns

**To bypass** (only if you're absolutely sure): `git commit --no-verify`

### 2. Enhanced .gitignore
- **Location**: `.gitignore`
- Comprehensive patterns for:
  - SSH private keys
  - SSL/TLS certificates
  - Cloud provider credentials (AWS, GCP)
  - Environment files with secrets
  - OAuth tokens
  - Database credentials

### 3. Git Attributes (Binary Marking)
- **Location**: `.gitattributes`
- Marks sensitive files as binary to prevent diffs showing in plain text

### 4. VS Code Extensions (Recommended)

Install one or more of these extensions for real-time secret detection:

```vscode-extensions
gitguardian-secret-security.gitguardian,nolindnaidoo.secrets-le,aquasecurityofficial.trivy-vulnerability-scanner
```

#### Recommended Extensions:
1. **GitGuardian** - Industry standard secret detection
2. **Secrets-LE** - Local-only scanning (no data sent off-machine)
3. **Aqua Trivy** - Vulnerability and secret scanner

## 🚨 What to Do If You Committed a Secret

1. **DO NOT PUSH** if you haven't already
2. Remove the secret from the last commit:
   ```bash
   git reset HEAD~1
   # Remove the secret from files
   git add .
   git commit -m "Your message"
   ```

3. **If already pushed**, follow the incident response:
   - Remove from history: `git filter-branch` or `BFG Repo-Cleaner`
   - Force push: `git push --force --all`
   - **IMMEDIATELY ROTATE THE CREDENTIAL** - it's compromised!
   - Check git reflog and clean up: `git reflog expire --expire=now --all && git gc --prune=now`

## ✅ Testing the Protection

Test that the pre-commit hook works:

```bash
# This should be blocked
echo "password='secret123'" > test-secret.txt
git add test-secret.txt
git commit -m "test"  # Should fail with warning
rm test-secret.txt
```

## 📝 Adding New Secret Patterns

To add new patterns to detect:
1. Edit `.git/hooks/pre-commit`
2. Add pattern to the `PATTERNS` array
3. Test with a sample commit

## 🔑 Managing Secrets Properly

### DO:
- ✅ Use environment variables
- ✅ Store secrets in vault services (AWS Secrets Manager, HashiCorp Vault, 1Password)
- ✅ Use `.env.local` for local development (already in .gitignore)
- ✅ Keep private keys in `secret/` directory (already in .gitignore)
- ✅ Document which secrets are needed (without including values)

### DON'T:
- ❌ Commit private keys
- ❌ Commit `.env` files with real credentials
- ❌ Hardcode API keys in code
- ❌ Share credentials in Slack/email
- ❌ Use `--no-verify` unless you know what you're doing

## 📚 Additional Resources

- [GitGuardian Best Practices](https://docs.gitguardian.com/secrets-detection/secrets-detection-engine/best-practices)
- [GitHub Secret Scanning](https://docs.github.com/en/code-security/secret-scanning)
- [OWASP Secrets Management Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Secrets_Management_Cheat_Sheet.html)

---

**Last Updated**: November 11, 2025
**Maintained by**: Project Security Team
