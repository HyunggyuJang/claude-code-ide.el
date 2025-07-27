# CLAUDE.md

This file provides security and development guidance for the claude-code-workspace-management project.

## ⚠️ CRITICAL SECURITY NOTICE

**THIS IS A PUBLIC REPOSITORY** - All content is visible to the world on GitHub.

### NEVER COMMIT:
- API keys, tokens, or secrets of any kind
- Passwords or private keys
- Personal email addresses or sensitive personal information
- Private file paths that reveal system structure
- SSH keys or certificates
- Company-specific configuration that shouldn't be public
- Real workspace names or project names that might reveal sensitive information

### ALLOWED PUBLIC CONTENT:
- General Emacs Lisp code patterns
- IDE integration techniques and protocols
- MCP (Model Context Protocol) implementation examples
- Non-sensitive development environment setups
- Public package configurations
- Generic workspace management patterns

## Security Guidelines

### Configuration Examples
- ✅ **GOOD**: `claude-code-ide-fallback-directory "~/projects"` (generic paths)
- ✅ **GOOD**: `(claude-code-ide--get-workspace-name)` (abstract workspace references)
- ❌ **BAD**: `"/Users/john/secret-company-project"` (specific private paths)
- ❌ **BAD**: Hard-coded API keys or authentication tokens

### Test Data
- ✅ **GOOD**: `"test-workspace"`, `"mock-project"` (generic test names)
- ✅ **GOOD**: Localhost URLs and port numbers for testing
- ❌ **BAD**: Real company workspace names or internal URLs
- ❌ **BAD**: Production server addresses or credentials

### Documentation
- ✅ **GOOD**: Generic examples and patterns
- ✅ **GOOD**: Abstract architectural descriptions
- ❌ **BAD**: Real company infrastructure details
- ❌ **BAD**: Specific internal tool names or configurations

### File Paths
- ✅ **GOOD**: `~/Documents/notes` or `${HOME}/projects`
- ⚠️ **NEUTRAL**: `/tmp/test-data` (acceptable for examples)
- ❌ **BAD**: Paths that reveal sensitive company or personal information

### Email Addresses
- ⚠️ **ACCEPTABLE**: Generic examples like `user@example.com`
- ❌ **BAD**: Real personal or corporate email addresses
- 💡 **TIP**: Use placeholder domains for examples

## Pre-commit Checklist

Before committing ANY changes:

1. **Secret Scan**: Search for common secret patterns
   ```bash
   rg -i "password|secret|key|token|api" --type elisp
   ```

2. **Personal Info Check**: Look for personal paths, email addresses
   ```bash
   rg "/Users/[^/]+" --type elisp
   rg "@.*\.(com|org|net)" --type elisp
   ```

3. **Review Diff**: Always review `git diff` before committing

4. **Test Suite**: Ensure all tests pass
   ```bash
   cd elisp/ && emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit
   ```

## Development Workflow

### Safe Configuration Handling
- Use generic placeholder values in examples
- Reference configuration through variables and functions
- Keep sensitive overrides in local files (gitignored)
- Document configuration requirements without exposing actual values

### Testing
- Use mock data and generic workspace names
- Test with localhost connections only
- Never commit test credentials or real server configurations
- Ensure tests work in isolated environments

### Code Comments
- Keep comments generic and educational
- Avoid references to specific companies or internal systems
- Use abstract examples in documentation
- Focus on technical patterns rather than specific use cases

## Emergency Procedures

### If Sensitive Information Is Accidentally Committed:

1. **DO NOT** just delete the file and commit - information remains in git history
2. **IMMEDIATELY** assess what was exposed
3. **CONTACT** repository maintainers for history rewriting if needed
4. **CONSIDER** using tools like `git-filter-repo` to remove from history
5. **ROTATE** any exposed credentials or change affected configurations

## File Types to Review Carefully

- `*.el` - All Emacs Lisp files
- `*.md` - Documentation and issue files
- `*.json` - Configuration files
- `*.org` - Documentation files
- Test files and mock data
- Any example configurations or documentation

## Safe Examples for Public Repository

### Configuration Patterns
```elisp
;; Good: Generic configuration
(defcustom claude-code-ide-fallback-directory "~/projects"
  "Default directory for project detection.")

;; Bad: Specific personal path
;; (setq my-secret-project-path "/Users/john/company-secrets")
```

### Test Data
```elisp
;; Good: Generic test workspace names
(should (equal (test-workspace-function) "test-workspace"))

;; Bad: Real workspace names
;; (should (equal (get-workspace) "secret-company-project"))
```

## Resources

- [Git Secrets Prevention](https://github.com/awslabs/git-secrets)
- [Open Source Security Best Practices](https://github.com/ossf/scorecard)
- [GitHub Security Documentation](https://docs.github.com/en/code-security)

---

**Remember: When in doubt, DON'T commit. This is an open source project - everything should be safe for public consumption.**
