#!/bin/sh
set -eu

HOOK_DIR=".git/hooks"
HOOK_FILE="$HOOK_DIR/commit-msg"

mkdir -p "$HOOK_DIR"

cat > "$HOOK_FILE" <<'EOF'
#!/bin/sh
set -eu

MSG_FILE="$1"
MSG="$(cat "$MSG_FILE")"

# Allowed:
#   type: subject
#   type(scope): subject
#
# Types:
#   feat fix docs style refactor perf test build ci chore revert
#
# Scopes (optional, domain only):
#   core solver generator

PATTERN='^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert)(\((core|solver|generator)\))?: .+$'

echo "$MSG" | grep -Eq "$PATTERN" || {
  echo "✗ Invalid commit message."
  echo
  echo "Expected Conventional Commits:"
  echo "  type: subject"
  echo "  type(scope): subject"
  echo
  echo "Examples:"
  echo "  chore: initial repository setup"
  echo "  feat(core): add grid data structure"
  echo "  test(core): add emptyGrid tests"
  echo
  echo "Allowed scopes: core, solver, generator (optional)"
  echo "Allowed types: feat, fix, docs, style, refactor, perf, test, build, ci, chore, revert"
  exit 1
}

exit 0
EOF

chmod +x "$HOOK_FILE"
echo "✓ Installed commit-msg hook into $HOOK_FILE"