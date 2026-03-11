#!/usr/bin/env sh
set -eu

usage() {
  cat <<'EOF'
Usage:
  scripts/install_omni.sh install [options]
  scripts/install_omni.sh uninstall [options]

Options:
  --binary <path>            Source Omni binary (default: <repo>/build/main)
  --prefix <dir>             Install prefix (default: ~/.local)
  --bindir <dir>             Launcher install dir (default: <prefix>/bin)
  --libexecdir <dir>         Internal binary dir (default: <prefix>/lib/omni)
  --manroot <dir>            Man root dir (default: <prefix>/share/man)
  --man1dir <dir>            man1 install dir (default: <manroot>/man1)
  --man7dir <dir>            man7 install dir (default: <manroot>/man7)
  --mandir <dir>             Alias for --man1dir
  --ld-library-path <value>  Base LD_LIBRARY_PATH for launcher
                             (default: /usr/local/lib:/opt/libtorch/lib)
  -h, --help                 Show this help

Examples:
  scripts/install_omni.sh install
  scripts/install_omni.sh install --prefix /usr/local
  scripts/install_omni.sh uninstall --prefix /usr/local
EOF
}

if [ $# -lt 1 ]; then
  usage
  exit 2
fi

action="$1"
shift

case "$action" in
  install|uninstall) ;;
  -h|--help)
    usage
    exit 0
    ;;
  *)
    echo "Unknown action: $action" >&2
    usage
    exit 2
    ;;
esac

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"

BINARY_PATH="$REPO_ROOT/build/main"
PREFIX="${HOME}/.local"
BINDIR=""
LIBEXECDIR=""
MANROOT=""
MAN1DIR=""
MAN7DIR=""
LD_LIB_VALUE="/usr/local/lib:/opt/libtorch/lib"
MAN1_SOURCE="$REPO_ROOT/docs/man/omni.1"
MAN7_SOURCE="$REPO_ROOT/docs/man/omni-language.7"

while [ $# -gt 0 ]; do
  case "$1" in
    --binary)
      [ $# -ge 2 ] || { echo "--binary requires a value" >&2; exit 2; }
      BINARY_PATH="$2"
      shift 2
      ;;
    --prefix)
      [ $# -ge 2 ] || { echo "--prefix requires a value" >&2; exit 2; }
      PREFIX="$2"
      shift 2
      ;;
    --bindir)
      [ $# -ge 2 ] || { echo "--bindir requires a value" >&2; exit 2; }
      BINDIR="$2"
      shift 2
      ;;
    --libexecdir)
      [ $# -ge 2 ] || { echo "--libexecdir requires a value" >&2; exit 2; }
      LIBEXECDIR="$2"
      shift 2
      ;;
    --manroot)
      [ $# -ge 2 ] || { echo "--manroot requires a value" >&2; exit 2; }
      MANROOT="$2"
      shift 2
      ;;
    --man1dir|--mandir)
      [ $# -ge 2 ] || { echo "$1 requires a value" >&2; exit 2; }
      MAN1DIR="$2"
      shift 2
      ;;
    --man7dir)
      [ $# -ge 2 ] || { echo "--man7dir requires a value" >&2; exit 2; }
      MAN7DIR="$2"
      shift 2
      ;;
    --ld-library-path)
      [ $# -ge 2 ] || { echo "--ld-library-path requires a value" >&2; exit 2; }
      LD_LIB_VALUE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage
      exit 2
      ;;
  esac
done

[ -n "$BINDIR" ] || BINDIR="$PREFIX/bin"
[ -n "$LIBEXECDIR" ] || LIBEXECDIR="$PREFIX/lib/omni"
[ -n "$MANROOT" ] || MANROOT="$PREFIX/share/man"
[ -n "$MAN1DIR" ] || MAN1DIR="$MANROOT/man1"
[ -n "$MAN7DIR" ] || MAN7DIR="$MANROOT/man7"

LAUNCHER_PATH="$BINDIR/omni"
TARGET_BIN="$LIBEXECDIR/omni-bin"
TARGET_MAN1="$MAN1DIR/omni.1"
TARGET_MAN7="$MAN7DIR/omni-language.7"

if [ "$action" = "install" ]; then
  [ -x "$BINARY_PATH" ] || {
    echo "Omni binary not found or not executable: $BINARY_PATH" >&2
    echo "Build first: c3c build" >&2
    exit 1
  }
  [ -f "$MAN1_SOURCE" ] || {
    echo "Man page source not found: $MAN1_SOURCE" >&2
    exit 1
  }
  [ -f "$MAN7_SOURCE" ] || {
    echo "Man page source not found: $MAN7_SOURCE" >&2
    exit 1
  }

  install -d "$BINDIR" "$LIBEXECDIR" "$MAN1DIR" "$MAN7DIR"
  install -m 755 "$BINARY_PATH" "$TARGET_BIN"
  install -m 644 "$MAN1_SOURCE" "$TARGET_MAN1"
  install -m 644 "$MAN7_SOURCE" "$TARGET_MAN7"

  cat >"$LAUNCHER_PATH" <<EOF
#!/bin/sh
exec env LD_LIBRARY_PATH="$LD_LIB_VALUE\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}" \\
    "$TARGET_BIN" "\$@"
EOF
  chmod 755 "$LAUNCHER_PATH"

  echo "Installed:"
  echo "  launcher: $LAUNCHER_PATH"
  echo "  binary:   $TARGET_BIN"
  echo "  man page: $TARGET_MAN1"
  echo "  man page: $TARGET_MAN7"
  echo
  echo "Verify:"
  echo "  omni --version"
  echo "  man -M \"${MANROOT}\" 1 omni"
  echo "  man -M \"${MANROOT}\" 7 omni-language"
  exit 0
fi

rm -f "$LAUNCHER_PATH" "$TARGET_BIN" "$TARGET_MAN1" "$TARGET_MAN7"
rmdir "$LIBEXECDIR" 2>/dev/null || true
rmdir "$MAN1DIR" 2>/dev/null || true
rmdir "$MAN7DIR" 2>/dev/null || true

echo "Uninstalled (if present):"
echo "  $LAUNCHER_PATH"
echo "  $TARGET_BIN"
echo "  $TARGET_MAN1"
echo "  $TARGET_MAN7"
