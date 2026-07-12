#!/data/data/com.termux/files/usr/bin/bash
# Run with:
#   bash mobile/termux-setup.sh

set -euo pipefail

WORKSPACE_DIR="$HOME/workspace"
REPO_DIR="$WORKSPACE_DIR/snow"
EMACS_FILES_DIR="$REPO_DIR/roles/emacs/files"
SYNCTHING_GUI_ADDRESS="127.0.0.1:8385"

log() { printf '\n\033[1;32m==> %s\033[0m\n' "$1"; }

log "Updating package lists and upgrading installed packages"
pkg update -y
pkg upgrade -y

log "Installing required packages"
pkg install -y git aspell aspell-en aspell-de ripgrep syncthing termux-services

log "Symlinking Emacs config files"
mkdir -p "$HOME/.emacs.d"
ln -sf "$EMACS_FILES_DIR/init.el" "$HOME/.emacs.d/init.el"
ln -sf "$EMACS_FILES_DIR/modeline-dark.el" "$HOME/.emacs.d/modeline-dark.el"
ln -sf "$EMACS_FILES_DIR/modeline-light.el" "$HOME/.emacs.d/modeline-light.el"
ln -sf "$EMACS_FILES_DIR/snippets.el" "$HOME/.emacs.d/snippets"
ln -sf "$EMACS_FILES_DIR/eshell" "$HOME/.emacs.d/eshell"

log "Writing early-init.el to expose Termux binaries to Emacs"
cat > "$HOME/.emacs.d/early-init.el" <<'EOF'
(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
                       (getenv "PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)
EOF

log "Setting up Syncthing as a termux-services runit service"
SV_DIR="$PREFIX/var/service/syncthing"
mkdir -p "$SV_DIR/log"
cat > "$SV_DIR/run" <<EOF
#!$PREFIX/bin/sh
exec syncthing --no-browser --gui-address=$SYNCTHING_GUI_ADDRESS 2>&1
EOF
chmod +x "$SV_DIR/run"
ln -sf "$PREFIX/share/termux-services/svlogger" "$SV_DIR/log/run"
rm -f "$SV_DIR/down"

log "Ensuring termux-services is sourced on every Termux startup"
STARTUP_LINE='source $PREFIX/etc/profile.d/start-services.sh'
if ! grep -qF "$STARTUP_LINE" "$HOME/.bashrc" 2>/dev/null; then
  echo "$STARTUP_LINE" >> "$HOME/.bashrc"
fi
# shellcheck disable=SC1091
source "$PREFIX/etc/profile.d/start-services.sh"

log "Enabling and starting the syncthing service"
sv-enable syncthing
sv up syncthing
sleep 1
sv status syncthing

log "Done!"
