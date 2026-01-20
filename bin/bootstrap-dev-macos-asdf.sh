#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Cliff's macOS dev bootstrap script (with asdf integration)
# - Installs Homebrew (if needed)
# - Installs core CLI tools
# - Installs asdf for managing runtimes (python, nodejs, java, clojure, sbcl)
# - Installs PDM and basedpyright via pipx
# - Installs Emacs
# - Sets up shell environment hooks
# ============================================================

log() {
  printf "\n[BOOTSTRAP] %s\n" "$*"
}

# ------------------------------
# 0. Xcode command line tools
# ------------------------------
log "Checking for Xcode Command Line Tools..."
if ! xcode-select -p >/dev/null 2>&1; then
  log "Xcode Command Line Tools not found. Triggering installer..."
  xcode-select --install || true
  log "Please complete the GUI installer, then re-run this script."
  exit 1
else
  log "Xcode Command Line Tools found."
fi

# ------------------------------
# 1. Homebrew
# ------------------------------
if ! command -v brew >/dev/null 2>&1; then
  log "Homebrew not found. Installing Homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
  log "Homebrew already installed. Skipping."
fi

# Ensure brew is in PATH for this script
if [[ "$(uname -m)" == "arm64" ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
else
  eval "$(/usr/local/bin/brew shellenv)"
fi

log "Updating Homebrew..."
brew update

# ------------------------------
# 2. Core packages via Homebrew
# ------------------------------
log "Installing core CLI packages..."
brew install \
  git \
  wget \
  curl \
  jq \
  pipx \
  graphviz \
  asdf \
  emacs

# ------------------------------
# 3. Shell configuration (.zshrc)
# ------------------------------
ZSHRC="${HOME}/.zshrc"

log "Ensuring Homebrew shellenv and asdf init are in ~/.zshrc..."

# Add brew shellenv snippet if not present
BREW_SHELLENV_LINE='eval "$($(brew --prefix)/bin/brew shellenv)"'
if ! grep -Fq "$BREW_SHELLENV_LINE" "$ZSHRC" 2>/dev/null; then
  {
    echo ""
    echo "# Added by dev bootstrap (Homebrew)"
    echo "$BREW_SHELLENV_LINE"
  } >> "$ZSHRC"
fi

# Add asdf init if not present
ASDF_INIT_LINE='. "$(brew --prefix asdf)/libexec/asdf.sh"'
if ! grep -Fq "$ASDF_INIT_LINE" "$ZSHRC" 2>/dev/null; then
  {
    echo ""
    echo "# Added by dev bootstrap (asdf)"
    echo "$ASDF_INIT_LINE"
  } >> "$ZSHRC"
fi

# Ensure pipx/LOCAL bin paths (pipx ensurepath usually does this, but we guard)
if ! grep -Fq 'export PATH="$HOME/.local/bin:$PATH"' "$ZSHRC" 2>/dev/null; then
  {
    echo ""
    echo "# Added by dev bootstrap (pipx)"
    echo 'export PATH="$HOME/.local/bin:$PATH"'
  } >> "$ZSHRC"
fi

# PDM_HOME is nice to have explicitly
if ! grep -Fq 'export PDM_HOME=' "$ZSHRC" 2>/dev/null; then
  {
    echo ""
    echo "# PDM configuration"
    echo 'export PDM_HOME="$HOME/.local/share/pdm"'
  } >> "$ZSHRC"
fi

log "Reloading shell environment for this script run..."
# Re-evaluate Brew and asdf for this script invocation
eval "$($(brew --prefix)/bin/brew shellenv)"
. "$(brew --prefix asdf)/libexec/asdf.sh"

# ------------------------------
# 4. asdf runtimes (python, nodejs, java, clojure, sbcl)
# ------------------------------
log "Adding asdf plugins (python, nodejs, java, clojure, sbcl) if needed..."

asdf plugin add python  >/dev/null 2>&1 || log "python plugin already present"
asdf plugin add nodejs >/dev/null 2>&1 || log "nodejs plugin already present"
asdf plugin add java   >/dev/null 2>&1 || log "java plugin already present"

asdf plugin add clojure https://github.com/halcyon/asdf-clojure.git >/dev/null 2>&1 || log "clojure plugin already present"
asdf plugin add sbcl    https://github.com/smashedtoatoms/asdf-sbcl.git >/dev/null 2>&1 || log "sbcl plugin already present"

log "Installing baseline runtimes with asdf..."

PY_VER="3.12.4"
NODE_VER="22.5.1"
JAVA_VER="temurin-21.0.4+7"
CLJ_VER="1.11.3.1463"   # adjust if this exact version is unavailable
SBCL_VER="2.4.11"       # adjust to a valid version from `asdf list-all sbcl`

asdf install python "${PY_VER}" || true
asdf global python "${PY_VER}"

# NodeJS plugin requires importing release team keys once.
# If you haven't done so previously, you may need to run:
#   bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring
# before installing nodejs versions.
if ! asdf list nodejs "${NODE_VER}" >/dev/null 2>&1; then
  log "Attempting nodejs ${NODE_VER} install. If this fails due to GPG keys, run:"
  log "  bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring"
  asdf install nodejs "${NODE_VER}" || true
fi
asdf global nodejs "${NODE_VER}" || true

asdf install java "${JAVA_VER}" || true
asdf global java "${JAVA_VER}"

# Clojure (CLI tools)
asdf install clojure "${CLJ_VER}" || true
asdf global clojure "${CLJ_VER}"

# Common Lisp (SBCL)
asdf install sbcl "${SBCL_VER}" || true
asdf global sbcl "${SBCL_VER}"

log "asdf runtime setup complete:"
asdf current || true

# ------------------------------
# 5. Python tooling: pipx, PDM, basedpyright
# ------------------------------
log "Ensuring pipx is on PATH..."
pipx ensurepath || true

log "Installing PDM and basedpyright via pipx..."
pipx install pdm || true
pipx install basedpyright || true

# ------------------------------
# 6. Final messages
# ------------------------------
cat <<'EOF'

============================================================
Bootstrap complete (from the script's perspective).

Next steps:

1. Open a NEW terminal so that updated PATH and environment
   from ~/.zshrc take effect (brew, asdf, pipx, PDM_HOME).

2. Verify tools:
   - brew --version
   - asdf --version
   - asdf current
   - python --version
   - node --version
   - java --version
   - clojure --version
   - sbcl --version
   - pdm --version
   - basedpyright --version
   - emacs --version

3. For Emacs:
   - Put your config in ~/.emacs.d/ (e.g. cew.org + init.el)
   - Start Emacs and let it install packages.

4. For new Python projects:
   - Use your template:
       git clone https://github.com/cwulfman/python-project-template myproj
       cd myproj
       pdm install -d
   - Consider adding a .tool-versions file like:
       python ${PY_VER}
       nodejs ${NODE_VER}
       java ${JAVA_VER}
       clojure ${CLJ_VER}
       sbcl ${SBCL_VER}
   - Emacs + Eglot should pick up basedpyright and .venv automatically.

5. For GraphDB:
   - Download the latest GraphDB distribution from Ontotext.
   - Unpack to: ~/opt/graphdb
   - Start it per their instructions; then point Emacs/SPARQL tools
     at http://localhost:7200/repositories/your-repo

You can safely re-run this script; it is mostly idempotent.
============================================================

EOF

log "Done."
