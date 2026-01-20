#!/usr/bin/env bash
set -euo pipefail

# Link core dotfiles from ~/repos/dotfiles into $HOME.
DOTFILES_ROOT="${DOTFILES_ROOT:-$HOME/repos/dotfiles}"

echo "[link-dotfiles] Using DOTFILES_ROOT=${DOTFILES_ROOT}"

link() {
  local target="$1"
  local linkname="$2"

  if [ -e "${linkname}" ] && [ ! -L "${linkname}" ]; then
    echo "[link-dotfiles] WARNING: ${linkname} exists and is not a symlink; skipping."
    return
  fi

  if [ -L "${linkname}" ]; then
    echo "[link-dotfiles] Replacing existing symlink ${linkname}"
    rm -f "${linkname}"
  fi

  echo "[link-dotfiles] ln -s ${target} ${linkname}"
  ln -s "${target}" "${linkname}"
}

# Zsh
link "${DOTFILES_ROOT}/zsh/zshrc" "${HOME}/.zshrc"

# Git
link "${DOTFILES_ROOT}/git/gitconfig" "${HOME}/.gitconfig"
link "${DOTFILES_ROOT}/git/gitignore_global" "${HOME}/.gitignore_global"

# Emacs
mkdir -p "${HOME}/.emacs.d"
link "${DOTFILES_ROOT}/emacs/cew.org" "${HOME}/.emacs.d/cew.org"
link "${DOTFILES_ROOT}/emacs/init.el" "${HOME}/.emacs.d/init.el"

echo "[link-dotfiles] Done."
