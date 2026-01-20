;;; dev-env.el --- Cliff's development environment helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; This module contains helper functions and environment setup that assumes
;; the directory structure described in dev-environment-context.org:
;;   ~/repos
;;   ~/repos/gh
;;   ~/repos/dotfiles
;;   ~/bin
;;   ~/data
;; and so on.

;;; Code:

(defgroup cew/dev-env nil
  "Development environment helpers for Cliff."
  :group 'environment)

(defcustom cew/dev-root (expand-file-name "~/repos")
  "Root directory for all version-controlled repositories."
  :type 'directory
  :group 'cew/dev-env)

(defcustom cew/dev-gh-root (expand-file-name "gh" cew/dev-root)
  "Root directory for GitHub repositories."
  :type 'directory
  :group 'cew/dev-env)

(defcustom cew/dev-data-root (expand-file-name "~/data")
  "Root directory for data sets."
  :type 'directory
  :group 'cew/dev-env)

(defcustom cew/dev-bin-dirs
  (list (expand-file-name "~/bin")
        (expand-file-name "~/.local/bin"))
  "List of user-level bin directories to prepend to PATH and `exec-path`."
  :type '(repeat directory)
  :group 'cew/dev-env)

(defun cew/dev--prepend-to-path (dir)
  "Prepend DIR to PATH and `exec-path` if it exists."
  (when (file-directory-p dir)
    (unless (member dir exec-path)
      (setq exec-path (cons dir exec-path)))
    (let* ((current (getenv "PATH"))
           (path-elems (and current (split-string current path-separator t))))
      (unless (member dir path-elems)
        (setenv "PATH" (concat dir path-separator (or current "")))))))

(defun cew/dev-init-path ()
  "Initialize PATH and `exec-path` according to `cew/dev-bin-dirs`."
  (interactive)
  (dolist (dir cew/dev-bin-dirs)
    (cew/dev--prepend-to-path dir)))

;; Initialize PATH as early as possible
(cew/dev-init-path)

(defun cew/dev-open-repo (name)
  "Open a repository named NAME under `cew/dev-root` using `dired`.
NAME may be \"perseus-aa/paa_graph_kb\" or similar."
  (interactive "sRepo (relative to repos/, e.g. perseus-aa/paa_graph_kb): ")
  (let* ((path (expand-file-name name cew/dev-root)))
    (if (file-directory-p path)
        (dired path)
      (user-error "No such repo directory: %s" path))))

(defun cew/dev-open-gh (name)
  "Open a GitHub-based repository relative to `cew/dev-gh-root`."
  (interactive "sGitHub repo (e.g. perseus-aa/paa_graph_kb): ")
  (let* ((path (expand-file-name name cew/dev-gh-root)))
    (if (file-directory-p path)
        (dired path)
      (user-error "No such GitHub repo directory: %s" path))))

;; Optional integration with project.el (Emacs 28+)
(with-eval-after-load 'project
  (defun cew/dev-project-try (dir)
    "Detect projects under `cew/dev-root` using DIR."
    (let ((root (locate-dominating-file dir ".git")))
      (when (and root (string-prefix-p (file-truename cew/dev-root)
                                       (file-truename root)))
        (cons 'transient root))))

  (add-hook 'project-find-functions #'cew/dev-project-try))

(provide 'dev-env)

;;; dev-env.el ends here
