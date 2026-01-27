;;; -*- lexical-binding: t -*-

;; The rest of your Emacs Lisp code follows...
(message "Lexical binding is: %S" lexical-binding)

;; Speed up startup a bit
(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose nil)

;; Ensure dotfiles/emacs is on the load-path, then load dev-env.el
(add-to-list 'load-path
             (expand-file-name "emacs"
                               (expand-file-name "repos/dotfiles" (getenv "HOME"))))

(require 'dev-env)

;; Keybindings for quickly jumping into project trees

;; Open any repo under ~/repos
(global-set-key (kbd "C-c p r") #'cew/dev-open-repo)

;; Open a GitHub repo under ~/repos/gh (e.g. "perseus-aa/paa_graph_kb")
(global-set-key (kbd "C-c p g") #'cew/dev-open-gh)

;; Optionally re-run PATH sync manually if you tweak bin dirs
(global-set-key (kbd "C-c p e") #'cew/dev-init-path)

(add-hook 'after-init-hook #'cew/dev-init-path)

(defun cew/asdf-current ()
  "Show the current asdf runtimes in a temporary buffer."
  (interactive)
  (let ((buf (get-buffer-create "*asdf-current*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Directory: %s\n\n" default-directory))
      (let ((exit-code (call-process "asdf" nil t nil "current")))
        (if (eq exit-code 0)
            (progn
              (goto-char (point-min))
              (special-mode))
          (insert "\n\n(asdf current failed; is asdf on PATH?)")
          (goto-char (point-min))
          (special-mode))))
    (pop-to-buffer buf)))

(defun cew/open-tool-versions ()
  "Open the nearest .tool-versions file (if any) in the current project tree."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".tool-versions")
                   (user-error "No .tool-versions found above %s" default-directory)))
         (file (expand-file-name ".tool-versions" root)))
    (find-file file)))

(defun cew/asdf-info-echo ()
  "Echo brief asdf info in the minibuffer (current Python, Node, Java)."
  (interactive)
  (let* ((output (with-temp-buffer
                   (when (eq 0 (call-process "asdf" nil t nil "current"))
                     (buffer-string))))
         (summary (and output
                       (mapconcat
                        (lambda (line)
                          (cond
                           ((string-match "^python \\([^ ]+\\)" line)
                            (format "py:%s" (match-string 1 line)))
                           ((string-match "^nodejs \\([^ ]+\\)" line)
                            (format "node:%s" (match-string 1 line)))
                           ((string-match "^java \\([^ ]+\\)" line)
                            (format "java:%s" (match-string 1 line)))
                           (t nil)))
                        (split-string output "\n" t)
                        " "))))
    (message "%s"
             (or summary
                 "asdf: no current versions (or asdf not available)"))))

;; Convenient keybindings
(global-set-key (kbd "C-c a c") #'cew/asdf-current)
(global-set-key (kbd "C-c a t") #'cew/open-tool-versions)
(global-set-key (kbd "C-c a i") #'cew/asdf-info-echo)

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Show line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Better defaults
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-default nil
      create-lockfiles nil)

;; Highlight current line
(global-hl-line-mode 0)

;; macOS clipboard integration is mostly automatic, but:
(setq select-enable-clipboard t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (dolist (var '("PATH" "PYENV_ROOT" "PDM_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :bind (;; Example keybindings
         ("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("C-x C-r" . consult-recent-file)))

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1))

;; project.el is built-in; configure a simple switcher
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p p") #'project-switch-project)

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package pyvenv
    :config
    (pyvenv-tracking-mode 1))

  (defun cew/project-root ()
    "Return the current project root, or nil."
    (when-let ((proj (project-current)))
      (project-root proj)))

  (defun cew/python-find-dot-venv ()
    "Return path to .venv under project root, or nil."
    (when-let* ((root (cew/project-root))
                (venv (expand-file-name ".venv" root)))
      (when (file-directory-p venv)
        venv)))

  (defun cew/pdm-venv-path ()
    "Try to get a PDM venv path for this project via `pdm venv list`."
    (when-let ((root (cew/project-root)))
      (with-temp-buffer
        (let ((default-directory root))
          (when (eq 0 (call-process "pdm" nil t nil "venv" "list" "--format" "{path}"))
            (goto-char (point-min))
            (when (re-search-forward "^\\S-" nil t)
              (string-trim
               (buffer-substring (line-beginning-position)
                                 (line-end-position)))))))))

  (defun cew/python-detect-venv ()
    "Detect a venv for the current project.
Order:
  1. .venv/ inside project root
  2. First venv reported by `pdm venv list`."
    (or (cew/python-find-dot-venv)
        (cew/pdm-venv-path)))

  (defun cew/python-auto-activate-venv ()
    "Automatically activate a project virtualenv for Python buffers."
    (when-let ((venv (cew/python-detect-venv)))
      (when (file-directory-p venv)
        (message "Activating venv: %s" venv)
        (pyvenv-activate venv))))

  (add-hook 'python-mode-hook #'cew/python-auto-activate-venv)
  (when (boundp 'python-ts-mode-hook)
    (add-hook 'python-ts-mode-hook #'cew/python-auto-activate-venv))

;; Built-in python-mode is fine; Emacs 30 has python-ts-mode with tree-sitter.
;; Use whichever Emacs chooses as default; hooks above handle both.

(setq python-shell-interpreter "python"
      python-indent-offset 4)

(use-package eglot
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio")))
  (setq eldoc-echo-area-use-multiline-p 3))

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-return-follows-link t)
  (setq org-id-link-to-org-use-id t)

  ;; Make sure IDs can be indexed broadly
  (setq org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory))
  (setq org-id-extra-files (directory-files-recursively (expand-file-name "~/org") "\\.org$"))

  (require 'org-id)

  ;; Ensure id: links have completion support
  (org-link-set-parameters "id"
                           :follow #'org-id-open
                           :store  #'org-id-store-link-maybe
                           :complete #'org-id-complete-link))

(use-package org-contrib
  :after org)

(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)

(defvar my/org-basic-task-template "* TODO %^{Task}
    :PROPERTIES:
    :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
    :END:
    Captured %<%Y-%m-%d %H:%M>
    %?

    %i
    " "Basic task data")

    (setq org-capture-templates
          (quote
    (
    ("j" "Journal Entry" entry
     (file+olp+datetree "~/personal/journal.org")
    "* %U
    %?
    %i
    %a")

  ("rq" "Reading: Quote" entry
   (file+headline "~/org/reading/inbox.org" "Inbox")
   "** Quote
  :PROPERTIES:
  :CREATED:  %U
  :LOC:      %^{Location}
  :SOURCE:   %^{Source}
  :END:
  #+begin_quote
  %i
  #+end_quote
  %?"
   :empty-lines 1)

  ("rt" "Reading: Thought" entry
 (file+headline "~/org/reading/inbox.org" "Inbox")
 "** Thought
:PROPERTIES:
:CREATED:  %U
:LOC:      %^{Location}
:SOURCE:   %^{Source}
:END:
%?"
 :empty-lines 1)
    	
    ("t" "Todo" entry
     (file+olp "~/gtd/gtd.org" "INBOX")
    "* TODO %?\n  %i\n  %a")

    ("m" "Meeting" entry
     (file+olp "~/gtd/notes.org" "Meetings")
     "* %U MEETING with %? :MEETING:
    ** Notes

    ** Actions
    "
     :clock-in t :clock-resume t)
    	
    ("p" "Phone" entry
     (file+olp "~/gtd/notes.org" "Meetings")
    "
    * %U CALL with %? :CALL:
     ** Notes

     ** Actions
     " :clock-in t :clock-resume t)
    	
    ("n" "Note" entry
     (file+headline "~/gtd/notes.org" "Notes")
     "* %u %?" :prepend t)
    )))

;; ============================================================================
;; Multi-Project Management with Org-Mode
;; ============================================================================

;; Project files location
(setq org-agenda-files '("~/org/clients/"
                         "~/org/personal.org"))

;; Enhanced TODO keywords for project management
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "PROJECT(p)" "|" "COMPLETED(C)")))

;; Log everything into drawers for clean headlines
(setq org-log-done 'time
      org-log-into-drawer t
      org-clock-into-drawer t)

;; Track clock history across sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Startup settings
(setq org-startup-folded 'overview
      org-startup-indented t)

;; Custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Daily Dashboard"
         ((agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")))
          (todo "WAITING"
                ((org-agenda-overriding-header "Blocked/Waiting")))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled TODOs")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
        
        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-day "-1d")))
          (todo "PROJECT"
                ((org-agenda-overriding-header "Active Projects")))
          (stuck ""
                 ((org-agenda-overriding-header "Stuck Projects")))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting For")))))
        
        ("p" "By Client"
         ((tags-todo "CATEGORY=\"princeton\""
                     ((org-agenda-overriding-header "Princeton")))
          (tags-todo "CATEGORY=\"perseus\""
                     ((org-agenda-overriding-header "Perseus")))
          (tags-todo "CATEGORY=\"dh-agency\""
                     ((org-agenda-overriding-header "DH Agency")))
          (tags-todo "CATEGORY=\"oset\""
                     ((org-agenda-overriding-header "OSET")))
          (tags-todo "CATEGORY=\"personal\""
                     ((org-agenda-overriding-header "Personal")))))
        
        ("r" "Clock Report - This Month"
         ((tags "CATEGORY=\"princeton\""
                ((org-agenda-overriding-header "Princeton - This Month")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                 (org-agenda-archives-mode t)))
          (tags "CATEGORY=\"perseus\""
                ((org-agenda-overriding-header "Perseus - This Month")))
          (tags "CATEGORY=\"dh-agency\""
                ((org-agenda-overriding-header "DH Agency - This Month")))
          (tags "CATEGORY=\"oset\""
                ((org-agenda-overriding-header "OSET - This Month"))))
         ((org-agenda-start-with-clockreport-mode t)
          (org-agenda-clockreport-parameter-plist
           '(:maxlevel 3 :scope agenda :block thismonth :fileskip0 t))))))

;; Capture templates for quick task entry
(add-to-list 'org-capture-templates
             '("w" "Work Task" entry
               (file+headline "~/org/clients/princeton.org" "Quick Tasks")
               "* TODO %?\n  SCHEDULED: %t\n  %i"
               :empty-lines 1))

(add-to-list 'org-capture-templates
             '("m" "Meeting" entry
               (file+headline "~/org/clients/princeton.org" "Meetings")
               "* DONE Meeting with %? - %U\n\n** Notes\n\n** Action Items\n*** TODO "
               :empty-lines 1
               :clock-in t
               :clock-resume t))

(add-to-list 'org-capture-templates
             '("j" "Journal Entry" entry
               (file+datetree "~/org/journal.org")
               "* %?\n  Entered on %U\n  %i"
               :empty-lines 1))

;; Helper function for Princeton monthly report
(defun my/princeton-monthly-report ()
  "Generate a monthly clock report for Princeton projects."
  (interactive)
  (let ((org-agenda-files '("~/org/clients/princeton.org")))
    (org-clock-report)
    (org-clocktable-write-default)
    (message "Princeton monthly report generated")))

;; Helper function to link to GitHub issues
(defun my/insert-github-issue-link ()
  "Insert a GitHub issue link in properties drawer."
  (interactive)
  (let ((url (read-string "GitHub Issue URL: ")))
    (org-set-property "GITHUB_ISSUE" url)))

(global-set-key (kbd "C-c g i") 'my/insert-github-issue-link)

;; Refile configuration - allows moving tasks between files
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Archive configuration
(setq org-archive-location "~/org/archive/%s_archive::")

;; Project files location
(setq org-agenda-files '("~/org/clients/"
                         "~/org/personal.org"))

;; Enhanced TODO keywords for project management
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "PROJECT(p)" "|" "COMPLETED(C)")))

;; Log everything into drawers
(setq org-log-done 'time
      org-log-into-drawer t
      org-clock-into-drawer t)

;; Track clock history across sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Daily Dashboard"
         ((agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")))
          (todo "WAITING"
                ((org-agenda-overriding-header "Blocked/Waiting")))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled TODOs")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
        
        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-day "-1d")))
          (todo "PROJECT"
                ((org-agenda-overriding-header "Active Projects")))
          (stuck ""
                 ((org-agenda-overriding-header "Stuck Projects")))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting For")))))
        
        ("p" "By Client"
         ((tags-todo "CATEGORY=\"princeton\""
                     ((org-agenda-overriding-header "Princeton")))
          (tags-todo "CATEGORY=\"perseus\""
                     ((org-agenda-overriding-header "Perseus")))
          (tags-todo "CATEGORY=\"dh-agency\""
                     ((org-agenda-overriding-header "DH Agency")))
          (tags-todo "CATEGORY=\"oset\""
                     ((org-agenda-overriding-header "OSET")))
          (tags-todo "CATEGORY=\"personal\""
                     ((org-agenda-overriding-header "Personal")))))
        
        ("r" "Clock Report - This Month"
         ((tags "CATEGORY=\"princeton\""
                ((org-agenda-overriding-header "Princeton - This Month")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                 (org-agenda-archives-mode t)))
          (tags "CATEGORY=\"perseus\""
                ((org-agenda-overriding-header "Perseus - This Month")))
          (tags "CATEGORY=\"dh-agency\""
                ((org-agenda-overriding-header "DH Agency - This Month")))
          (tags "CATEGORY=\"oset\""
                ((org-agenda-overriding-header "OSET - This Month"))))
         ((org-agenda-start-with-clockreport-mode t)
          (org-agenda-clockreport-parameter-plist
           '(:maxlevel 3 :scope agenda :block thismonth :fileskip0 t))))))

;; Capture templates for quick task entry
(add-to-list 'org-capture-templates
             '("w" "Work Task" entry
               (file+headline "~/org/clients/princeton.org" "Tasks")
               "* TODO %?\n  SCHEDULED: %t\n  %i"
               :empty-lines 1))

(add-to-list 'org-capture-templates
             '("m" "Meeting" entry
               (file+headline "~/org/clients/princeton.org" "Meetings")
               "* DONE Meeting with %? - %U\n\n** Notes\n\n** Action Items\n*** TODO "
               :empty-lines 1
               :clock-in t
               :clock-resume t))

;; Helper function for Princeton monthly report
(defun my/princeton-monthly-report ()
  "Generate a monthly clock report for Princeton projects."
  (interactive)
  (let ((org-agenda-files '("~/org/clients/princeton.org")))
    (org-clock-report)
    (org-clocktable-write-default)
    (message "Princeton monthly report generated")))

;; Helper function to link to GitHub issues
(defun my/insert-github-issue-link ()
  "Insert a GitHub issue link in properties drawer."
  (interactive)
  (let ((url (read-string "GitHub Issue URL: ")))
    (org-set-property "GITHUB_ISSUE" url)))

(global-set-key (kbd "C-c g i") 'my/insert-github-issue-link)

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/personal/notes/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package org-contrib
  :after org)

  ;; Load ob-sparql from org-contrib (if available)
  (with-eval-after-load 'org
    (require 'ob-sparql nil 'noerror)
    (add-to-list 'org-babel-load-languages '(sparql . t)))

  ;; Optional default settings for SPARQL blocks
  (setq org-babel-default-header-args:sparql
        '((:results . "table")
          (:endpoint . "http://localhost:7200/repositories/OSET")))

(use-package ttl-mode
  :mode ("\\.ttl\\'" . ttl-mode))

(use-package sparql-mode
  :mode ("\\.rq\\'" . sparql-mode))

(use-package org-contrib
  :after org)

;; Load ob-sparql from org-contrib (if available)
(with-eval-after-load 'org
  (require 'ob-shell nil 'noerror)
  (add-to-list 'org-babel-load-languages '(shell . t)))

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 4 1000 1000))))
