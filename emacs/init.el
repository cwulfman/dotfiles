;;; init.el --- minimal loader for cew.org -*- lexical-binding: t; -*-

(require 'org)  ;; built-in, safe to require early
(org-babel-load-file (expand-file-name "cew.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ea650f6ba4839360a905e1857f8fcce823661ade6c8d23fcba67254abc0cba1b"
     "d2c76098def8b2b10b45d2092c86ca9c8b95d58fabbc8850d28899181d8f6581"
     "e604027b0160128a9bd633a63cd111227eea017cfbbe1ff5f3f7c83c9fb72bdf"
     "f76876670af99c2ca9eedecc9bb7559166726800fa9774d9e9630293354f25a4"
     "ba35218e2ee4e80235b226d72c94169195505fee0424cab82bb54f391884d469"
     default))
 '(package-selected-packages
   '(consult corfu denote ef-themes exec-path-from-shell just-ts-mode
             lsp-treemacs magit marginalia orderless org-contrib
             pyvenv royal-hemlock-theme sparql-mode ttl-mode vertico))
 '(sparql-default-base-url "http://localhost:7200/sparql")
 '(sparql-default-format "text/csv")
 '(sparql-prompt-base-url nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
