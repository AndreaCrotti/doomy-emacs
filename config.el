;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(add-to-list 'load-path (file-truename "~/.config/doom"))
(require 'functions)
(require 'aliases)

;; helper functions
(setq user-full-name "Andrea Crotti"
      user-mail-address "andrea.crotti.0@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 18))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/RoamNotes/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(global-subword-mode 1)

(add-hook! 'text-mode (lambda () (auto-revert-mode 1)))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and (eq major-mode 'comint-mode)
             (string-match "finished" string)
             (not
              (with-current-buffer buffer
                (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (let ((window (get-buffer-window buf)))
                        (when (and (window-live-p window)
                                   (eq buf (window-buffer window)))
                          (delete-window window))))
                    buffer)))

(add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful)

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

(setq-hook! org-mode
  prettify-symbols-alist '(("#+end_quote" . "”")
                           ("#+END_QUOTE" . "”")
                           ("#+begin_quote" . "“")
                           ("#+BEGIN_QUOTE" . "“")
                           ("#+end_src" . "«")
                           ("#+END_SRC" . "«")
                           ("#+begin_src" . "»")
                           ("#+BEGIN_SRC" . "»")
                           ("#+name:" . "»")
                           ("#+NAME:" . "»")))

(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (package! visual-fill-column)

(setq gac-custom-interval 0.5)

(use-package projectile
  :diminish projectile
  :config
  (projectile-global-mode)
  :bind (("<f6>" . projectile-ripgrep)
         ("C-<f6>" . projectile-replace)
         ("<f7>" . projectile-find-file)
         ("<f8>" . projectile-run-shell)
         ("<f9>" . projectile-command-map)
         :map projectile-mode-map
         ("s-d" . projectile-find-dir)
         ("s-p" . projectile-switch-project)
         ("s-f" . projectile-find-file)
         ("s-a" . projectile-ag))
  :custom
  (projectile-completion-system 'default)
  (projectile-switch-project-action 'projectile-find-file))

(use-package smartparens
  :config
  (smartparens-global-mode nil)
  (smartparens-global-strict-mode t)
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-]" . sp-select-next-thing-exchange)
   ("C-<left_bracket>" . sp-select-previous-thing)
   ("C-M-]" . sp-select-next-thing)
   ("M-F" . sp-forward-symbol)
   ("M-B" . sp-backward-symbol)))

(use-package windmove
  :init (windmove-default-keybindings 'shift))

(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

;; (use-package browse-kill-ring
;;   :config
;;   (browse-kill-ring-default-keybindings))

(use-package cider
  :bind (("C-<f5>" . cider-test-run-test))
  :config
  (setq cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t

        cider-overlays-use-font-lock t)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-display-in-current-window nil)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
  (cider-repl-tab-command #'indent-for-tab-command)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-require-ns-on-set nil)
  (nrepl-log-messages t)
  (cider-auto-test-mode t))

(use-package neil
  :custom
  (neil-inject-dep-to-project-p t))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :bind
  (("C-c l" . lsp-clojure-refactor-menu/body))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.bb" . clojure-mode)))

(use-package company
  :init (global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-show-numbers t))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/RoamNotes"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d" . org-roam-dailies-goto-today)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam.capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defhydra lsp-clojure-refactor-menu (:color blue :hint nil)
  "
Threading                      Code Manip                      Namespace                       Misc
------------------------------------------------------------------------------------------------------------------------------------------------------
_th_: Thread first             _el_: Expand let                _cn_: Clean ns                  _cp_: Cycle privacy
_tf_: Thread first all         _il_: Introduce let             _am_: Add missing libspec       _cc_: Cycle coll
_tt_: Thread last              _ml_: Move to let
_tl_: Thread last all          _ef_: Extract function
_ua_: Unwind all               _rn_: Rename
_uw_: Unwind thread            _mf_: Move formattedtextfield
"

  ("am" lsp-clojure-add-missing-libspec)
  ("cc" lsp-clojure-cycle-coll)
  ("cn" lsp-clojure-clean-ns)
  ("cp" lsp-clojure-cycle-privacy)
  ("ef" lsp-clojure-extract-function)
  ("el" lsp-clojure-expand-let)
  ("il" lsp-clojure-introduce-let)
  ("mf" lsp-clojure-move-form)
  ("ml" lsp-clojure-move-to-let)
  ("rn" lsp-rename)
  ("tf" lsp-clojure-thread-first-all)
  ("th" lsp-clojure-thread-first)
  ("tl" lsp-clojure-thread-last-all)
  ("tt" lsp-clojure-thread-last)
  ("ua" lsp-clojure-unwind-all)
  ("uw" lsp-clojure-unwind-thread))

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (java-mode . lsp)
         (json-mode . lsp)
         (elixir-mode . lsp)
         (elm-mode . lsp)
         (kotlin-mode . lsp)
         (markdown-mode . lsp)
         (python-mode . lsp)
         (scala-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (terraform-mode . lsp)
         (web-mode . lsp)
         (yaml-mode . lsp)
         (xml-mode . lsp))

  :bind (("M-?" . lsp-find-definition)
         ;; ("M-/" . lsp-find-references)
         ("M-'" . lsp-treemacs-call-hierarchy))

  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  ;; Optional: In case `clojure-lsp` is not in your PATH
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))

  :custom
  ;; turn this on to capture client/server comms before
  ;; submitting bug reports with `lsp-workspace-show-log`
  (lsp-log-io t)
  (lsp-lens-enable t)
  (lsp-signature t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-indentation t)
  (lsp-enable-folding t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay .01)
  (lsp-keymap-prefix nil))

(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(global-set-key [f1] 'delete-window)
(global-set-key [f2] 'split-window-horizontally)

(global-unset-key (kbd "C-z"))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package bhr
  :commands (bhr-view-timesheet bhr-submit-multiple))

(use-package ejc-sql
  :custom
  (clomacs-httpd-default-port 8090)
  :config
  (push 'ejc-company-backend company-backends)
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (company-mode t))))

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :bind
  (("C-<tab>" . hs-toggle-hiding)))

(use-package wakatime-mode
  :config
  (global-wakatime-mode t))

(use-package treemacs
  :custom
  (treemacs-tag-follow-mode nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode nil)
  (treemacs-indent-guide-mode t)
  (treemacs-git-commit-diff-mode nil))

(use-package rg
  :custom
  (rg-command-line-flags '("--max-columns 150" "--max-columns-preview")))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.rest" . restclient-mode))
  :config
  (add-hook 'restclient-mode-hook 'outline-minor-mode)
  (add-hook 'restclient-mode-hook
            (lambda ()
              (outline-minor-mode t)
              (local-set-key (kbd "<tab>") 'outline-toggle-children)
              (setq outline-regexp "#+"))))
