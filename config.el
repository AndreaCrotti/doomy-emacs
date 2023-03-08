;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
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
(setq org-directory "~/org/")


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

(defun set-font-with-size (size)
  (set-frame-font
   (format "-CTDB-Fira Code-normal-normal-normal-*-%s-*-*-*-m-0-iso10646-1"
           size)))

(defun set-font-size (&optional _)
  (interactive)
  (let ((new-size
         (cond
          ;; external 1k monitor
          ((equal 1920 (x-display-pixel-width)) 12)
          ;; internal 4k monitor
          ((equal 3840 (x-display-pixel-width)) 28)
          ;; external 4k monitor
          ((equal 7680 (x-display-pixel-width)) 18)
          (t 24))))

    (message "changing the font size to %s" new-size)
    (set-font-with-size new-size)))

;; not seems to do exactly what needed really, not just called when moving to a different monitor
;; (add-hook 'window-size-change-functions 'set-font-size)
(set-font-size)

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

(setq forge-owned-accounts '(("AndreaCrotti")))

;; (setq-hook! org-mode
;;   prettify-symbols-alist '(("#+end_quote" . "”")
;;                            ("#+END_QUOTE" . "”")
;;                            ("#+begin_quote" . "“")
;;                            ("#+BEGIN_QUOTE" . "“")
;;                            ("#+end_src" . "«")
;;                            ("#+END_SRC" . "«")
;;                            ("#+begin_src" . "»")
;;                            ("#+BEGIN_SRC" . "»")
;;                            ("#+name:" . "»")
;;                            ("#+NAME:" . "»")))

(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (package! visual-fill-column)
