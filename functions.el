;;; functions.el -*- lexical-binding: t; -*-

(provide 'functions)

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open) {:theme :portal.colors/solarized-dark})) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(defun ca-next-defun ()
  (interactive)
  (end-of-defun 2)
  (beginning-of-defun 1))

(defun ca-prev-defun ()
  (interactive)
  (beginning-of-defun))

(defun vpn-up ()
  (interactive)
  (async-shell-command "sudo systemctl start wg-quick@vpn"))

(defun vpn-down ()
  (interactive)
  (async-shell-command "sudo systemctl stop wg-quick@vpn"))

(defun set-font-with-size (size)
  (interactive)
  (set-frame-font
   (format "-CTDB-Fira Code-normal-normal-normal-*-%s-*-*-*-m-0-iso10646-1"
           size)))

(when (display-graphic-p)
  (defun set-font-size (&optional _)
    (interactive)
    (let ((new-size
           (cond
            ;; external 1k monitor
            ((equal 1920 (x-display-pixel-width)) 12)
            ;; internal 4k monitor
            ((equal 3840 (x-display-pixel-width)) 28)
            ;; external 4k monitor
            ((>= (x-display-pixel-width) 7680) 18)
            (t 24))))

      (message "changing the font size to %s" new-size)
      (set-font-with-size new-size)))

  (set-font-size))
