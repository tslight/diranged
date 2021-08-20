;;; diranged.el --- Dired on Acid -*- lexical-binding: t; -*-

;;; Commentary:

;; A minor mode to make dired a bit more like ranger, but just crazy, not evil.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; URL: https://gitlab.com/tslight/diranged
;; Package-Requires: ((emacs "26.1"))

;;; Code:
(require 'dired)

;;;###autoload
(defgroup diranged nil
  "Toggle preview of files when navigating in `dired'."
  :group 'dired)

;;;###autoload
(defcustom diranged-mode nil
  "Toggle variable `diranged-mode'.
Setting this variable directly does not take effect; use either
\\[customize] or the function `diranged-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'dired)

;;;###autoload
(defcustom diranged-max-file-size 10
  "Maximum size of file to view in MB."
  :group 'diranged
  :type 'integer)

;;;###autoload
(defcustom diranged-kill-on-move t
  "Kill spawned buffers as we go."
  :group 'diranged
  :type 'boolean)

;;;###autoload
(defcustom diranged-kill-on-exit t
  "Kill spawned buffers on quitting variable `diranged-mode'."
  :group 'diranged
  :type 'boolean)

;;;###autoload
(defcustom diranged-restore-windows t
  "Restore previous window layout after exiting `dired-mode-map'."
  :group 'diranged
  :type 'boolean)

;;;###autoload
(defcustom diranged-disable-on-quit t
  "Disable `diranged-mode' on `quit-window'."
  :group 'diranged
  :type 'boolean)

(defvar diranged--buffer-list (buffer-list)
  "Current buffer list, don't kill already open buffers.")

(defvar diranged--buffers nil
  "All buffers opened by `diranged'.")

(defvar diranged--window-list (window-list)
  "Current window list. Don't kill already open windows.")

(defun diranged--file-larger-than-p (filename)
  "Check if FILENAME is larger than `diranged-max-file-size'."
  (> (/ (floor
         (file-attribute-size (file-attributes filename)))
        (* 1024 1024))
     diranged-max-file-size))

(defun diranged--kill-buffers-without-window ()
  "Kill all `diranged--buffers', not displayed in any window."
  (interactive)
  (mapc (lambda (buffer)
          (unless (get-buffer-window buffer t)
            (delete buffer diranged--buffers)
            (kill-buffer-if-not-modified buffer)))
        diranged--buffers))

(defun diranged--display-directory (directory)
  "Enable `diranged-mode' on DIRECTORY and return buffer name."
  (with-current-buffer (or
                        (car (or (dired-buffers-for-dir directory) ()))
                        (dired-noselect directory))
    (diranged-mode 1)
    (run-hooks 'diranged-mode-hook)
    (current-buffer)))

(defun diranged--display-file ()
  "View current file in temporary buffer and other window."
  (let ((entry-name (dired-file-name-at-point)))
    (unless (diranged--file-larger-than-p entry-name)
      (add-to-list 'diranged--buffers
                   (window-buffer
                    (display-buffer
                     (if (file-directory-p entry-name)
                         (diranged--display-directory entry-name)
                       (or
                        (find-buffer-visiting entry-name)
                        (find-file-noselect entry-name)))
                     t)))
      (if diranged-kill-on-move
          (diranged--kill-buffers-without-window)))))

;;;###autoload
(defun diranged-beginning-of-buffer ()
  "Go to first file in directory and preview."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 2)
  (diranged--display-file))

;;;###autoload
(defun diranged-end-of-buffer ()
  "Go to last file in directory and preview."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1)
  (diranged--display-file))

;;;###autoload
(defun diranged-scroll-up ()
  "Scroll up and preview."
  (interactive)
  (scroll-up-command)
  (while (not (dired-file-name-at-point))
    (dired-previous-line 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-scroll-down ()
  "Scroll down and preview."
  (interactive)
  (scroll-down-command)
  (while (not (dired-file-name-at-point))
    (dired-next-line 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-prev-dirline (arg)
  "Move down to next ARG directory and preview."
  (interactive "p")
  (dired-prev-dirline (if (> arg 1) arg 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-next-dirline (arg)
  "Move up to next ARG directory and preview."
  (interactive "p")
  (dired-next-dirline (if (> arg 1) arg 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-next-line (arg)
  "Move down ARG lines and view file in other window."
  (interactive "p")
  (dired-next-line (if (> arg 1) arg 1))
  (if (eobp) (dired-previous-line 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-previous-line (arg)
  "Move up ARG lines and view file in other window."
  (interactive "p")
  (dired-previous-line (if (> arg 1) arg 1))
  (while (not (dired-file-name-at-point))
    (dired-next-line 1)) ;; account for 2 lines at top of dired buffer.
  (diranged--display-file))

;;;###autoload
(defun diranged-find-alternate-file ()
  "If visiting a directory, preview the new directory.
Otherwise `dired-find-file-other-window'."
  (interactive)
  (if (and (dired-file-name-at-point)
           (file-directory-p (dired-file-name-at-point)))
      (progn
        (dired-find-alternate-file)
        (diranged--display-file))
    (dired-find-file-other-window)))

;;;###autoload
(defun diranged-find-file ()
  "If visiting a directory, preview the new directory.
Otherwise `dired-find-file-other-window'."
  (interactive)
  (if (and (dired-file-name-at-point)
           (file-directory-p (dired-file-name-at-point)))
      (progn
        (dired-find-file)
        (diranged--display-file))
    (dired-find-file-other-window)))

;;;###autoload
(defun diranged-view-file ()
  "If visiting a directory, preview the new directory."
  (interactive)
  (if (and (dired-file-name-at-point)
           (file-directory-p (dired-file-name-at-point)))
      (progn
        (dired-view-file)
        (diranged--display-file))
    (view-file-other-window (dired-file-name-at-point))))

;;;###autoload
(defun diranged-up-directory ()
  "Go up a directory, but retain preview state."
  (interactive)
  (if diranged-kill-on-move
      (find-alternate-file "..")
    (dired-up-directory))
  (diranged-mode 1)
  (add-to-list 'diranged--buffers (current-buffer))
  (diranged--display-file))

;;;###autoload
(defun diranged-flag-file-deletion (arg)
  "Flag ARG number of files for deletion and then preview."
  (interactive "p")
  (dired-flag-file-deletion (if (> arg 1) arg 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-do-delete ()
  "Delete, but maintain preview."
  (interactive)
  (dired-do-delete)
  (diranged--display-file))

;;;###autoload
(defun diranged-mark (arg)
  "Mark ARG files, and maintain preview."
  (interactive "p")
  (dired-mark (if (> arg 1) arg 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-unmark (arg)
  "Unmark ARG files, and maintain preview."
  (interactive "p")
  (dired-unmark (if (> arg 1) arg 1))
  (diranged--display-file))

;;;###autoload
(defun diranged-quit-window ()
  "Kill current preview when burying `dired'."
  (interactive)
  (if diranged-disable-on-quit
      (diranged-mode -1)
    (progn
      (diranged--kill-buffers-without-window)
      (if diranged-restore-windows
          (jump-to-register :pre_diranged))))
  (quit-window))

;;;###autoload
(defvar diranged-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<SPC>") 'scroll-other-window)
    (define-key map (kbd "<backspace>") 'scroll-other-window-down)
    (define-key map [remap forward-char] 'diranged-find-alternate-file)
    (define-key map [remap backward-char] 'diranged-up-directory)
    (define-key map [remap right-char] 'diranged-find-alternate-file)
    (define-key map [remap left-char] 'diranged-up-directory)
    (define-key map [remap next-line] 'diranged-next-line)
    (define-key map [remap previous-line] 'diranged-previous-line)
    (define-key map [remap scroll-up-command] 'diranged-scroll-up)
    (define-key map [remap scroll-down-command] 'diranged-scroll-down)
    (define-key map [remap beginning-of-buffer] 'diranged-beginning-of-buffer)
    (define-key map [remap end-of-buffer] 'diranged-end-of-buffer)
    (define-key map [remap quit-window] 'diranged-quit-window)
    (define-key map [remap dired-find-alternate-file] 'diranged-find-alternate-file)
    (define-key map [remap dired-find-file] 'diranged-find-file)
    (define-key map [remap dired-view-file] 'diranged-view-file)
    (define-key map [remap dired-mark] 'diranged-mark)
    (define-key map [remap dired-unmark] 'diranged-unmark)
    (define-key map [remap dired-do-delete] 'diranged-do-delete)
    (define-key map [remap dired-flag-file-deletion] 'diranged-flag-file-deletion)
    (define-key map [remap dired-up-directory] 'diranged-up-directory)
    (define-key map [remap dired-next-line] 'diranged-next-line)
    (define-key map [remap dired-previous-line] 'diranged-previous-line)
    (define-key map [remap dired-next-dirline] 'diranged-next-dirline)
    (define-key map [remap dired-prev-dirline] 'diranged-prev-dirline)
    map)
  "Keymap `dired' functions to more `diranged' equivalents.")

(defun diranged--enable ()
  "Dirange all the `dired' things."
  (when diranged-restore-windows
    (window-configuration-to-register :pre_diranged)
    (delete-other-windows))
  (diranged--display-file))

(defun diranged--disable ()
  "Restore `dired' to sanity."
  (let ((current-point (point)))
    (if diranged-restore-windows
        (jump-to-register :pre_diranged))
    (if diranged-kill-on-exit
        (diranged--kill-buffers-without-window))
    (setq diranged--buffers nil)
    (goto-char current-point)))

;;;###autoload
(define-minor-mode diranged-mode
  "Toggle preview of files when navigating in `dired'.
Like `ranger-mode', but just crazy, not evil."
  :init-value nil
  :group 'diranged
  :keymap diranged-mode-map
  :lighter " diranged"
  (if (derived-mode-p 'dired-mode)
      (if diranged-mode
          (diranged--enable)
        (diranged--disable))
    (progn
      (setq diranged-mode nil)
      (error "Only `dired-mode' can be diranged"))))

(provide 'diranged)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; diranged.el ends here
