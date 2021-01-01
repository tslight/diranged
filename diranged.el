;;; diranged.el --- Dired on Acid -*- lexical-binding: t; -*-

;;; Commentary:

;; A minor mode to make dired a bit more like ranger, but just crazy, not evil.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; URL: https://gitlab.com/tspub/lisp/diranged
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
  :group 'dired
  :require 'dired)

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
(defcustom diranged-steal-all-the-keys t
  "Let diranged have it's wicked way with `dired-mode-map'."
  :group 'diranged
  :type 'boolean)

;;;###autoload
(defcustom diranged-restore-windows t
  "Restore previous window layout after exiting `dired-mode-map'."
  :group 'diranged
  :type 'boolean)

(defvar diranged--buffer-list (buffer-list)
  "Current buffer list, don't kill already open buffers.")

(defvar diranged--buffers nil
  "All buffers opened by `diranged'.")

(defvar diranged--window-list (window-list)
  "Current window list. Don't kill already open windows.")

(defvar diranged--preview-window nil
  "Window of the preview.")

(defun diranged--file-larger-than-p (filename)
  "Check if FILENAME is larger than `diranged-max-file-size'."
  (> (/ (floor
         (file-attribute-size (file-attributes filename)))
        (* 1024 1024))
     diranged-max-file-size))

(defun diranged--killing-spree (&optional buffers)
  "Mercilessly murder BUFFERS created by `diranged'.
If KILL-WINDOW is true also delete the preview window."
  (mapc (lambda (buffer)
          (unless (or (member buffer diranged--buffer-list)
                      ;; never delete current buffer!
                      (eq (current-buffer) buffer))
            (kill-buffer-if-not-modified buffer)))
        (if buffers buffers diranged--buffers)))

(defun diranged--display-file()
  "View current file in temporary buffer and other window."
  (interactive)
  (if (dired-file-name-at-point)
      (unless (diranged--file-larger-than-p (dired-get-filename))
        (add-to-list 'diranged--buffers (window-buffer (dired-display-file)))
        (setq diranged--preview-window (get-buffer-window
                                        (car diranged--buffers)))
        ;; if first 2 elements are the same we're probably banging up against
        ;; the top or bottom of the file list.
        (unless (eq (car diranged--buffers) (cadr diranged--buffers))
          (if (and diranged-kill-on-move (cdr diranged--buffers))
              (diranged--killing-spree (cdr diranged--buffers)))))))

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
  (if (and (dired-file-name-at-point) (file-directory-p (dired-get-filename)))
      (progn (dired-find-alternate-file) (diranged--display-file))
    (dired-find-file-other-window)))

;;;###autoload
(defun diranged-find-file ()
  "If visiting a directory, preview the new directory.
Otherwise `dired-find-file-other-window'."
  (interactive)
  (if (and (dired-file-name-at-point) (file-directory-p (dired-get-filename)))
      (progn (dired-find-file) (diranged--display-file))
    (dired-find-file-other-window)))

;;;###autoload
(defun diranged-view-file ()
  "If visiting a directory, preview the new directory."
  (interactive)
  (if (and (dired-file-name-at-point) (file-directory-p (dired-get-filename)))
      (progn (dired-view-file) (diranged--display-file))
    (view-file-other-window (dired-get-filename))))

;;;###autoload
(defun diranged-up-directory ()
  "Go up a directory, but retain preview state."
  (interactive)
  (if diranged-kill-on-move
      (find-alternate-file "..")
    (dired-up-directory))
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
  (when (derived-mode-p 'dired-mode)
    (diranged--killing-spree)
    (if diranged-restore-windows
        (jump-to-register :pre_diranged))
    (quit-window)))

(defun diranged--remap-all ()
  "Remap all motion keys in `dired-mode' to be more `diranged'."
  (define-key dired-mode-map (kbd "<SPC>") 'scroll-other-window)
  (define-key dired-mode-map (kbd "<backspace>") 'scroll-other-window-down)
  (define-key dired-mode-map [remap forward-char] 'diranged-find-alternate-file)
  (define-key dired-mode-map [remap backward-char] 'diranged-up-directory)
  (define-key dired-mode-map [remap right-char] 'diranged-find-alternate-file)
  (define-key dired-mode-map [remap left-char] 'diranged-up-directory)
  (define-key dired-mode-map [remap next-line] 'diranged-next-line)
  (define-key dired-mode-map [remap previous-line] 'diranged-previous-line)
  (define-key dired-mode-map [remap scroll-up-command] 'diranged-scroll-up)
  (define-key dired-mode-map [remap scroll-down-command] 'diranged-scroll-down)
  (define-key dired-mode-map [remap beginning-of-buffer] 'diranged-beginning-of-buffer)
  (define-key dired-mode-map [remap end-of-buffer] 'diranged-end-of-buffer)
  (define-key dired-mode-map [remap quit-window] 'diranged-quit-window)
  )

(defun diranged--restore-dired-mode-map ()
  "Restore original state of `dired-mode-map'."
  (define-key dired-mode-map (kbd "<SPC>") 'next-line)
  (define-key dired-mode-map (kbd "<backspace>") 'previous-line)
  (define-key dired-mode-map [remap forward-char] 'forward-char)
  (define-key dired-mode-map [remap backward-char] 'backward-char)
  (define-key dired-mode-map [remap right-char] 'right-char)
  (define-key dired-mode-map [remap left-char] 'left-char)
  (define-key dired-mode-map [remap next-line] 'next-line)
  (define-key dired-mode-map [remap previous-line] 'previous-line)
  (define-key dired-mode-map [remap scroll-up-command] 'scroll-up-command)
  (define-key dired-mode-map [remap scroll-down-command] 'scroll-down-command)
  (define-key dired-mode-map [remap beginning-of-buffer] 'beginning-of-buffer)
  (define-key dired-mode-map [remap end-of-buffer] 'end-of-buffer))

;;;###autoload
(defvar diranged-mode-map
  (let ((map (make-sparse-keymap)))
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
  (if diranged-steal-all-the-keys (diranged--remap-all))
  (diranged--display-file))

(defun diranged--disable ()
  "Restore `dired' to sanity."
  (if diranged-restore-windows (jump-to-register :pre_diranged))
  (if diranged-kill-on-exit (diranged--killing-spree))
  (if diranged-steal-all-the-keys (diranged--restore-dired-mode-map)))

(defun diranged--find-dired-buffer ()
  "Return the next available `dired' buffer."
  (car (delete nil (mapcar (lambda (buffer)
                             (with-current-buffer buffer
                               (if (equal major-mode 'dired-mode)
                                   buffer)))
                           (buffer-list)))))

;;;###autoload
(define-minor-mode diranged-mode
  "Toggle preview of files when navigating in `dired'.
Like `ranger-mode', but just crazy, not evil."
  :global t
  :group 'diranged
  :keymap diranged-mode-map
  :lighter " diranged!"
  :require 'dired
  ;; If we are not in a `dired' buffer, switch to one or create one.
  (unless (derived-mode-p 'dired-mode)
    (message "Are you diranged? Not yet...")
    (let ((dired-buffer (diranged--find-dired-buffer)))
      (if dired-buffer
          (switch-to-buffer dired-buffer)
        (dired "."))))
  (if diranged-mode (diranged--enable) (diranged--disable)))

(provide 'diranged)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; diranged.el ends here
