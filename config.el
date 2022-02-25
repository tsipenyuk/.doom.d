;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Arseniy Tsipenyuk"
      user-mail-address "arseniy.tsipenyuk@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)
(setq doom-font (font-spec :family "Fira Code" :size 19))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/personal/gdrive/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq kill-whole-line t)
(global-subword-mode 1)

(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

(after! org
        (setq org-roam-directory "~/personal/gdrive/org/roam/")
        (setq org-roam-index-file "~/personal/gdrive/org/roam/index.org"))

(setq json-reformat:indent-width 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)


(map! :leader
      :desc "recursive grep"
      "!" #'rgrep)

(map! :leader
      :desc "swap buffers left"
      "w z" #'windmove-swap-states-left
      "w Z" #'windmove-swap-states-right)

(map! :leader
      :desc "async shell"
      ";" #'async-shell-command)

(map! :leader
      :desc "apply prettier to file"
      "f t" #'prettier-js)

(map! :leader
      :desc "Jump to a line start in the current buffer"
      "l l" #'avy-goto-line)

(map! :leader
      :desc "recursive grep"
      "l n" #'goto-line)

(map! :leader
      :desc "sort lines"
      "l s" #'sort-lines)

(map! :leader
      :desc "fill to end"
      "l f" #'fill-to-end)



;;(setq-hook! 'ng2-html-mode-hook +format-with 'prettier)
(setq prettier-js-args '(
                         "--trailing-comma" "all"
                         "--bracket-spacing" "true"
                         "--tab-width" "2"
                         "--semi" "false"
                         "--single-quote" "true"
                         ))

(defun fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))

(defun fill-short (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 72)
      (insert-char char))))

;; Copy the current buffer file name
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
