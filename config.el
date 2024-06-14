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
(setq org-directory "~/Dropbox/org/")

;; disable tool-bar
;; https://discourse.doomemacs.org/t/how-to-have-tool-bar-mode-0-apply-at-startup-to-avoid-large-title-bar-on-macos-sonoma-when-using-railwaycat-homebrew-emacsmacport/4222
(add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

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
(which-key-mode)

(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)
;; (keyboard-translate ?\" ?\')
;; (keyboard-translate ?\' ?\")
(keyboard-translate ?\: ?\;)
(keyboard-translate ?\; ?\:)

(after! org
  (setq org-reveal-root "file:///Users/user/git/reveal.js")
  (setq org-reveal-custom-css
        "section.reveal section.slide .title { display: none; }")
  (setq org-reveal-title-slide nil)
  (setq org-reveal-toc-title "git: einige Features und Best Practices")
  (setq org-roam-directory "~/Dropbox/org/roam/")
  (setq frg-roam-index-file "~/Dropbox/org/roam/index.org"))

(setq json-reformat:indent-width 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)

(setq org-log-done 'time)

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
      :desc "Line navigation and manipulation"
      "l f" #'fill-to-end
      "l l" #'avy-goto-line
      "l n" #'goto-line
      "l r" #'string-rectangle
      "l s" #'sort-lines)

(map! :leader
      :desc "magit & org"
      "m m k" #'with-editor-cancel
      "m m c" #'with-editor-finish
      "m o o" #'org-open-at-point
      "m o p" #'org-set-property)

(map! :leader
      :desc "file management"
      ")" #'import-relative-file-name-and-insert
      "+" #'search-project-for-export-word-at-point
      "c p r" #'copy-relative-file-name-to-clipboard
      "c p a" #'copy-file-name-to-clipboard)


(map! :leader
      :desc "ejira / execute"
      "e u" #'vterm-send-up
      "e k" #'term-interrupt-subjob
      "e r" #'ejira-update-my-projects
      "e p" #'ejira-push-item-under-point
      "e e" #'eglot
      "e c" #'eglot-code-actions
      )

(map! :leader
      "f o" #'find-file-other-window)

(map! :leader
      "w a" #'other-frame)

(map! :leader
      :desc "Run e2e-db"
      "r z d" #'(lambda () (interactive) (async-shell-command "yarn run e2e-db"))
      :desc "Run ng serve --port 4201"
      "r z a 1" #'(lambda () (interactive) (async-shell-command "nvm use && ng serve --port 4201"))
      :desc "Run ng serve --port 4202"
      "r z a 2" #'(lambda () (interactive) (async-shell-command "nvm use && ng serve --port 4202"))
      :desc "Run electron-4202"
      "r z e 2" #'(lambda () (interactive) (async-shell-command "yarn run electron-4202"))
      :desc "Run e2e-playwright"
      "r z p" #'(lambda () (interactive) (async-shell-command "yarn run e2e-playwright"))
      )

(map! :leader
      "o s" #'sql-postgres)

(map! :leader
      :desc "toggle / tide"
      (:n "t r" nil) ; Unbind "t r f"
      "t j t" #'tide-jsdoc-template
      "t m r" #'read-only-mode
      "t o i" #'tide-organize-imports
      "t r f" #'tide-rename-file
      "t r s" #'tide-rename-symbol
      "t s s" #'tide-restart-server)

;; mail ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! mu4e
  (setq mu4e-maildir "~/.mail/Gmail")
  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval 300)
  (setq mu4e-compose-signature "Arseniy Tsipenyuk"))

(map!
 :map mu4e-headers-mode-map
 "j" 'mu4e-headers-next
 "k" 'mu4e-headers-prev
 "RET" 'mu4e-headers-view-message
 "m" 'mu4e-compose-new)

(map!
 :map mu4e-view-mode-map
 "j" 'mu4e-view-headers-next
 "k" 'mu4e-view-headers-prev
 "q" 'mu4e~view-quit-buffer)

;; mail end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(setq-hook! 'ng2-html-mode-hook +format-with 'prettier)
(remove-hook 'after-init-hook 'savehist-mode)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
;; Remove hooks for automatic formatting on save
(remove-hook 'js2-mode-hook 'prettier-js-mode)
(remove-hook 'web-mode-hook 'prettier-js-mode)

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


(defun copy-relative-file-name-to-clipboard ()
  "Copy the current buffer file name as a relative path to the file open in the other buffer to the clipboard, prepended with './'."
  (interactive)
  (let* ((current-buffer-filename (if (equal major-mode 'dired-mode)
                                      default-directory
                                    (buffer-file-name)))
         (other-buffer-filename (with-current-buffer (other-buffer (current-buffer) 1)
                                  (if (equal major-mode 'dired-mode)
                                      default-directory
                                    (buffer-file-name))))
         (relative-path (when (and current-buffer-filename other-buffer-filename)
                          (concat "./" (file-relative-name current-buffer-filename (file-name-directory other-buffer-filename))))))
    (if relative-path
        (progn
          (kill-new relative-path)
          (message "Copied relative file name '%s' to the clipboard." relative-path))
      (message "Could not determine the relative path."))))

;; (defun import-relative-file-name-and-insert ()
;;   "Inserts the current buffer file name as a relative path to the file open in the other buffer, prepended with 'import {  } from ' and appended with '.' at point."
;;   (interactive)
;;   (let* ((current-buffer-filename (if (equal major-mode 'dired-mode)
;;                                       default-directory
;;                                     (buffer-file-name)))
;;          (other-buffer-filename (with-current-buffer (other-buffer (current-buffer) 1)
;;                                   (if (equal major-mode 'dired-mode)
;;                                       default-directory
;;                                     (buffer-file-name))))
;;          (relative-path (when (and current-buffer-filename other-buffer-filename)
;;                           (let ((raw-relative-path (file-relative-name current-buffer-filename (file-name-directory other-buffer-filename))))
;;                             (if (string-suffix-p ".ts" raw-relative-path)
;;                                 (substring raw-relative-path 0 -3) ; Strip '.ts'
;;                               raw-relative-path)))))
;;     (if relative-path
;;         (progn
;;           (with-current-buffer (other-buffer (current-buffer) 1)
;;             (goto-char (point-min))
;;             (insert (concat "import {  } from './" relative-path "'"))
;;             (newline))
;;           (message "Inserted relative import '%s'." relative-path))
;;       (message "Could not determine the relative path."))))


;; org-jira config
;; (edit-server-start)

(use-package! emacs-everywhere
  :init
  (setq emacs-everywhere-major-mode 'org-mode) ; Set your preferred major mode
  :config
  (add-hook 'emacs-everywhere-init-hooks 'evil-emacs-state)) ; Optional: Use Evil keybindings


(after! org
  (setq roam-org-directory "~/Dropbox/org/roam")
  (add-to-list 'org-agenda-files roam-org-directory))

;; setting postgres
(setq sql-postgres-program "/usr/local/Cellar/postgresql@11/11.18_1/bin/psql")
(setq sql-postgres-login-params
      '((user "postgres")
        (database "tomedo_db")
        (server "localhost")))

;; prog lang config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp performance
;; (setq gc-cons-threshold 100000000)
;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
;; (setq lsp-idle-delay 0.500)

;; Enable LSP for TypeScript and HTML
;; (after! lsp
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("typescript-language-server" "--stdio"))
;;                     :major-modes '(typescript-mode web-mode) ;; Add other modes as needed
;;                     :server-id 'tsserver)))

;; Enable LSP
(add-hook 'typescript-mode-hook #'lsp)
;; (add-hook 'web-mode-hook #'lsp)


;; typescript config
(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))

(after! typescript-mode
  (map! :map typescript-mode-map
        :localleader
        :desc "Insert import statement" "ii" #'js/import
        :desc "Insert named import" "if" #'js/import-from))

(after! typescript-mode
  (add-hook 'typescript-mode-hook (lambda ()
                                    ;; (set (make-local-variable 'rebox-style-loop) '(25 17 21))
                                    ;; (set (make-local-variable 'rebox-min-fill-column) 40)
                                    (set (make-local-variable 'rebox-comment-style) 'jsdoc)
                                    (rebox-mode 1))))


;; CUSTOM FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close all buffers
(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; typescript config
(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))

(after! typescript-mode
  (map! :map typescript-mode-map
        :localleader
        :desc "Insert import statement" "ii" #'js/import
        :desc "Insert named import" "if" #'js/import-from))


;; CUSTOM FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close all buffers
(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; typescript config
(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))

(after! typescript-mode
  (map! :map typescript-mode-map
        :localleader
        :desc "Insert import statement" "ii" #'js/import
        :desc "Insert named import" "if" #'js/import-from))


;; CUSTOM FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close all buffers
(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; lilypond
;; (use-package! lilypond
;;   :load-path "~/.config/doom/local/lilypond/lilypond-mode"
;;   :defer t
;;   :mode ("\\.ly$" . LilyPond-mode)
;;   :config
;;   (setq lilypond-program "/usr/local/bin/lilypond"))

;; (load! "local/lilypond/lilypond-mode")

;; (defun my/lilypond-compile-and-preview ()
;;  "Compile and preview the first .ly file found in the current folder."
;;  (interactive)
;;  (let* ((ly-files (directory-files default-directory t ".*\\.ly$"))
;;         (ly-file (car ly-files))
;;         (pdf-file (concat (file-name-sans-extension ly-file) ".pdf")))
;;    (if (not ly-file)
;;        (message "No .ly files found in the current folder.")
;;      (save-buffer)
;;      (shell-command (format "lilypond %s" ly-file))
;;      (+evil/window-move-right)
;;      (find-file pdf-file)
;;      (+evil/window-move-left)
;;      (kill-current-buffer)
;;      )))

(map! :leader
      (:prefix "l"
       :desc "LilyPond Compile and Preview" "c" #'my/lilypond-compile-and-preview))


(defun arts/run-bun-test ()
  "Run 'bun test <filename>' from the folder of the current file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (let* ((default-directory (file-name-directory filename))
             (command (concat "bun test " (file-name-nondirectory filename))))
        (shell-command command)))))

(map! :leader
      (:prefix "j"
       :desc "js/ts" "t" #'arts/run-bun-test))

(defun arts/format-buffer-and-save ()
  "Format the current buffer using +format/buffer and save the buffer."
  (interactive)
  (prettier-js)
  (save-buffer))

(map! :leader
      :desc "Format buffer and save"
      "b f" #'arts/format-buffer-and-save)

;; json mode
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(defun arts/format-json-buffer-and-save ()
  "Format the current buffer using +format/buffer and save the buffer."
  (interactive)
  (json-pretty-print-buffer)
  (save-buffer))

;; Bind the function to a key only in json-mode
(add-hook 'json-mode-hook
          (lambda ()
            (map! :leader
                  :desc "Format buffer and save"
                  "b f" #'arts/format-json-buffer-and-save)))


;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; ;; if you use typescript-mode
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;; ;; if you use treesitter based typescript-ts-mode (emacs 29+)
;; (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

;; do not ask for confirmation when quitting
(setq confirm-kill-emacs nil)

;; ts-import

(defun get-word-at-point ()
  "Return the JavaScript word at point."
  (let ((word-chars "a-zA-Z0-9_$")
        (start (point))
        (end (point)))
    ;; Move the start position to the beginning of the word
    (while (and (not (bobp))
                (string-match-p (concat "[" word-chars "]")
                                (char-to-string (char-before))))
      (backward-char)
      (setq start (point)))
    ;; Move the end position to the end of the word
    (goto-char end)
    (while (and (not (eobp))
                (string-match-p (concat "[" word-chars "]")
                                (char-to-string (char-after))))
      (forward-char)
      (setq end (point)))
    ;; Return the word at point
    (buffer-substring-no-properties start end)))

(defun search-project-with-query (query)
  "Search the project for the given QUERY."
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory (projectile-project-root)))
    (cond
     ((modulep! :completion ivy)
      (counsel-rg query (projectile-project-root)))
     ((modulep! :completion helm)
      (helm-projectile-rg query))
     ((modulep! :completion vertico)
      (consult-ripgrep (projectile-project-root) query))
     (t
      (projectile-ripgrep query)))))

(defun search-project-for-export-word-at-point ()
  "Search the project for 'export .* <word-at-point>' and open the buffer with the first match."
  (interactive)
  (let ((word (get-word-at-point)))
    (if (not (string= word ""))
        (let ((search-query (concat "export\\s+\\(type\\|interface\\|class\\|function\\|const\\)\\s+" word)))
          (search-project-with-query search-query))
      (message "No word at point."))))

(defun import-relative-file-name-and-insert ()
  "Inserts the current buffer file name as a relative path to the file open in the other buffer, prepended with 'import { <word-at-point> } from ' and appended with '.' at point."
  (interactive)
  (let* ((current-buffer-filename (if (equal major-mode 'dired-mode)
                                      default-directory
                                    (buffer-file-name)))
         (other-buffer-filename (with-current-buffer (other-buffer (current-buffer) 1)
                                  (if (equal major-mode 'dired-mode)
                                      default-directory
                                    (buffer-file-name))))
         (relative-path (when (and current-buffer-filename other-buffer-filename)
                          (let ((raw-relative-path (file-relative-name current-buffer-filename (file-name-directory other-buffer-filename))))
                            (if (string-suffix-p ".ts" raw-relative-path)
                                (substring raw-relative-path 0 -3) ; Strip '.ts'
                              raw-relative-path))))
         (word-at-point (get-word-at-point)))
    (if relative-path
        (progn
          (with-current-buffer (other-buffer (current-buffer) 1)
            (goto-char (point-min))
            (insert (concat "import { " word-at-point " } from './" relative-path "'"))
            (newline))
          (other-buffer (current-buffer))
          (message "Inserted relative import '%s' with word '%s'." relative-path word-at-point))
      (message "Could not determine the relative path."))))

(defun copy-word-at-point ()
  "Copy the JavaScript word at point."
  (interactive)
  (let ((word (get-word-at-point)))
    (kill-new word)
    (message "Copied: %s" word)))



;; lilypond
;; (setq load-path (append (list (expand-file-name "~/.emacs.d/.local/straight/repos/lilypond/elisp")) load-path))
(load (expand-file-name "~/.emacs.d/.local/straight/repos/lilypond/elisp/lilypond-mode"))
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
