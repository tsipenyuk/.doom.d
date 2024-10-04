;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Personal Information
(setq user-full-name "Arseniy Tsipenyuk"
      user-mail-address "arseniy.tsipenyuk@gmail.com")

;; UI Configuration
(setq doom-theme 'doom-solarized-light
      doom-font (font-spec :family "Fira Code" :size 19)
      display-line-numbers-type t)

;; Disable toolbar on startup (for macOS Sonoma)
(add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

;; Org Configuration
(setq org-directory "~/Dropbox/org/"
      org-roam-directory "~/Dropbox/org/roam/"
      org-agenda-files (list org-directory org-roam-directory))

;; Global Settings
(setq kill-whole-line t
      confirm-kill-emacs nil)
(global-subword-mode 1)
(which-key-mode)

;; Custom Key Translations
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)
(keyboard-translate ?\: ?\;)
(keyboard-translate ?\; ?\:)

;; JSON Configuration
(setq json-reformat:indent-width 2)
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2)

;; Org-related Configuration
(after! org
  (setq org-reveal-root "file:///Users/user/git/reveal.js"
        org-reveal-custom-css "section.reveal section.slide .title { display: none; }"
        org-reveal-title-slide nil
        org-reveal-toc-title "git: einige Features und Best Practices"
        org-log-done 'time))

;; Custom Keybindings
(map! :leader
      "!" #'rgrep
      "w z" #'windmove-swap-states-left
      "w Z" #'windmove-swap-states-right
      ";" #'async-shell-command
      "f t" #'prettier-js
      "f o" #'find-file-other-window
      "w a" #'other-frame
      "o s" #'sql-postgres
      "b f" #'arts/format-buffer-and-save
      "f y" #'arts/copy-file-contents
      "f p" #'arts/paste-file-contents)

(map! :leader
      (:prefix ("l" . "line")
       "f" #'arts/fill-to-end
       "l" #'avy-goto-line
       "n" #'goto-line
       "r" #'string-rectangle
       "s" #'sort-lines))

(map! :leader
      (:prefix ("m" . "misc")
       "m k" #'with-editor-cancel
       "m c" #'with-editor-finish
       "o o" #'org-open-at-point
       "o p" #'org-set-property))

(map! :leader
      (:prefix ("e" . "ejira/execute")
       "u" #'vterm-send-up
       "k" #'term-interrupt-subjob
       "r" #'ejira-update-my-projects
       "p" #'ejira-push-item-under-point
       "e" #'eglot
       "c" #'eglot-code-actions))

(map! :leader
      (:prefix ("r" . "run")
       "z d" (cmd! (async-shell-command "yarn run e2e-db"))
       "z a 1" (cmd! (async-shell-command "nvm use && ng serve --port 4201"))
       "z a 2" (cmd! (async-shell-command "nvm use && ng serve --port 4202"))
       "z e 2" (cmd! (async-shell-command "yarn run electron-4202"))
       "z p" (cmd! (async-shell-command "yarn run e2e-playwright"))))

;; Custom Functions
(defun arts/close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun arts/run-bun-test ()
  "Run 'bun test <filename>' from the folder of the current file."
  (interactive)
  (when-let ((filename (buffer-file-name)))
    (let* ((default-directory (file-name-directory filename))
           (command (concat "bun test " (file-name-nondirectory filename))))
      (shell-command command))))

(defun arts/format-buffer-and-save ()
  "Format the current buffer using prettier-js and save the buffer."
  (interactive)
  (prettier-js)
  (save-buffer))

(defun arts/format-json-buffer-and-save ()
  "Format the current JSON buffer and save it."
  (interactive)
  (json-pretty-print-buffer)
  (save-buffer))

(defun arts/get-word-at-point ()
  "Return the JavaScript word at point."
  (let ((word-chars "a-zA-Z0-9_$")
        (start (point))
        (end (point)))
    (save-excursion
      (while (and (not (bobp))
                  (string-match-p (concat "[" word-chars "]")
                                  (char-to-string (char-before))))
        (backward-char)
        (setq start (point)))
      (goto-char end)
      (while (and (not (eobp))
                  (string-match-p (concat "[" word-chars "]")
                                  (char-to-string (char-after))))
        (forward-char)
        (setq end (point))))
    (buffer-substring-no-properties start end)))

(defun arts/search-project-for-export-word-at-point ()
  "Search the project for 'export .* <word-at-point>' and open the buffer with the first match."
  (interactive)
  (if-let ((word (arts/get-word-at-point)))
      (let ((search-query (concat "export\\s+\\(type\\|interface\\|class\\|async\\s+function\\|function\\|enum\\|const\\)\\s+" word)))
        (cond
         ((modulep! :completion ivy)
          (counsel-rg search-query (projectile-project-root)))
         ((modulep! :completion helm)
          (helm-projectile-rg search-query))
         ((modulep! :completion vertico)
          (consult-ripgrep (projectile-project-root) search-query))
         (t
          (projectile-ripgrep search-query))))
    (message "No word at point.")))

(defun arts/import-relative-file-name-and-insert ()
  "Insert an import statement for the word at point from the relative path of the current file."
  (interactive)
  (let* ((current-buffer-filename (buffer-file-name))
         (other-buffer-filename (with-current-buffer (other-buffer (current-buffer) 1)
                                  (buffer-file-name)))
         (relative-path (when (and current-buffer-filename other-buffer-filename)
                          (file-relative-name
                           (file-name-sans-extension current-buffer-filename)
                           (file-name-directory other-buffer-filename))))
         (word-at-point (arts/get-word-at-point)))
    (if relative-path
        (with-current-buffer (other-buffer (current-buffer) 1)
          (save-excursion
            (goto-char (point-min))
            (insert (format "import { %s } from './%s'\n" word-at-point relative-path)))
          (message "Inserted relative import '%s' with word '%s'." relative-path word-at-point))
      (message "Could not determine the relative path."))))

(defun arts/copy-file-contents ()
  "Copy the entire contents of the current buffer to the kill ring."
  (interactive)
  (kill-new (buffer-string))
  (message "Copied entire file contents to clipboard"))

(defun arts/paste-file-contents ()
  "Clear the current buffer's contents and paste the contents from the clipboard."
  (interactive)
  (delete-region (point-min) (point-max))
  (yank)
  (message "Pasted clipboard contents into the current file"))

(defun arts/fill-to-end (char)
  "Fill to the end of the line with the given character."
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))

;; Mode-specific configurations
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))

(after! typescript-mode
  (map! :map typescript-mode-map
        :localleader
        :desc "Insert import statement" "ii" #'js/import
        :desc "Insert named import" "if" #'js/import-from))

;; JSON mode configuration
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'json-mode-hook
          (lambda ()
            (map! :leader
                  :desc "Format buffer and save"
                  "b f" #'arts/format-json-buffer-and-save)))

;; LilyPond configuration
(use-package! lilypond-mode
  :mode "\\.ly\\'"
  :config
  (setq LilyPond-pdf-command "open")
  (map! :map LilyPond-mode-map
        :localleader
        :desc "Compile" "c" #'LilyPond-command-compile
        :desc "View" "v" #'LilyPond-command-view
        :desc "Play MIDI" "p" #'LilyPond-play-midi
        :desc "Open PDF" "o" #'LilyPond-open-pdf-in-new-tab))

;; Shell script formatting
(use-package! reformatter
  :config
  (reformatter-define shfmt
    :program "shfmt"
    :args `("-i" "2" "-ci" "-bn" ,@(when buffer-file-name
                                     (list "-filename" buffer-file-name)))
    :lighter " ShFmt"))

(add-hook 'sh-mode-hook #'shfmt-on-save-mode)

;; Jenkinsfile and Groovy configuration
(use-package! jenkinsfile-mode
  :mode (("Jenkinsfile\\'" . jenkinsfile-mode)
         ("\\.jenkinsfile\\'" . jenkinsfile-mode))
  :config
  (setq jenkinsfile-mode-groovy-indent-offset 2))

(use-package! groovy-mode
  :config
  (setq groovy-indent-offset 2))

(after! format-all
  (set-formatter! 'groovy-format
    '("groovy-format" "-c" "import java.nio.file.*; def content = new String(Files.readAllBytes(Paths.get(\"%S\")), \"UTF-8\"); def formatted = groovy.ui.OutputTransforms.format(content); print formatted;")
    :modes '(groovy-mode jenkinsfile-mode)))

(set-formatter! 'jenkinsfile-formatter
  '("jenkinsfile-formatter" "--stdin")
  :modes '(groovy-mode jenkins-mode))

(add-hook! '(groovy-mode-hook jenkins-mode-hook)
  (add-hook 'before-save-hook #'format-all-buffer nil t))

;; Spell checking
(setq ispell-dictionary "en_US")

;; Key frequency tracking
(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Mail configuration
(after! mu4e
  (setq mu4e-maildir "~/.mail/Gmail"
        mu4e-drafts-folder "/[Gmail]/Drafts"
        mu4e-sent-folder   "/[Gmail]/Sent Mail"
        mu4e-trash-folder  "/[Gmail]/Trash"
        mu4e-refile-folder "/[Gmail]/All Mail"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-compose-signature "Arseniy Tsipenyuk"))

(map! :map mu4e-headers-mode-map
      "j" #'mu4e-headers-next
      "k" #'mu4e-headers-prev
      "RET" #'mu4e-headers-view-message
      "m" #'mu4e-compose-new)

(map! :map mu4e-view-mode-map
      "j" #'mu4e-view-headers-next
      "k" #'mu4e-view-headers-prev
      "q" #'mu4e~view-quit-buffer)

;; PostgreSQL configuration
(setq sql-postgres-program "/usr/local/Cellar/postgresql@11/11.18_1/bin/psql"
      sql-postgres-login-params
      '((user "postgres")
        (database "tomedo_db")
        (server "localhost")))

;; Prettier configuration
(setq prettier-js-args '("--trailing-comma" "all"
                         "--bracket-spacing" "true"
                         "--tab-width" "2"
                         "--semi" "false"
                         "--single-quote" "true"))
