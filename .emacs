;;; Emacs config ;;;

;;; CODE:

(setq load-path (cons (expand-file-name "~/elisp") load-path))
(add-to-list 'load-path "~/.emacs.d/add-node-modules-path")

(server-start)
(global-unset-key "\C-o")
(global-set-key "\C-x5" 'split-window-horizontally)

(setq mail-self-blind t)
(setq sendmail-coding-system 'iso-2022-jp)
(setq visible-bell 1)
(setq require-final-newline t)

(global-auto-revert-mode t)
;; disable menu bar in buffers
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'git-commit)
(require 'highlight-indentation)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; maybe-new-shell -- go to existing shell, or make new shell, based on argument
(defun maybe-new-shell (arg)
  "Create new or jump to existing shell if the command has an argument."
  (interactive "p")

  ; no matter what happens, we will need to go to (after creating if necessary) the first shell buffer
  (eshell)
  (if (/= arg 1)
    (let ((new-shell-name (concat "*eshell-" (number-to-string arg) "*")))
      (if (get-buffer new-shell-name)
        (switch-to-buffer new-shell-name)
          (rename-buffer " temp-shell" nil)   ; space-prefixed buffer names are reserved for internal use
          (shell)
          (rename-buffer new-shell-name)
          (set-buffer " temp-shell")
          (rename-buffer "*eshell*")
          (set-buffer new-shell-name)
      )
    )
  )
)


(add-hook 'eshell-mode-hook
   (lambda ()
     (setenv "TERM" "emacs") ; enable colors
     ))


(defun back()
  (interactive)
  (other-window -1))

(global-set-key "\C-o\C-p" 'back)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make C-o C-s work from dired
;;
;; make empty keymap
(defvar dired-o-map nil)
(or dired-o-map (setq dired-o-map (make-keymap)))

;; register hook to change keymap
(add-hook 'dired-mode-hook
  (lambda () (define-key dired-mode-map "\C-o" dired-o-map)))

;; define favorite mappings
(define-key dired-o-map "s" 'maybe-new-shell)

(global-font-lock-mode t)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backup files
;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pcomplete for git
;; http://www.masteringemacs.org/articles/2012/01/16/pcomplete-context-sensitive-completion-emacs/
;; see also https://github.com/leoliu/pcmpl-git-el
;
(defconst pcmpl-git-commands
  '("add" "bisect" "branch" "br" "checkout" "co" "clone"
    "commit" "diff" "fetch" "grep"
    "init" "log" "merge" "mv" "pull" "push" "rebase"
    "reset" "rm" "show" "status" "tag" )
  "List of `git' commands")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE"
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let ((ref-list))
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (add-to-list 'ref-list (match-string 1)))
      ref-list)))

(defun pcomplete/git ()
  "Completion for `git'"
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  ;; complete files/dirs forever if the command is `add' or `rm'
  (cond
   ((pcomplete-match (regexp-opt '("add" "rm")) 1)
    (while (pcomplete-here (pcomplete-entries))))
   ;; provide branch completion for the commands `checkout' and 'merge'.
   ((pcomplete-match (regexp-opt '("checkout" "co" "merge")) 1)
    (pcomplete-here* (pcmpl-git-get-refs "heads")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mac osx clipboard
;; see http://allkindsofrandomstuff.blogspot.in/2009/09/sharing-mac-clipboard-with-emacs.html
;;
(defun copy-from-osx ()
(shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
(let ((process-connection-type nil))
(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
  (process-send-string proc text)
  (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell
;;
(require 'em-smart)
(setq eshell-history-size 10000)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(load "em-hist")           ; So the history vars are defined

(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

;;; ---- path manipulation

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat " ["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                            (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize " # " 'face 'default))))

(setq eshell-highlight-prompt nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(auto-insert-mode) ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs-file-templates")
(setq auto-insert-query nil)

;;;Make Copy-Paste etc work properly on ssh when using mac

(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(set-face-background 'highlight-indentation-face "lightgray")
(put 'downcase-region 'disabled nil)

; The emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024 100)) ;; 10mb

;; emacs gc default is too low
(setq gc-cons-threshold 100000000)
(setq company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;;;;;;; Web mode config for most of the web development ;;;;;;;

(defun web-mode-init-hook ()
  "Hooks for Web mode"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))


(which-key-mode)


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (prettier-rc-mode))

;;;;;; End web-mode config ;;;;;


(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'prog-mode-hook 'helm-mode)
(add-hook 'typescript-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'prettier-rc-mode)
(add-hook 'js2-mode-hook 'prettier-rc-mode)
(add-hook 'web-mode-hook 'prettier-rc-mode)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'company-mode)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

(projectile-global-mode)
(setq projectile-enable-caching t)
;; customizations for finding definitions and references in JS/TS projects
(global-set-key "\C-x\C-i" 'lsp-ui-peek-find-definitions)
(global-set-key "\C-x\C-e" 'lsp-ui-peek-find-references)
(global-set-key "\C-xf" 'projectile-find-file)
(global-set-key "\C-xh" 'flycheck-list-errors)
(setq lsp-ui-sideline-show-flycheck t)



;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)



; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
(add-hook 'lsp-mode-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'typescript-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

(add-hook 'lsp-after-apply-edits-hook (lambda (operation) (when (eq operation 'rename) (save-buffer))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-preview-mode outline-magic gitconfig use-package-hydra flycheck-tip lsp-treemacs yasnippet helm-describe-modes helm-mode-manager eslint-rc flycheck lsp-mode prettier-rc lsp-ui jsonnet-mode tree-sitter-langs eslint-fix yaml-mode which-key websocket web-server web-mode-edit-element use-package typescript-mode solarized-theme projectile prettier-js prettier monokai-theme magit json-mode highlight-indentation helm-xref helm-lsp git-timemachine fold-this dap-mode company color-theme-sanityinc-solarized add-node-modules-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
