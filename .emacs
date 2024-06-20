;;; Emacs config ;;;
;;; CODE:
(setq load-path (cons (expand-file-name "~/elisp") load-path))
(add-to-list 'load-path "~/.emacs.d/add-node-modules-path")
(add-to-list 'load-path "~/.emacs.d/highlight-indentation-mode")
(load "~/.emacs.d/highlight-indentation-mode.el")


(server-start)
(global-unset-key "\C-o")
(global-set-key "\C-x5" 'split-window-horizontally)


(setq mail-self-blind t)
(setq sendmail-coding-system 'iso-2022-jp)
(setq visible-bell 1)
(setq require-final-newline t)

(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
;; disable menu bar in buffers
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'git-commit)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

;(global-font-lock-mode t)

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

;;;;;; End web-mode config ;;;;;


(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'typescript-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'prettier-rc-mode)
(add-hook 'js2-mode-hook 'prettier-rc-mode)
(add-hook 'web-mode-hook 'prettier-rc-mode)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'company-mode)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)


(projectile-global-mode)

(add-to-list 'projectile-globally-ignored-directories "-/*/node_modules")
(add-to-list 'projectile-globally-ignored-directories "-/*/.git/")

(setq projectile-enable-caching t)

(require 'helm)
(helm-mode)
;; customizations for finding definitions and references in JS/TS projects
(global-set-key "\C-x\C-i" 'lsp-ui-peek-find-definitions)
(global-set-key "\C-x\C-e" 'lsp-ui-peek-find-references)
(global-set-key "\C-xf" 'projectile-find-file)
(global-set-key "\C-xh" 'flycheck-list-errors)
(global-set-key "\C-x\C-l" 'lsp-execute-code-action)
(setq lsp-ui-sideline-show-flycheck t)



;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  ;locate dominating file
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

;(add-hook 'lsp-mode-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'typescript-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'lsp-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))


(load-theme 'solarized-light t)






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "180adb18379d7720859b39124cb6a79b4225d28cef4bfcf4ae2702b199a274c8" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(package-selected-packages
   '(quelpa-use-package quelpa exec-path-from-shell solarized-theme helm-ag helm-mode-manager helm-z web-mode prettier-rc prettier yasnippet projectile lsp-ui flycheck-tip company lsp-treemacs eslint-fix eslint-rc lsp-mode flycheck which-key tree-sitter-langs outline-magic tree-sitter gitconfig git-modes transpose-frame markdown-mode prettier-js fold-this git-wip-timemachine git-time-metric typescript-mode typescript python-mode magit json-mode js2-mode highlight-indentation highlight-indent-guides highlight-chars gradle-mode go-playground go-errcheck git-timemachine)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
