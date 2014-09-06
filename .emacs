(setq load-path (cons (expand-file-name "~/elisp") load-path))
(add-to-list 'load-path "~/.emacs.d/elpa/editorconfig-0.3/")
(add-to-list 'load-path "~/.emacs.d/elpa/js2-mode-20140114/")
(add-to-list 'load-path "~/.emacs.d/elpa/handlebars-mode-1.3/")
(add-to-list 'load-path "~/.emacs.d/elpa/magit-1.2.1/")
(add-to-list 'load-path "~/.emacs.d/highlight-indent")

(server-start)

(global-unset-key "\C-o")
(global-set-key "\C-x5" 'split-window-horizontally)
(setq mail-self-blind t)
(setq sendmail-coding-system 'iso-2022-jp)
(setq visible-bell 1)
(setq require-final-newline t)
(global-auto-revert-mode t)
(setq ruby-deep-indent-paren nil)

(load "editorconfig")

(require 'magit)
(require 'highlight-indentation)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)

(require 'handlebars-mode)

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

(add-hook 'eshell-preoutput-filter-functions
;;  'ansi-color-filter-apply) ;; this doesn't work, but the next line does!!
  'ansi-color-apply)

(add-hook 'eshell-mode-hook (lambda ()(setenv "TERM" "emacs"))) ;display colors when git commands are used in eshell.

;; Bob's customizations!
;;
(global-set-key "\C-o\C-s" 'maybe-new-shell)
(global-set-key "\C-o\C-l" 'goto-line)
(global-set-key "\C-o\C-r" 'revert-buffer)
(global-set-key "\C-o\C-c" 'compile)
(global-set-key "\C-o\C-g" 'magit-status)
(global-set-key "\C-o\C-v" 'find-file-at-point)
(global-set-key "\C-o\C-f" 'find-name-dired)
(global-set-key "\C-o\C-z" 'zap-to-char)
(global-set-key "\C-o\C-e" 'js2-next-error)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; python mode
;
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
  
(autoload 'python-mode "python-mode" "Python editing mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css mode
;;
;; (autoload 'css-mode "css-mode")
;; (setq auto-mode-alist       
;;      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
 (setq auto-mode-alist       
      (cons '("\\.scss\\'" . css-mode) auto-mode-alist))

(autoload 'sgml-mode "sgml-mode")
(setq auto-mode-alist       
     (cons '("\\.html\\'" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist       
     (cons '("\\.handlebars\\'" . handlebars-mode) auto-mode-alist))

(add-hook 'sgml-mode-hook
	  (lambda ()
	    ;; Default indentation to 2, but let SGML mode guess, too.
	    (set (make-local-variable 'sgml-basic-offset) 2)
	    (sgml-guess-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XSL mode
;;
(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; Turn on font lock when in XSL mode
(add-hook 'xsl-mode-hook
	  'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
	'("\\.xsl" . xsl-mode))
       auto-mode-alist))

;; Uncomment if using abbreviations
;; (abbrev-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript mode
;; see www.hesketh.com/...
;;
;; (autoload 'javascript-mode
;;      "javascript-mode" "Javascript mode" t)

;;    (setq auto-mode-alist
;;          (append '(("\\.js$" . javascript-mode))
;;                   auto-mode-alist))

;; (autoload 'js2-mode
;;   "js2-mode" "Javascript mode" t)

;; (setq auto-mode-alist
;;       (append '(("\\.js$" . js2-mode))
;; 	      auto-mode-alist))

(add-hook 'js-mode-hook 'highlight-indentation-mode)

(defun ldd-js2-parse-jshintrc ()
  "This looks recursively up for a .jshintrc and extracts the
globals from it to add them to js2-additional-externs."
  (let* ((jshintrc (find-file-recursively-up "^\\.jshintrc$"))
         (json (and jshintrc
                    (json-read-file (car jshintrc))))
         (globals (and json
                       (cdr (assq 'globals json))))
        )
    (when globals
      (setq js2-additional-externs
            (append
             (mapcar (lambda (pair)
                         (symbol-name (car pair))
                     )
                     globals
             )
             js2-additional-externs
            )
      )
      (js2-reparse t)
    )
  )
)
(add-hook 'js2-init-hook 'ldd-js2-parse-jshintrc)
;;(js2-imenu-extras-mode)
;;(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;;cursor movemtn takes into account camelcasing bullshit
(add-hook 'js2-mode-hook 'subword-mode)

;; make fooBar look like foo_Bar
(add-hook 'js2-mode-hook 'glasses-mode)

;; match parens
(add-hook 'js2-mode-hook 'show-paren-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;(add-to-list 'load-path
;;              "~/elisp/yasnippet")
;;(require 'yasnippet)
;;(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse error messages from 4xslt
;;
(load "compile")
(setq compilation-error-regexp-alist
  (append '(("Source document (\\(.+\\)):.+line \\([0-9]+\\).+column \\([0-9]+\\)" 1 2 3))
	  compilation-error-regexp-alist))
(setq compilation-error-regexp-alist
  (append '(("Stylesheet (\\(.+\\)):.+line \\([0-9]+\\).+column \\([0-9]+\\)" 1 2 3))
	  compilation-error-regexp-alist))
(setq compilation-error-regexp-alist
  (append '(("Malformed expression: \\(.+\\),.+line \\([0-9]+\\).+column \\([0-9]+\\)" 1 2 3))
	  compilation-error-regexp-alist))

;;(require 'google)
;;(setq google-license-key "VKZC//VQFHJsaQNdHdf+gdYKRs0XyVG8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backup files
;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; Put autosave files (ie #foo#) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(css-indent-offset 2)
 '(js2-basic-offset 2)
 '(js2-global-externs (quote ("Kaybus" "Ember" "moment" "Em" "DS"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; suppress creation of dot-hash files
;; (fmakunbound 'lock-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto completion inside meacs shell
;;
;;(autoload 'bash-completion-dynamic-complete 
;;  "bash-completion"
;;  "BASH completion hook")
;;(add-hook 'shell-dynamic-complete-functions
;;  'bash-completion-dynamic-complete)
;;(add-hook 'shell-command-complete-functions
;;  'bash-completion-dynamic-complete)

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
;; mail
;;
(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 user-mail-address "bob.myers@kaybus.com"
 user-full-name "Bob Myers"
 smtpmail-starttls-credentials '(("corpmail.kaybus.com" 25 nil nil))
 smtpmail-auth-credentials  (expand-file-name "~/.authinfo")
 smtpmail-default-smtp-server "corpmail.kaybus.com"
 smtpmail-smtp-server "corpmail.kaybus.com"
 smtpmail-smtp-service 25
 smtpmail-debug-info t
 starttls-extra-arguments nil
 starttls-gnutls-program (executable-find "gnutls-cli")
 smtpmail-warn-about-unknown-extensions t
 starttls-use-gnutls t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
;;(eshell-smart-initialize)

(setq eshell-history-size 1024)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctags
;; (defun build-ctags ()
;;   (interactive)
;;   (message "building project tags")
;;   (let ((root "/Users/rtm/kaybus/kaybus/"))
;;     (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
;;   (visit-project-tags)
;;   (message "tags built successfully"))

;; (defun visit-project-tags ()
;;   (interactive)
;;   (let ((tags-file (concat "/Users/rtm/kaybus/kaybus/" "TAGS")))
;;     (visit-tags-table tags-file)
;;     (message (concat "Loaded " tags-file))))

;; (defun my-find-tag ()
;;   (interactive)
;;   (if (file-exists-p (concat "/Users/rtm/kaybus/kaybus/" "TAGS"))
;;       (visit-project-tags)
;;     (build-ctags))
;;   (etags-select-find-tag-at-point))

;; (global-set-key (kbd "M-.") 'my-find-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tern
;;(add-to-list 'load-path "/usr/local/share/tern/emacs/")
;;(autoload 'tern-mode "tern.el" nil t)
;;
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; marmalade
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; po-mode
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)
