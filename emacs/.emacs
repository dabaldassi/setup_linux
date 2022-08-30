
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 25)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(cmake-ide-cmake-args nil)
 '(column-number-mode 1)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(flycheck-c/c++-clang-executable "/usr/bin/clang-nocolor")
 '(flycheck-clang-analyzer-executable "/usr/bin/clang-nocolor")
 '(global-linum-mode t)
 '(helm-completion-style (quote helm))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (go-mode web-completion-data yaml-mode lsp-ui lsp-mode cargo cargo-mode glsl-mode gn-mode company-irony-c-headers flycheck-irony irony-eldoc srefactor xcscope company-tern tern-auto-complete tern js2-mode cpputils-cmake markdown-mode qml-mode company-ansible nyan-mode helm dockerfile-mode docker cmake-mode company-math cmake-ide company-qml company)))
 '(safe-local-variable-values (quote ((setq c-default-style "linux"))))
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq make-backup-files nil)

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '( "~/.emacs.d/snippets/yasnippet-snippets/snippets"
	))
(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
(defalias 'yas/table-hash 'yas--table-hash)
(yas-reload-all)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'company)
(require 'company-c-headers)
;; (require 'company-irony)
;; (require 'company-irony-c-headers)
;; (require 'flycheck-irony)
(global-company-mode)

(defun setup_irony ()
  "Irony mode configuration."
  (add-hook 'irony-mode-hook 'irony-eldoc)
  ;; (add-to-list 'company-backends 'company-irony)
  ;; (add-to-list 'company-backends 'company-irony-c-headers)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
  (when (or (eq major-mode 'c-mode)	; Prevent from being loaded by c derived mode
	    (eq major-mode 'c++-mode))
    (irony-mode 1)))

(defun cc-base ()
  "Common configuration for c and c++ mode."
  ;; Company mode
  ;; (setf company-backends '())
  ;; (add-to-list 'company-backends 'company-keywords)
  (setup_irony))

(add-hook 'c++-mode-hook 'cc-base)

(setq company-backends (delete 'company-semantic company-backends))
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)

(require 'cc-mode)
(define-key c-mode-map  (kbd "<C-tab>") 'company-complete)
(define-key c++-mode-map  (kbd "<C-tab>") 'company-complete)

(require 'cmake-ide)
(require 'subr-x)
(cmake-ide-setup)
(put 'scroll-left 'disabled nil)
(global-set-key (kbd "<f5>") 'cmake-ide-compile)

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(helm-mode 1)

(require 'nyan-mode)
(nyan-mode 1)
(global-set-key (kbd "<f12>") 'nyan-start-music)
(global-set-key (kbd "C-x <f12>") 'nyan-stop-music)

(defun duplicate-line(n)
  (interactive "P")
  (setq register 0)
  
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line)
  (cua-copy-region register)

  (defun p(n)
    (interactive)
    (if (not n)
	(setq n 1))
    (newline)
    (cua-paste register)
  
    (if (> n 1)
	(p (- n 1)))
    )
  (p n)
  )

(defun two_points ()
  (interactive)
  (insert "::")
  (company-complete)
  )

(defun draw_arrow ()
  (interactive)
  (insert "->")
  (company-complete)
  )

(global-set-key (kbd "C-c d") 'duplicate-line)

(add-hook
 'c++-mode-hook
 (lambda ()
   (local-set-key (kbd "§") 'two_points)
   (local-set-key (kbd "ù") 'draw_arrow)))

(add-hook
 'c-mode-hook
 (lambda ()
   (local-set-key (kbd "ù") 'draw_arrow)))

(with-eval-after-load 'flycheck
  (require 'flycheck-clang-analyzer)
  (flycheck-clang-analyzer-setup)
  )

(global-flycheck-mode 1)
(load-file "~/.emacs.d/plugins/member.functions.el")

(require 'cpputils-cmake)
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c m")

(require 'tern)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; (require 'company-tern)
;; (add-hook 'js-mode-hook '(lambda () (setq-local company-backends '((company-web company-css company-tern :with company-yasnippet)))))

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(defun namespace-indent ()
   (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'namespace-indent)

;; (c-set-offset
;;  '(c-offsets-alist . ((case-label . 0)
;;                       (inline-open . 0)
;;                       (substatement-open . 0)
;;                       (inlambda . 0) ; no extra indent for lambda
;;                       (block-open . 0) ; no space before {
;;                       (knr-argdecl-intro . -))))

;; (require 'rtags)
;; (defun setup_rtags ()
;;   "Rtags configuration.
;; Used only for nevigation."
;;   (interactive)
;;   (rtags-start-process-unless-running)
;;   (setq rtags-display-result-backend 'helm)
;;   (global-set-key  (kbd "M-.") 'rtags-find-symbol-at-point)
;;   (global-set-key  (kbd "M-?") 'rtags-location-stack-back)
;;   (global-set-key  (kbd "M-,") 'rtags-location-stack-forward)
;;   (global-set-key  (kbd "C-c r r") 'rtags-rename-symbol)
;;    ;; '(
     
;;    ;; 	 ("M-?"     .  rtags-find-references-at-point)
;;    ;;   ("M-,"     .  rtags-location-stack-back)
;;    ;;   ("C-,"   .    rtags-location-stack-forward)
;;    ;;   ("C-c r r" .  rtags-rename-symbolrtags-next-match)
;;    ;;   ))
;;   (add-hook 'kill-emacs-hook 'rtags-quit-rdm))

;; (setup_rtags)

;; (require 'srefactor)

;; (semantic-mode 1)

;; (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;; (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;; (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
;; (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
;; (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
;; (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)

;;; .Emacs ends here
