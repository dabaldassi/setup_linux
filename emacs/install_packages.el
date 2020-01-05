(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 25)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents)

(package-install 'company)
(package-install 'company-irony)
(package-install 'company-c-headers)
(package-install 'company-anaconda)
(package-install 'neotree)
(package-install 'helm)
(package-install 'multiple-cursors)
(package-install 'cmake-ide)
(package-install 'nyan-mode)
(package-install 'yasnippet)
(package-install 'flycheck-clang-analyzer)

