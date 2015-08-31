;; company.el

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-anaconda))

(provide 'config-company)

;; company-c-headers
(require `company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.2.1/")
