(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(auctex ox-hugo))
 '(safe-local-variable-values
   '((eval let ((root (locate-dominating-file default-directory ".dir-locals.el")))
      (load (expand-file-name "lisp/latex-docs.el" root) nil t)
      (latex-docs-setup-buffer)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'customize-variable 'disabled nil)
(put 'erase-buffer 'disabled nil)
