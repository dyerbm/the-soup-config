;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin Dyer"
      user-mail-address "dyerbm@mcmaster.ca")

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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t
      projectile-project-search-path '("~/Documents")
      )


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


(let ((default-directory (expand-file-name "packages" doom-private-dir)))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'exec-path "/usr/local/texlive/2021/bin/x86_64-linux")
(add-to-list 'exec-path "/usr/local/texlive/2021/texmf-dist/tex/")

(setq doom-localleader-key ";")

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(require 'org)

(setq org-startup-folded t)

(setq org-preview-latex-default-process 'dvipng)
(setq org-latex-packages-alist '(("" "physics" t)))
(setq org-latex-packages-alist '(("" "siunitx" t)))

;; auto compile latex fragments
(use-package! org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; latex processing
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f")) ;check what this does

(setq bibtex-dialect 'BibTeX)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
(setq company-global-modes '(not org-mode)) ;disable company mode in org

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due today"
                                   :deadline today)
                                  (:name "Important"
                                   :priority "A")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)
                                  (:name "Big Outcomes"
                                   :tag "bo")))
  :config
  (org-super-agenda-mode)
  )
(setq org-agenda-files (list "~/Documents/Org/SchoolTasks.org"))

(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

(use-package org-ref
  :ensure t
  :init
  (with-eval-after-load 'ox
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format.

Do this only if the export backend is `html' or a derivative of
that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
      (when (org-export-derived-backend-p backend 'html)
        (org-ref-process-buffer 'html)))
    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html)))

(setq reftex-default-bibliography '("~/Documents/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/Documents/bibliography/notes.org"
      org-ref-default-bibliography '("~/Documents/bibliography/references.bib")
      org-ref-pdf-directory "~/Documents/bibliography/pdfs")

(setq bibtex-completion-bibliography "~/Documents/bibliography/references.bib"
      bibtex-completion-library-path "~/Documents/bibliography/pdfs/"
      bibtex-completion-notes-path "~/Documents/bibliography/notes.org"
      bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath)))


(require 'bibtex)

(setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5
	org-ref-bibtex-hydra-key-binding (kbd "H-b"))

(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)

;; User org-ref-ivy
(require 'org-ref-ivy)

(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link) ;; need to be able to use this after all

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable) ;Drag-and-drop to dired

(use-package! pdf-tools
  :config
  (evil-define-key 'normal pdf-view-mode-map (kbd ":") 'pdf-view-goto-page)
  (map! :localleader
        :map pdf-view-mode-map
          "f" #'pdf-occur
          ;; History
          "c" #'pdf-history-clear
          "j" #'pdf-history-backward
          "k" #'pdf-history-forward

          "o" #'pdf-outline))

(add-hook! 'pdf-view-mode-hook
           (pdf-view-midnight-minor-mode))

(use-package! ace-window
  :config
  (map! :leader
        "k" nil
        :desc "ace-window" "k" #'ace-window)
  (setq aw-scope 'global
        aw-ignore-on nil ; allow ace to jump to any buffer
        ))

(map! :leader
      (:desc "next buffer" "D" #'switch-to-next-buffer
        :desc "prev buffer" "d" #'switch-to-prev-buffer
        )
      (:prefix "s"
        :desc "swiper-isearch-thing-at-point" "t" #'swiper-isearch-thing-at-point)
        ;; :desc "helm-projectile-rg" "p" #'helm-projectile-rg)
      (:desc "repeat last command" "." #'repeat))

(use-package! ivy
 :config
 (map! :leader
     "A" #'ivy-switch-buffer
      "a" nil
      (:prefix ("a" . "switch-to-buffer")
       :desc "c"   "c"  #'(lambda () (interactive) (my/ivy-switch-buffer "\(cpp\|c\)"))
       :desc "h"   "h"  #'(lambda () (interactive) (my/ivy-switch-buffer "\(hpp\|h\)"))
       :desc "m"   "m"  #'(lambda () (interactive) (my/ivy-switch-buffer "\(mat\|m\)"))
       :desc "pdf" "f"  #'(lambda () (interactive) (my/ivy-switch-buffer "pdf"))
       :desc "py"  "p"  #'(lambda () (interactive) (my/ivy-switch-buffer "py"))
       :desc "org" "o"  #'(lambda () (interactive) (my/ivy-switch-buffer "org"))
       :desc "el"  "e"  #'(lambda () (interactive) (my/ivy-switch-buffer "el"))
       :desc "bib" "b"  #'(lambda () (interactive)  (my/ivy-switch-buffer "bib")))))

(defun my/ivy-switch-buffer (extension)
  ;; Show available buffers for a given extension
  (interactive)
  (let ((completion-regexp-list (list (concat ".\." extension "$"))))
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer)))

(defun my/switch-to-next-buffer-with-same-extension ()
  (interactive)
(save-match-data ; is usually a good idea
  (string-match "\..$" (buffer-name))))

(map! :leader
      (:prefix "w"
       :desc "maximize window" "f" #'my/toggle-maximize-buffer
       :desc "make new frame"  "n" #'make-frame))

(defun my/toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(use-package! magit
  :config
  (map! :leader
        (:prefix "g"
         :desc "status" "G" #'my/magit-status
         :desc "buffer-lock" "T" #'magit-toggle-buffer-lock)))

(defun my/magit-status ()
  "Use ivy to specify directory from which to open a magit status buffer.
Default starting place is the home directory."
  (interactive)
  (let ((default-directory "~/"))
    (ivy-read "git status: " #'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :action #'(lambda (x)
                          (magit-status x))
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'my/magit-status)))

(map! :leader
      "x" nil
      (:prefix ("x" . "dired")
       :desc "dired here" "d" #'(lambda () (interactive) (dired default-directory))
       :desc "dired" "D" #'dired))

(setq delete-by-moving-to-trash t) ; Move to trash bin instead of permanently deleting it

(with-eval-after-load 'ox
  (require 'ox-hugo))

(use-package! ox-hugo
  :ensure t ;Auto-install the package from Melpa
  :pin melpa ;packages-achrives
  :after ox)
