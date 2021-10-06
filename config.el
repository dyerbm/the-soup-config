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

(setq doom-localleader-key ";")

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

;; (save-match-data ; is usually a good idea
;;       (and (string-match "\(\..$\)." "eric.dyer@l3harris.com")
;;            (setq user (match-string 1 "eric.dyer@l3harris.com")
;;                  domain (match-string 1 "eric.dyer@l3harris.com") ) ))

;; orgmode stuff
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)
(setq org-agenda-files (list "~/Documents/Org/SchoolTasks.org"))

;;orgmode latex stuff
(setq org-preview-latex-default-process 'dvipng)
(setq org-latex-packages-alist '(("" "physics" t)))
;auto compile latex fragments
(use-package! org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; start-up format and phscroll
(use-package! phscroll)
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-startup-truncated nil)
(use-package! org-phscroll)

;; org ref set-up
(require 'org-ref)
(setq reftex-default-bibliography '("~/Documents/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/Documents/bibliography/notes.org"
      org-ref-default-bibliography '("~/Documents/bibliography/references.bib")
      org-ref-pdf-directory "~/Documents/bibliography/pdfs")

(setq bibtex-completion-bibliography "~/Documents/bibliography/references.bib"
      bibtex-completion-library-path "~/Documents/bibliography/pdfs"
      bibtex-completion-notes-path "~/Documents/bibliography/notes.org")

;(setq bibtex-completion-pdf-open-function 'pdf-tools)
(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results)))
    (funcall bibtex-completion-pdf-open-function (car (bibtex-completion-find-pdf key)))))

(setq org-ref-open-pdf-function #'my/org-ref-open-pdf-at-point)

;; Allow for pulling pdfs from sci-hub
;; Thanks to user Ajned on the emacs stack exchange
;; Sci-hub function
(defun sci-hub-pdf-url (doi)
  "Get url to the pdf from SCI-HUB"
  (setq *doi-utils-pdf-url* (concat "https://sci-hub.se/" doi) ;captcha
        *doi-utils-waiting* t
        )
  ;; try to find PDF url (if it exists)
  (url-retrieve (concat "https://sci-hub.se/" doi)
            (lambda (status)
              (goto-char (point-min))
              (while (search-forward-regexp "\\(https://\\|//sci-hub.se/downloads\\).+download=true'" nil t)
                (let ((foundurl (match-string 0)))
                  (message foundurl)
                  (if (string-match "https:" foundurl)
                  (setq *doi-utils-pdf-url* foundurl)
                (setq *doi-utils-pdf-url* (concat "https:" foundurl))))
                (setq *doi-utils-waiting* nil))))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)

;; Update doi function to use the sci-hub-pdf-url function
(defun doi-utils-get-bibtex-entry-pdf (&optional arg)
    "Download pdf for entry at point if the pdf does not already exist locally.
The entry must have a doi. The pdf will be saved to
`org-ref-pdf-directory', by the name %s.pdf where %s is the
bibtex label.  Files will not be overwritten.  The pdf will be
checked to make sure it is a pdf, and not some html failure
page. You must have permission to access the pdf. We open the pdf
at the end if `doi-utils-open-pdf-after-download' is non-nil.

With one prefix ARG, directly get the pdf from a file (through
`read-file-name') instead of looking up a DOI. With a double
prefix ARG, directly get the pdf from an open buffer (through
`read-buffer-to-switch') instead. These two alternative methods
work even if the entry has no DOI, and the pdf file is not
checked."
    (interactive "P")
    (save-excursion
      (bibtex-beginning-of-entry)
      (let ( ;; get doi, removing http://dx.doi.org/ if it is there.
        (doi (replace-regexp-in-string
          "https?://\\(dx.\\)?.doi.org/" ""
          (bibtex-autokey-get-field "doi")))
        (key (cdr (assoc "=key=" (bibtex-parse-entry))))
        (pdf-url)
        (pdf-file))
    (setq pdf-file (concat
            (if org-ref-pdf-directory
                (file-name-as-directory org-ref-pdf-directory)
              (read-directory-name "PDF directory: " "."))
            key ".pdf"))
    ;; now get file if needed.
    (unless (file-exists-p pdf-file)
      (cond
       ((and (not arg)
         doi
         (if (doi-utils-get-pdf-url doi)
             (setq pdf-url (doi-utils-get-pdf-url doi))
           (setq pdf-url "https://www.sciencedirect.com/science/article/")))
        (url-copy-file pdf-url pdf-file)
        ;; now check if we got a pdf
        (if (org-ref-pdf-p pdf-file)
        (message "%s saved" pdf-file)
          (delete-file pdf-file)
          ;; sci-hub fallback option
          (setq pdf-url (sci-hub-pdf-url doi))
          (url-copy-file pdf-url pdf-file)
          ;; now check if we got a pdf
          (if (org-ref-pdf-p pdf-file)
          (message "%s saved" pdf-file)
        (delete-file pdf-file)
        (message "No pdf was downloaded.") ; SH captcha
        (browse-url pdf-url))))
       ;; End of sci-hub fallback option
       ((equal arg '(4))
        (copy-file (expand-file-name (read-file-name "Pdf file: " nil nil t))
               pdf-file))
       ((equal arg '(16))
        (with-current-buffer (read-buffer-to-switch "Pdf buffer: ")
          (write-file pdf-file)))
       (t
        (message "We don't have a recipe for this journal.")))
      (when (and doi-utils-open-pdf-after-download (file-exists-p pdf-file))
        (message "Here")
        (org-open-file pdf-file))))))

(setq doi-utils-open-pdf-after-download t) ;always open the pdf after downloading
(setq doi-utils-make-notes t) ;auto generate notes

;; latex processing
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f")) ;check what this does

(bibtex-set-dialect 'BibTeX)
