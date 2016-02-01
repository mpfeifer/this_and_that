;; [ header

;; Info  : Emacs initialization file
;; Author: Matthias
;; Date  : 20.06.2014

;; ]

;; [ personal information

(setq user-full-name "Matthias Pfeifer"
      user-mail-address "mpfeifer77@gmail.com")

;; ]

;; [ custom set variables

(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)

;; quiet reference to free variable warnings

(defvar Man-mode-map)

;; ]

;; [ packaging

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/"))
      package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/")

(package-initialize)

(defun mp/install-package (package)
  "Install PACKAGE if it has not yet been installed."
  (interactive "SPackage:")
  (if (package-installed-p package)
      nil
    (package-install package)))

(mp/install-package 'use-package)

(require 'use-package)

(setq use-package-verbose t
      use-package-always-ensure t)


;; ]

;; [ General Emacs Behaviour

(put 'downcase-region 'disabled nil)

(setq delete-exited-processes t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'symbol-to-string 'symbol-name)
(defalias 'string-to-symbol 'intern)

(global-set-key (kbd "<RET>") 'newline-and-indent)

(add-to-list 'load-path "~/.emacs.d/lib/")

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

(setq gc-cons-threshold 50000000)

(server-start)

;; ]

;; [ isearch

;; ‘C-w’     - Select the (rest of the) word the TextCursor is on as the search string;
;; ‘M-s C-e’ - Select the text up to the end of the line as the search string (this was bound to ‘C-y’ up until Emacs 24.1).
;; ‘M-s h r’ - Highlight regular expression (‘highlight-regexp’)
;; ‘M-s h u’ - Unhighlight regular expression
;; ‘M-s o’   - call ‘occur’ with the current search term

(global-set-key (kbd "C-s") 'isearch-forward-regexp)

(defadvice isearch-forward-regexp (before kill-ring-save-before-search activate)
  "Save region (if active) to kill-ring before starting isearch. This way region
can be inserted into isearch easily with C-y."
  (when (region-active-p)
    (kill-ring-save (region-beginning) (region-end))))

;; ]

;; [ s

(use-package s)

;;]

;; [ auto insert

(defun mp/yas-preprocessor()
  "Replace all yasnippets in buffer. Snippets must be marked with $(yas ...).
Snippets are actually expanded - but positions ($0, $1, etc.) are not respected."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "$(yas \\([a-zA-Z0-9_]+\\))" (point-max) t)
      (let ((mb (match-beginning 0))
            (ms (match-string 1))
            (me (match-end 0)))
        (goto-char mb)             ; go to the $ (the beginning of the match)
        (delete-char 6)            ; delete '$(yas ' prefix
        (forward-char (- me mb 6)) ; go to the end of the match
        (delete-char -1)           ; delete the last ')'
        (call-interactively yas-expand))))

  (setq auto-insert-directory "~/.emacs.d/templates/"
	auto-insert-query nil
	auto-insert-alist (quote
			   (
			    (("pom.xml//'" . "Maven Project Object Model") . ["template-pom.xml" mp/yas-preprocessor])
			    (("Activator.java" . "OSGI BundleActivator") . "Activator.java")
			    (("\\.html\\'" . "HTML Skeleton")
			     "<html>" > \n
			     "<head>" > \n
			     "<meta charset=\"utf-8\" />" > \n
			     "<title></title>" > \n
			     "<link rel=\"stylesheet\" type=\"text/css\" href=\"" (file-name-sans-extension (buffer-name)) ".css\" />" > \n
			     "<script src=\"http://code.jquery.com/jquery-1.11.1.js\"></script>" > \n
			     "<script src=\"" (file-name-sans-extension (buffer-name)) ".js\"></script>" > \n
			     "</head>" > \n
			     "<body>" \n > 
			     _ > \n
			     "</body>" > \n
			     "</html>" > \n)
			    (("\\.java\\'" . "Java") . ["template.java" mp/yas-preprocessor])
			    (("\\.xml\\'" . "XML") . ["template.xml" mp/yas-preprocessor])
			    (("\\.css\\'" . "Cascading Stylesheets File") . ["template.css" mp/yas-preprocessor])
			    (("\\.js\\'" . "Javascript Sourcecode") . ["template.js" mp/yas-preprocessor])
			    (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C++ header")
			     (upcase
			      (concat
			       (file-name-sans-extension (buffer-name))
			       "_"
			       (file-name-extension buffer-file-name)))
			     > "#ifndef " str \n
			     > "#define " str "\n\n"
			     > "class " (file-name-sans-extension (buffer-name)) " {\n"
			     "public:" "\n"
			     > (file-name-sans-extension (buffer-name)) "();\n"
			     > "virtual ~" (file-name-sans-extension (buffer-name)) "();\n"
			     "};" _
			     "\n\n"
			     > "#endif") ) ) ) )

(auto-insert-mode)

;; ]

;; [ appearence

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))

;; do not show startup screen
(setq inhibit-startup-screen t)

(require 'hl-line)
(global-hl-line-mode nil)

(use-package ample-zen-theme
  :config
  (load-theme 'ample-zen))

;; [ backups

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; ]

;; [ imenu

(setq imenu-auto-rescan t
      imenu-use-popup-menu nil
      imenu-space-replacement "-"
      imenu-sort-function 'imenu--sort-by-name)

;; ] 

;; [ ibuffer

(defun ibuffer-previous-line ()
  "Move point to last buffer when going before first buffer."
  (interactive)
  (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))))

(defun ibuffer-next-line ()
  "Wrap point to first buffer when goint after last buffer."
  (interactive)
  (next-line)
  (if (>= (line-number-at-pos) 
	  (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil)
    (add-hook 'ibuffer-mode-hook
	      '(lambda ()
		 (ibuffer-auto-mode 1)
		 (ibuffer-switch-to-saved-filter-groups "standard")
		 (define-key ibuffer-mode-map (kbd "p") 'ibuffer-previous-line)
		 (define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-previous-line)
		 (define-key ibuffer-mode-map (kbd "n") 'ibuffer-next-line)
		 (define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-next-line)))))
;; ]

;; [ emacs lisp mode

(defun mp/dotemacs-mode-hook ()
  (local-set-key (kbd "C-S-n") 'forward-paragraph)
  (local-set-key (kbd "C-S-p") 'backward-paragraph)
  (local-set-key (kbd "C-*") 'imenu)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^;; \\[")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^;; ]$")
  (setq-local imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression '((nil "^;; \\[ \\(.*\\)" 1))))

(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun mp/emacs-lisp-mode-hook ()
  (eldoc-mode 1)
  (when (string= (buffer-name) "init.el")
    (mp/dotemacs-mode-hook)
    )
  (local-set-key (kbd "C-c C-c") 'byte-compile-current-buffer)
  (electric-pair-mode)
  ) 

(add-hook 'emacs-lisp-mode-hook 'mp/emacs-lisp-mode-hook)

;; ]

;; [ emacs lisp slime navigation

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))
;; ]

;; [ save history

(setq savehist-file "~/.emacs.d/various/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

;; ]

;; [ expand region

;; Very handy package. Sets er/try-expand-list on a per mode basis to list of
;; defuns. Each defun marks part of the buffer. Incrementally largens the part
;; of the buffer a defun operats on. The next larger marked part is then set
;; to the region.
;; To customize add defun to er/try-expand-list in any mode hook.

(use-package expand-region
  :bind
  ("C-v" . er/expand-region)
  ("C-S-v" . er/contract-region))

;; ]

;; [ yasnippet

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-,") 'yas-expand))

;; ]

;; [ auto complete
;;
;; TODO - want to trigger auto-complete by key
;; TODO - want keybindings in auto-complete for search and scroll
;; TODO - want to understand how documentation in auto-complete works
;; TODO - want to evaluate how well auto-complete is pushed forward or if its better to switch to alternative package

(use-package auto-complete
  :config
  (require 'auto-complete-config)
  (define-key ac-menu-map (kbd "C-s") 'ac-isearch)

  (add-hook 'html-mode-hook '(lambda ()
			       "Enable html auto-complete for html-mode."
			       (require 'ac-html)
			       (auto-complete-mode)
			       (setq ac-sources '(ac-source-html-attribute-value
						  'ac-source-html-tag
						  'ac-source-html-attribute))))

  (defun mp/css-mode-hook ()
    (setq ac-sources '(ac-source-css-property)))

  (add-hook 'css-mode-hook' mp/css-mode-hook)

  (setq ac-sources '(ac-source-features ;; collects 'require-able features from the file sytem
		     ac-source-functions
		     ac-source-variables
		     ac-source-symbols)))
;; ]

;; [ avy-mode

(use-package avy
  :bind ("C-S-j" . avy-goto-word-or-subword-1) )

;; ]

;; [ js2 mode

(use-package js2-mode
  :mode "\\.js\\'" )

;; ]

;; [ info browser

(defadvice info (before
		 info-browser-own-frame (&optional file-or-node buffer)
		 activate)
  "Open new frame for info browser."
  (let ((next-index 0))
    (while (get-buffer (format "info[%d]" next-index))
      (setq next-index (+ 1 next-index)))
    (setq buffer (format "info[%d]" next-index)))
  (select-frame (make-frame)))

(defun mp/Info-mode-hook ()
  "Personal info mode hook."
  (define-key Info-mode-map (kbd "C-s") 'isearch-forward-regexp) )

(add-hook 'Info-mode-hook 'mp/Info-mode-hook)

;; ]

;; [ calendar

;;; (make-toggle-buffer buffer-name buffer-factory prefered-buffer-position)
(global-set-key (kbd "<f4>") '(lambda () (interactive)
				"Toggle calendar visibility"
				(let ((calendar-window (get-buffer-window "*Calendar*")))
				  (if calendar-window
				      (delete-window calendar-window)
				    (calendar)))))

(setq calendar-mark-holidays-flag t
      calendar-date-style 'european
      calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; german weekdays
(setq calendar-day-name-array
      ["Sonntag" "Montag" "Dienstag" "Mittwoch"
       "Donnerstag" "Freitag" "Samstag"])

;; german months
(setq calendar-month-name-array
      ["Januar" "Februar" "März" "April" "Mai" "Juni"
       "Juli" "August" "September" "Oktober" "November" "Dezember"])

;; german seasons
(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

;; german holidays
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
	(holiday-fixed 5 1 "1. Mai")
	(holiday-fixed 10 3 "Tag der Deutschen Einheit")))

(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent")
	(holiday-float 12 0 -3 "2. Advent")
	(holiday-float 12 0 -2 "3. Advent")
	(holiday-float 12 0 -1 "4. Advent")
	(holiday-fixed 12 25 "1. Weihnachtstag")
	(holiday-fixed 12 26 "2. Weihnachtstag")
	(holiday-fixed 1 6 "Heilige Drei Könige")
	;; Date of Easter calculation taken from holidays.el.
	(let* ((century (1+ (/ displayed-year 100)))
	       (shifted-epact (% (+ 14 (* 11 (% displayed-year 19))
				    (- (/ (* 3 century) 4))
				    (/ (+ 5 (* 8 century)) 25)
				    (* 30 century))
				 30))
	       (adjusted-epact (if (or (= shifted-epact 0)
				       (and (= shifted-epact 1)
					    (< 10 (% displayed-year 19))))
				   (1+ shifted-epact)
				 shifted-epact))
	       (paschal-moon (- (calendar-absolute-from-gregorian
				 (list 4 19 displayed-year))
				adjusted-epact))
	       (easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))
	  (filter-visible-calendar-holidays
	   (mapcar
	    (lambda (l)
	      (list (calendar-gregorian-from-absolute (+ easter (car l)))
		    (nth 1 l)))
	    '(
	      (-48 "Rosenmontag")
	      ( -2 "Karfreitag")
	      ( 0 "Ostersonntag")
	      ( +1 "Ostermontag")
	      (+39 "Christi Himmelfahrt")
	      (+49 "Pfingstsonntag")
	      (+50 "Pfingstmontag")
	      (+60 "Fronleichnam")))))
	(holiday-fixed 8 15 "Mariä Himmelfahrt")
	(holiday-fixed 11 1 "Allerheiligen")
	(holiday-float 11 3 1 "Buß- und Bettag" 16)
	(holiday-float 11 0 1 "Totensonntag" 20)))

;;(setq calendar-holidays
;;      (append general-holidays local-holidays other-holidays
;;	      christian-holidays solar-holidays))

;; ]

;; [ session management

;; save and restore open buffers
(desktop-save-mode)

;; [ org mode

(require 'ob-plantuml)

(setq dot.exe "C:\\graphviz\\bin\\dot.exe"
      org-plantuml-jar-path "C:\\java\\lib\\plantuml.jar")

(setq org-ellipsis "…")  ;; ⤵, ▼, ↴, ⬎, ⤷, ⋱

(setenv "GRAPHVIZ_DOT" dot.exe)

(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(defun mp/org-mode-hook-extension ()
  "org mode hook extender [mp]"
  (auto-fill-mode)
  (local-set-key (kbd "<return>") 'org-return-indent))

(add-hook 'org-mode-hook 'mp/org-mode-hook-extension)

;; ]

;; [ prodigy service manager

(defsubst mp/toggle-prodigy-buffer ()
  (interactive)
  (if (string= (buffer-name) "*prodigy*")
      (quit-window)
    (prodigy)))

(use-package prodigy
  :config
  (global-set-key (kbd "<f5>") 'mp/toggle-prodigy-buffer)
  (prodigy-define-service
   :name "Echo Server"
   :command "mvn"
   :args '("exec:java -DmainClass=\"EchoServer-1.0.jar\"")
   :cwd "/home/user/java/EchoServer/" ) )

;; ]

;; [ speedbar

(use-package sr-speedbar
  :bind ("<f6>" . sr-speedbar-toggle) )

;; ]

;; [ tags

(setq tags-revert-without-query t)

;; ]

;; [ perl mode

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook '(lambda ()
			      (interactive)
			      (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point)))

;; ]

;; [ hungry delete mode
;;
;; This mode makes backspace erase all consecutive whitespace (instead of just a single one).

(use-package hungry-delete
  :config
  (require 'hungry-delete)
  (global-hungry-delete-mode))

;; ]

;; [ eshell

(defun mp/eshell-mode-hook ()
  "Personal eshell mode hook."  
  (interactive)  
  (auto-complete-mode)
  (setq ac-sources '(ac-source-filename ac-source-files-in-current-dir))
  (local-set-key (kbd "C-c C-c") 'mp/eshell)
  )

(add-hook 'eshell-mode-hook 'mp/eshell-mode-hook)

(defconst eshell-window-height -15 "Height of eshell window")
(defconst eshell-buffer-name "*eshell*")

(defun mp/eshell ()
  (interactive)
  (let ((numWindows (length (window-list))))
    (if (string= (buffer-name) eshell-buffer-name)
	;; Current buffer is eshell buffer. Lets close it.
	(if (eq 1 numWindows)
	    (delete-frame)
	  (delete-window))
      (if (eq 1 numWindows)
	  ;; Current buffer is not an eshell buffer. Lets start one.
	  (let ((newwindow (split-window nil eshell-window-height)))
	    (select-window newwindow)
	    (eshell)
	    )
	(progn
	  (select-frame (make-frame))
	  (eshell))))))

(global-set-key (kbd "<f7>") 'mp/eshell)

(defun eshell-emergency-exit ()
  "When eshell refuses to close with \"Text is read-only.\" message
exit eshell with this function instead."
  (interactive)
  (let ((inhibit-read-only t)) (kill-this-buffer)))

;; ]

;; [ C/C++

(defun mp/c-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'c-mode-hook 'mp/c-mode-hook)

;; ]

;; [ web development

(defgroup mp nil "All things related to my customization [mp]." :group 'Emacs)

(defgroup development nil "All things related to development [mp]." :group 'mp)

(defgroup web nil "All things related to web development [mp]." :group 'development)

(defcustom web-project-root "~/www/" "New web projects are stored in this directory." :group 'web)

(defun mp/start-web-project (name)
  (interactive "MProjectname? ")
  (let ((projectroot (concat web-project-root name)))
    (unless (file-exists-p projectroot)
      (mkdir projectroot))
    (select-frame (make-frame))
    (split-window-vertically)
    (find-file (concat projectroot "/" name ".html"))
    (other-window 1)
    (find-file (concat projectroot "/" name ".js"))
    (split-window-horizontally)
    (find-file (concat projectroot "/" name ".css"))
    (other-window -1)))

;; ]

;; [ frame+window handling

(defgroup frames+windows nil "All things related to windows and frames [mp]." :group 'mp)

(defcustom mp/frame-configuration-file "~/.emacs.d/frame-configuration.el" "New web projects are stored in this directory." :group 'famres+windows)

(winner-mode)

(add-hook 'kill-emacs-hook #'(lambda ()
			       "Store window configuration in filesystem."
			       (interactive)
			       (frame-configuration-to-register ?w)
			       (with-temp-buffer
				 (insert (prin1-to-string (get-register ?w)))
				 (write-region (point-min) (point-max) mp/frame-configuration-file))))

(defun mp/detach-window ()
  "Iff current frame hosts at least two windows, close current window
and display corresponding buffer in new frame."
  (interactive)
  (if (not (one-window-p))
      (let ((buffer (current-buffer)))
	(delete-window)
	(display-buffer-pop-up-frame buffer nil))
    (message "Refusing to detach window when one-window-p is true.")))

(defun split-window-below-select ()
  "Just like split-window-below, but select the newly created window."
  (interactive)
  (split-window-below)
  (other-window 1) )

(defun split-window-right-select ()
  "Just like split-window-right, but select the newly created window."
  (interactive)
  (split-window-right)
  (other-window 1) )

(global-set-key (kbd "<f1>") 'mp/detach-window)
(global-set-key (kbd "<f2>") 'make-frame)
(global-set-key (kbd "<f3>") 'delete-frame)
(global-set-key (kbd "C-x 2") 'split-window-below-select)
(global-set-key (kbd "C-x 3") 'split-window-right-select)

;; ]

;; [ ediff

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; ]

;; [ whitespace mode

(global-set-key (kbd "<f12> w") 'whitespace-mode)

;; ]

;; [ Man mode

(defsubst scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

(defsubst scroll-down-one-line ()
  (interactive)  
  (scroll-down 1)
  )

(defun mp/man-mode-hook ()
  (define-key Man-mode-map (kbd "C-n") 'scroll-up-one-line)
  (define-key Man-mode-map (kbd "C-p") 'scroll-down-one-line))

(add-hook 'Man-mode-hook 'mp/man-mode-hook)

;; ]

;; [ java mode

;; (defvar mp/jde-autoloaded nil "Wether or not jdee has already been autoloaded.")

;; (eval-when-compile
;;   (setq jdee-path "~/.emacs.d/jdee-2.4.1/lisp")
;;   (add-to-list 'load-path jdee-path)
;;   (require 'jde))

;; (defun mp/jde-autoloader ()
;;   "- delayed auto-loading for jde"
;;   (when (not mp/jde-autoloaded)
;;     (setq mp/jde-autoloaded t)
;;     (setq jdee-path "~/.emacs.d/jdee-2.4.1/lisp")
;;     (add-to-list 'load-path jdee-path)
;;     (autoload 'jde-mode "jde" "JDE mode" t))
;;   (jde-mode))

;; (setq auto-mode-alist
;;       (append '(("\\.java\\'" . mp/jde-autoloader)) auto-mode-alist))

;; ]

;; [ dired

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "<backspace>")
	      (lambda () (interactive) (find-alternate-file "..")))))

;; ]

;; [ xml editin

;; handy: (nxml-balanced-close-start-tag-inline)

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]*[^/]>"
	       "-->\\|</[^/>]*[^/]>"
	       "<!--"
	       sgml-skip-tag-forward
	       nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(defun mp/nxml-mode-setup ()
  (local-set-key (kbd "<") '(lambda () (interactive) (insert "<") (yas-expand))))

(add-hook 'nxml-mode-hook 'mp/nxml-mode-setup)

;; ]

;; [ maven integration

(defun mp/maven-integration ()
  (interactive)
  (when (string= "pom.xml" (buffer-name))
    (progn
      (setq compile-command "mvn clean install")
      (local-set-key (kbd "C-c C-c") 'compile))))

(add-hook 'nxml-mode-hook 'mp/maven-integration)

;; ]

;; [ ido-mode

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

(ido-mode t)
(ido-everywhere)

(use-package smex
  ;; https://github.com/nonsequitur/smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode)
  (setq ido-cr+-max-items 50000))

;; [ tags

(setq tags-file-name nil
      tags-table-list nil)

;; ]

;; [ Where was I [editing text]?

(defun mp/store-lot-position ()
  (point-to-register ?z))

(defun mp/goto-lot-position ()
  (interactive)
  (jump-to-register ?z))

(add-hook 'post-self-insert-hook 'mp/store-lot-position)

(global-set-key (kbd "C-c q") 'mp/goto-lot-position)

;; ]

;; [ occur

(defun mp/occur-mark-regexp ()
  (interactive)
  (when (not mp/occur-marked-regexp)
    (setq mp/occur-origin-buffer (current-buffer))
    (highlight-regexp mp/occur-last-regexp)))

(defun mp/occur-next-line ()
  (interactive)
  (next-line)
  (occur-mode-goto-occurrence)
  (recenter-top-bottom 2)
  (mp/occur-mark-regexp)
  (other-window 1))

(defun mp/occur-prev-line ()
  (interactive)
  (previous-line)
  (occur-mode-goto-occurrence)
  (recenter-top-bottom 2)
  (mp/occur-mark-regexp)  
  (other-window 1))

(defun mp/occur-quit-window ()
  (interactive)
  (quit-window)
  (unhighlight-regexp mp/occur-last-regexp)
  (setq mp/occur-marked-regexp nil
	mp/occur-last-regexp nil
	mp/occur-origin-buffer nil))

(global-set-key (kbd "C-o") 'occur)

(defvar mp/occur-marked-regexp nil)
(defvar mp/occur-last-regexp nil)
(defvar mp/occur-origin-buffer nil)

(defadvice occur (after select-occur-window-after-occur activate)
  "Do make *Occur* buffer current after calling occur."
  (let ((occur-window (get-buffer-window "*Occur*")))
    (when occur-window
      (select-window occur-window)))
  (setq mp/occur-last-regexp (concat (car regexp-history) ".*")))

(defun mp/occur-mode-hook ()
  (local-set-key (kbd "C-n") 'mp/occur-next-line)
  (local-set-key (kbd "C-p") 'mp/occur-prev-line)
  (local-set-key (kbd "q") 'mp/occur-quit-window))

(add-hook 'occur-mode-hook 'mp/occur-mode-hook)

;; ]

;; TODO: create directories: various and auto-save
