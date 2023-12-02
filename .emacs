;; .emacs

;;==============================================================================
;; kpnote-s
;;==============================================================================
;; Get a list of indent options
;; C-h v c-offsets-alist RET
;; C-c C-s tells you what syntactic rule controls the current cursor position

;; C-h k <some_key_combination>
;; will tell you what command is bound to that key combination

;; Uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; Enable cscope - search C/C++ code for where symbols are used & defined
;;(require 'xcscope)
;;(global-set-key [f1] 'cscope-find-this-text-string)
;;(global-set-key [f2] 'cscope-find-this-symbol)
;;(global-set-key [f3] 'cscope-find-global-definition-no-prompting)
;;(global-set-key [f4] 'cscope-find-functions-calling-this-function)

;; Get emacs to be quiet
(setq ring-bell-function 'ignore)

;; Don't show a splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Insert a line above cursor (Ctrl + Shift + Return)
(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; Bound to: ctrl + shift + return
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Use nxml for html formating (it's much better than html-mode)
(fset 'html-mode 'nxml-mode)

;; Don't auto add carriage returns
(add-hook 'nxml-mode-hook 'turn-off-auto-fill)

;; Loads: highlight-symbol.el, rainbow-delimiters.el, real-auto-save.el, and
;; typescript-mode.el
;; kptodo uncomment all these ~ forgot to grab the lisp @_@
;; (add-to-list 'load-path "~/.emacs.d/lisp")

;; kptodo uncomment
;; Highlight-symbol.el init
;;(require 'highlight-symbol)
;;(add-hook 'c++-mode-hook 'highlight-symbol-mode)
;;(add-hook 'java-mode-hook 'highlight-symbol-mode)
;;(add-hook 'makefile-mode-hook 'highlight-symbol-mode)
;;(add-hook 'sgml-mode-hook 'highlight-symbol-mode) ;; (4xml)
;;(add-hook 'typescript-mode-hook 'highlight-symbol-mode)
;;(add-hook 'javascript-mode-hook 'highlight-symbol-mode)
;;(add-hook 'nxml-mode-hook 'highlight-symbol-mode)
;;(setq highlight-symbol-idle-delay 1.0)

;; Python indent 2 spaces
(add-hook 'python-mode-hook '(lambda () 
 (setq python-indent 2)))

;; Make sure to still display the line number in buffers with very long lines
(setq line-number-display-limit-width 512)

;; kptodo uncomment
;; Rainbow highlighting
;; Highlight nested parentheses, brackets, and braces according to their depth.
;; (require 'rainbow-delimiters)
;; (add-hook 'c++-mode-hook 'rainbow-delimiters-mode)

;; Emacs built-in compiler stuff
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'next-error)

;; Make it so we don't have to hit enter to compile
(setq compilation-read-command nil)

;; Don't open the compile window
(defun my-compile-finish (buffer outstr)
  (unless (string-match "finished" outstr)
    (switch-to-buffer-other-window buffer))
  t)
(setq compilation-finish-functions 'my-compile-finish)
(require 'cl)
(defadvice compilation-start
  (around inhibit-display
      (command &optional mode name-function highlight-regexp)) 
  (if (not (string-match "^\\(find\\|grep\\)" command))
      ;; kpnote changed from 'flet' to 'cl-flet'
      ;; (flet is obsolete in emacs ver 26.1)
      (cl-flet ((display-buffer)
         (set-window-point)
         (goto-char)) 
    (fset 'display-buffer 'ignore)
    (fset 'goto-char 'ignore)
    (fset 'set-window-point 'ignore)
    (save-window-excursion 
      ad-do-it))
    ad-do-it))
(ad-activate 'compilation-start)

;; Open read only
(defun command-line-find-file-read-only (switch)
   (find-file-read-only (pop command-line-args-left)))
(add-to-list 'command-switch-alist
             '("--read-only" . command-line-find-file-read-only))

;; Enable visual feedback on selections
(setq transient-mark-mode t)

;; Display the column number
(setq column-number-mode t)

;; Make word completion (Alt-/) case sensitive 
(setq dabbrev-case-fold-search nil)

;; Auto save bookmarks whenever they change
(setq bookmark-save-flag 1)

;; Some shortcuts
(global-set-key "\C-f" 'goto-line)
(global-set-key "\C-xr" 'replace-string)
(global-set-key [C-tab] 'other-window)
(global-set-key "\M-s" 'ispell-word)

;; Lazy-boi way to avoid typing std::cout
(defun coutMacro ()
  (interactive)
  (insert "std::cout <<  << std::endl;")
  (move-to-column 0)
  (search-forward "<< ")
  (indent-according-to-mode)
  )
(global-set-key "\M-c" 'coutMacro)

;; Copy the current line and move the cursor to the next line
(defun duplicate-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (beginning-of-line)
  
;; Move forward-line 1
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key "\C-j" 'duplicate-line)

;; Copy the word under the cursor
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )
(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (forward-char 1)
  (copy-thing 'backward-word 'forward-word arg)
  (backward-char 1)
  )
(global-set-key "\C-o" 'copy-word)


(setq-default indent-tabs-mode nil)

;; Background / foreground colors
(setq default-frame-alist '((background-color . "black")
                            (foreground-color . "white")))

;; Stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop leaving the backup~ (filename.extension~) scattered everywhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

;; Don't save backup files at all!
(setq make-backup-files nil)
(setq auto-save-default nil)

;; =====================================================================
;; Auto Save
;; =====================================================================
(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

;; Auto-save-interval - number of input events before a save
;; Auto-save-timeout  - number of seconds before a save
(setq auto-save-interval 1 auto-save-timeout 1)

;; Scroll only one line at the bottom of the file
(setq scroll-step 1)

;; Reload the buffer from the file on disk without confirming the reload
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))
(global-set-key [f7] 'revert-buffer-no-confirm)

;; Hooks for C/C++ style
(add-hook 'c-mode-hook
	  (function (lambda ()
		      (setq c-file-style "local"))))
(add-hook 'c++-mode-hook
	  (function (lambda ()
		      (setq c-file-style "local"))))

(c-add-style "local"
             '(
               (c-basic-offset . 2)
               (c-offsets-alist . (
                                   (substatement-open    . 0)
                                   (member-init-intro    . 1)
                                   (case-label  . 2)
                                   (inclass              . 2)
                                   (inline-open          . 0)
                                   ))))

(c-set-offset 'substatement-open 0)
(c-set-offset 'case-label '+)
(setq c-default-style "linux" c-basic-offset 2)

;; Java offsets
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)
                            (c-set-offset 'substatement-open '0)
                            (c-set-offset 'statement-cont '0)
                            ))

 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(custom-set-variables
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 ;; Removes the menu bar at the top
 ;; '(menu-bar-mode nil)
 '(perl-indent-level 2)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
)

;; GLSL Vertex and Fragment shaders should open in c++ mode
(setq auto-mode-alist (cons '("\\.vert$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.frag$" . c++-mode) auto-mode-alist))

;; String utils
(defun eassist-string-last (string n)
  (substring string (- (length string) n)))
(defun eassist-string-without-last (string n)
  (substring string 0 (max 0(- (length string) n))))
(defun eassist-string-ends-with (string end)
  (string= end (eassist-string-last string (length end))))

;; Hot-key switching between header and body files (must have the same name)
(defun eassist-do-for-first-suitable (lst action)
  (if (null lst)
      nil
    (let ((res (funcall action (car lst))))
      (if (null res)
          (eassist-do-for-first-suitable (cdr lst) action)
        res))))

;; Will match with the following chars after the '.'
(setq eassist-header-switches '(
                             ("h" . ("cpp" "cc" "c"))
                             ("hpp" . ("cpp"))
                             ("cpp" . ("h" "hpp"))
                             ("c" . ("h"))
                             ("cc" . ("h" "hpp"))
			     ("C" . ("H"))
			     ("H" . ("C"))
                             ))

(defun eassist-switch-h-cpp ()
  (interactive)
  (let ((ext (file-name-extension (buffer-file-name))))
    (if (null (eassist-do-for-first-suitable 
	       eassist-header-switches
	       (lambda (i)
		 (if (string= (car i) ext)
		     (progn
		       (if (null 
			    (eassist-do-for-first-suitable (cdr i)
							   'eassist-try-h-cpp))
			   (message "No corresponding pair (header or body)"))
		       ext)
		   nil))))
        (message "Current file is not a header or body file!"))))

(defun eassist-try-h-cpp (ext)
  (eassist-find-if-exist
   (concat (eassist-string-without-last (buffer-file-name) (length
							    (file-name-extension (buffer-file-name)))) ext)))

(defun eassist-find-if-exist (file)
  (if (file-exists-p file)
      (progn (find-file file) file)
    nil))

;; Finally, bind the key
(global-set-key "\C-q" 'eassist-switch-h-cpp)
