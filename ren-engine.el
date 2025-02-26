;;; ren-engine.el --- major-mode for Ren-Engine syntax

;;; Commentary:
;;;   Just highlighting

;;; License:
;;;   GPL-3

;;; Code:

(defvar ren-engine-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?_ "w" ren-engine-mode-syntax-table)  ; _ is part of word, not space-symbol
(modify-syntax-entry ?' "\"" ren-engine-mode-syntax-table) ; ' = "
(modify-syntax-entry ?# "<" ren-engine-mode-syntax-table)  ; # = start comment
(modify-syntax-entry ?\n ">" ren-engine-mode-syntax-table) ; \n = end comment

; set as space-symbols, not part of word:
(modify-syntax-entry ?& "-" ren-engine-mode-syntax-table)
(modify-syntax-entry ?* "-" ren-engine-mode-syntax-table)
(modify-syntax-entry ?+ "-" ren-engine-mode-syntax-table)
(modify-syntax-entry ?- "-" ren-engine-mode-syntax-table)
(modify-syntax-entry ?< "-" ren-engine-mode-syntax-table)
(modify-syntax-entry ?> "-" ren-engine-mode-syntax-table)


(defvar x-elements '(
"$"
"block"
"button"
"call"
;"circles"
;"clockwise"
"contains"
;"counterclockwise"
"elif"
"else"
"for"
"hbox"
"hide"
"hotspot"
"if"
"image"
"imagebutton"
"imagemap"
"jump"
"init"
"key"
"label"
"menu"
"null"
"nvl"
"parallel"
"pause"
"play"
"python"
"queue"
"repeat"
"scene"
"screen"
"show"
"stop"
"style"
"text"
"textbutton"
"transform"
"translate"
"vbox"
"while"
"window"
))

(defvar x-keywords '(
"at"
"behind"
"expression"
"fadein"
"fadeout"
"has"
;"knot"
"use"
"zorder"

"action"
"activate_sound"
"alternate"
"align"
"alpha"
"anchor"
"bold"
"clipping"
"color"
"corner_sizes"
"crop"
"delay"
"first_delay"
"font"
"ground"
"hover"
"hover_sound"
"hovered"
"italic"
;"justify"
"modal"
"mouse"
;"mousewheel"
"outlinecolor"
"pos"
"rotate"
"save"
"selected"
"size"
"size_min"
"size_max"
"skip_mouse"
"spacing"
"spacing_min"
"spacing_max"
"strikethrough"
"text_size"
"text_size_min"
"text_size_max"
"text_align"
"text_valign"
"underline"
"unhovered"
"xalign"
"xanchor"
"xpos"
"xsize"
"xsize_min"
"xsize_max"
"xzoom"
"yalign"
"yanchor"
"ypos"
"ysize"
"ysize_min"
"ysize_max"
"yzoom"
"zoom"
"hover_bold"
"hover_color"
"hover_font"
"hover_italic"
"hover_outlinecolor"
"hover_strikethrough"
"hover_text_size"
"hover_text_align"
"hover_text_valign"
"hover_underline"
))


(defvar x-python-keywords '(
"and" "as" "assert" "break" "class" "continue" "del" "def"
"except" "exec" "finally" "for" "from" "global" "import"
"in" "is" "lambda" "not" "or" "pass" "print" "raise"
"return" "try" "while" "with" "yield"
"self" "True" "False" "None"
))

(defvar x-python-exceptions '(
"ArithmeticError" "AssertionError" "AttributeError"
"BaseException" "DeprecationWarning" "EOFError"
"EnvironmentError" "Exception" "FloatingPointError"
"FutureWarning" "GeneratorExit" "IOError" "ImportError"
"ImportWarning" "IndentationError" "IndexError" "KeyError"
"KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
"NotImplemented" "NotImplementedError" "OSError"
"OverflowError" "PendingDeprecationWarning" "ReferenceError"
"RuntimeError" "RuntimeWarning" "StandardError"
"StopIteration" "SyntaxError" "SyntaxWarning" "SystemError"
"SystemExit" "TabError" "TypeError" "UnboundLocalError"
"UnicodeDecodeError" "UnicodeEncodeError" "UnicodeError"
"UnicodeTranslateError" "UnicodeWarning" "UserWarning"
"ValueError" "Warning" "ZeroDivisionError"
))

(defvar x-python-builtin '(
"abs" "absolute" "all" "any" "bool" "callable" "chr"
"classmethod" "compile" "complex" "delattr" "dict" "dir"
"divmod" "enumerate" "eval" "filter" "float" "frozenset"
"getattr" "globals" "hasattr" "hash" "hex" "id" "int"
"isinstance" "issubclass" "iter" "len" "list" "locals"
"map" "max" "min" "object" "oct" "open" "ord" "pow"
"property" "range" "repr" "reversed" "round" "set"
"setattr" "slice" "sorted" "staticmethod" "str" "sum"
"super" "tuple" "type" "vars" "zip"
))


;; generate regex string for each category of keywords
(defvar x-elements-regexp (regexp-opt x-elements 'words))
(defvar x-keywords-regexp (regexp-opt x-keywords 'words))
(defvar x-python-keywords-regexp (regexp-opt x-python-keywords 'words))
(defvar x-python-exceptions-regexp (regexp-opt x-python-exceptions 'words))
(defvar x-python-builtin-regexp (regexp-opt x-python-builtin 'words))


(defvar ren-engine-font-lock-keywords `(
    (,x-python-keywords-regexp . font-lock-builtin-face)
    (,x-python-exceptions-regexp . font-lock-constant-face)
    (,x-python-builtin-regexp . font-lock-function-name-face)
    (,x-elements-regexp . font-lock-type-face)
    (,x-keywords-regexp . font-lock-keyword-face)
    ;; note: order above matters, because once colored, that part won't change.
    ;; in general, put longer words first
))



;;;###autoload
(define-derived-mode ren-engine-mode prog-mode "ren-engine mode"
  "Major mode for editing .rpy (Ren-Engine)"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((ren-engine-font-lock-keywords)))

  ;; for comment processing in my-newline
  (setq comment-start "#")
  (comment-normalize-vars)
  
  
  (defun my-newline ()
	"Make newline as Ren-Engine code."
	(interactive)
	(let ((cur-indent (current-indentation)) cs)
	  (save-excursion
		(setq cs (comment-beginning))
		(when cs (goto-char cs))
		(skip-chars-backward " \t")
		(when (looking-back ":" 0)
		  (setq cur-indent (+ cur-indent tab-width))
		)
	  )
	  (insert "\n")
	  (indent-line-to cur-indent)
	)
  )
  (global-set-key (kbd "RET") 'my-newline)
  
  (defun my-tab ()
	"Insert a tab char. (ASCII 9, \t)."
	(interactive)
	(if (use-region-p)
		(let* ((start (region-beginning)) (end (region-end)) i)
		  (goto-char start)
		  (beginning-of-line)
		  (setq start (point))
		  
		  (goto-char end)
		  (skip-chars-forward " \t")
		  (setq end (point))
		  
		  (setq i start)
		  (goto-char i)
		  (while (<= i end)
			(skip-chars-forward " \t")
			(insert "\t")
			(setq end (1+ end))
			(forward-line)
			(if (< (point) (point-max)) ; moved?
				(setq i (point))
			  (setq i (1+ end)) ; break
			)
		  )
		  
		  (goto-char start)
		  (push-mark end)
		  (setq deactivate-mark nil)
		)
	  (progn (skip-chars-forward " \t") (insert "\t"))
	)
  )
  (global-set-key (kbd "TAB") 'my-tab)
  
  (defun my-untab ()
	"Remove a tab char. (ASCII 9, \t)."
	(interactive)
	(if (use-region-p)
		(let* ((start (region-beginning)) (end (region-end)) i)
		  (goto-char start)
		  (beginning-of-line)
		  (setq start (point))
		  
		  (goto-char end)
		  (skip-chars-forward " \t")
		  (setq end (point))
		  
		  (setq i start)
		  (goto-char i)
		  (while (<= i end)
		  	(skip-chars-forward " \t")
			(if (looking-back "\t" 0)
				(progn (delete-char -1) (setq end (- end 1)))
			  (when (looking-back "    " 0)
				(progn (delete-char -4) (setq end (- end 4)))
			  )
			)
			(forward-line)
			(if (< (point) (point-max)) ; moved?
				(setq i (point))
			  (setq i (1+ end)) ; break
			)
		  )
		  (goto-char start)
		  (push-mark end)
		  (setq deactivate-mark nil)
		)
	  (save-excursion
		(beginning-of-line)
		(skip-chars-forward " \t")
		(if (looking-back "\t" 0)
			(delete-char -1)
		  (when (looking-back "    " 0) (delete-char -4))
		)
	  )
	)
  )
  (global-set-key (kbd "<backtab>") 'my-untab)

  (defun my-start ()
	"Start Ren-Engine file <start.exe> (win) or <start.sh> (linux)"
	(interactive)
	(let* ((file-name (buffer-file-name)) (path (file-name-directory file-name)) exe-path prev-path)
	  (while path
		(setq win-path (concat path "start.exe"))
		(setq linux-path (concat path "start.sh"))
		(if (file-exists-p (concat path "resources")) ; in directory to start?
			(progn ; start
			  (if (or (eq system-type "windows-nt")
					  (eq system-type "cygwin"))
				  (setq exe-path (car (file-expand-wildcards (concat path "*.exe"))))
				(setq   exe-path (car (file-expand-wildcards (concat path "*.sh"))))
			  )
			  (start-process "Ren-Engine" "Ren-Engine output" exe-path)
			  (setq path nil) ; and break <while>
		    )
		  (progn ; else
			(setq prev-path path)
			(setq path (file-name-directory (directory-file-name path))) ; path = parent-directory of path
			(when (string= prev-path path)
			  (setq path nil)
			  (message "Ren-Engine directory <resources> not found")
			)
		  )
		)
	  )
	)
  )
  (global-set-key (kbd "<f5>") 'my-start)
  
  (defun my-show-output ()
	"Show output of finished Ren-Engine"
	(interactive)
	(switch-to-buffer "Ren-Engine output")
  )
  (global-set-key (kbd "<f6>") 'my-show-output)
  
  (set-syntax-table ren-engine-mode-syntax-table))


(add-to-list 'auto-mode-alist '("\\.rpy\\'" . ren-engine-mode))
(add-to-list 'same-window-buffer-names "*Ren-Engine*")

(provide 'ren-engine)

;;; ren-engine.el ends here
