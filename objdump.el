;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Objdump mode -- disassemble an object/executable and browse code.
;;
;; This code can be used in two ways: (1) M-x objdump on a specified
;; executable, object file, or library, where we run objdump directly
;; and process the results; (2) a saved disassembly with a mode line
;; telling Emacs to use objdump-mode for the file.
;;
;; I started this because I needed to be able to examine Linux kernel
;; and module code at the assembly level, based on stack traces dumped
;; out by the kernel.
;;
;; With an objdump-mode, one might want and reasonably expect to look
;; at data sections, strings, shared library dependencies, etc.;
;; "objdump" doesn't necessarily imply "disassemble", but the name
;; "disassemble" is already used for disassembling Emacs Lisp byte
;; code.

;; FIXME: Assumes 64-bit objects and 64-bit Emacs.

;; Possible enhancements:
;;  - more search input formats (see below at objdump-find-address)
;;  - hide/show function/file/line info
;;    - maybe shorten filename to basename only
;;  - pretty colors^W^Wfont-lock support?
;;  - which-function-mode
;;  - customize suffix handling
;;  - examine non-code sections?
;;  - DWARF debug info? (use readelf, pahole?) Gather info on variables
;;    defined at the current point in the function and their
;;    locations; global variable/function/type definitions; structure
;;    layouts (visualize with padding?); etc.
;;  - ?? other disassembly tools (Mac OS X otool, etc)
;;    - probably needs per-tool helper methods...
;;  - Search for references to a symbol, with name completion.
;;  - show sections, allow examining each as code/raw data/strings/etc
;;    - doesn't play nice with loading a saved disassembly listing
;;  - make objdump-revert retain current position
;;  - optionally invoke objdump-mode after find-file on .o
;;  - do/don't demangle C++ symbol names (affects symbol-name syntax)
;;  - make a mode suitable for auto-mode-alist
;;  - click/RET on symbol name in reference to jump to definition
;;  - click/RET to get source file if available
;;  - cross-platform disassembly, e.g., 32-bit ARM on 64-bit x86 host,
;;    or 64-bit target on 32-bit host; cygwin target, unix host
;;  - Maybe patch instructions with reloc info, so a call doesn't look
;;    like (on x86) callq to the immediately-following-address +
;;    R_X86_64_PC32 reloc to foo-4.
;;  - Now that objdump-symbol-table has been added, store addresses in
;;    those symbols instead of always searching for the symbol name
;;    again and re-parsing the text of the address.
;;  - Use objdump or nm to get the whole symbol table, including names
;;    that may not show up in disassembly (e.g., because two names map
;;    to the same location).
;;
;; Would it be easier to talk to a GDB subprocess to do some of this
;; work somehow?
;;
;; There should be other code to call out to for hex/bignum processing.

(defvar objdump-file-name nil
  "Name of object file currently being examined with objdump-mode, if any.")
(make-variable-buffer-local 'objdump-file-name)
(defvar objdump-symbol-table nil
  "...")
(make-variable-buffer-local 'objdump-symbol-table)

(defcustom objdump-command "objdump"
  "Command to run to disassemble object file"
  :group 'objdump)

;;;###autoload
(define-derived-mode objdump-mode text-mode "Objdump"
  "Major mode for viewing object file disassembly."
  (setq buffer-read-only t))

;; Helpers

;; XXX For handling 64-bit objects in a 32-bit Emacs, this should
;; probably generate a list of numbers using only 16 or 24 bits in
;; each number.  Unless we get bignum support.
;;
;; Actually, a 64-bit Emacs still can't describe a full 64-bit address
;; range with integers. For now, we take advantage of the fact that
;; x86_64 addresses are actually constrained to 48 bits, sign
;; extended.
(defun convert-64bit-to-number (string)
  (if (and (eq (length string) 16)
	   (string-match "^ff" string))
      (- (string-to-number (substring string 2) 16)
	 (expt 2 56))
    (string-to-number string 16)))

(defun convert-number-to-64bit (number)
  (if (< number 0)
      (format "ff%14x" (+ number (expt 2 56)))
    (format "%x" number)))

(defun objdump--get-symbols ()
  (or objdump-symbol-table
      (progn
	(setq objdump-symbol-table (make-vector 300 nil))
	(save-excursion
	  (save-match-data
	    (goto-char (point-min))
	    (while (re-search-forward "^[0-9a-f]+ <\\([a-zA-Z_0-9:.]+\\)>:$"
				      nil t)
	      (intern (match-string 1) objdump-symbol-table))))
	objdump-symbol-table)))

(defun objdump--read-address (prompt)
  (completing-read prompt (objdump--get-symbols) nil))

;; Read an address and find it in the disassembly.
;;
;; Supported formats:
;;  - symname
;;  - symname+0x123  (Linux kernel stack trace)
;; Should add:
;;  - whatever gdb emits
;;  - GNU libc backtrace format
;;  - 123 (absolute; leading 0x optional; maybe leading ffff... optional?)
(defun objdump-find-address (expr)
  "Find an address in a buffer containing an objdump disassembly.

EXPR is either a \"symbolname+0x123\" style sum, as displayed in
a Linux kernel stack trace or, just \"symbolname\"."
  (interactive (list (objdump--read-address "Address to find: ")))
  (save-match-data
    (cond ((string-match "^\\([a-zA-Z0-9_.]+\\)\\+0x\\([0-9a-fA-F]+\\)$" expr)
	   (let* ((symbol-string (match-string 1 expr))
		  (offset-string (match-string 2 expr))
		  (offset (convert-64bit-to-number offset-string))
		  symbol-value address-string)
	     (save-excursion
	       (goto-char (point-min))
	       (if (search-forward-regexp (concat "^\\([0-9a-f]+\\) <"
						  symbol-string
						  ">:$")
					  nil t)
		   (setq symbol-value (convert-64bit-to-number (match-string 1)))
		 (error "Symbol %s not found" symbol-string)))
	     (setq address-string (convert-number-to-64bit (+ offset symbol-value)))
	     (goto-char (save-excursion
			  (goto-char (point-min))
			  (if (search-forward-regexp (concat "^ *"
							     address-string
							     ":\t") nil t)
			      (match-beginning 0)
			    (error "Address %s not found" address-string))))
	     (message "%s(0x%s) + 0x%x = 0x%s"
		      symbol-string
		      (convert-number-to-64bit symbol-value)
		      offset address-string)
	     ))
	  ((string-match "^[a-zA-Z0-9_.]+$" expr)
	   (goto-char (save-excursion
			(goto-char (point-min))
			(if (search-forward-regexp (concat "^\\([0-9a-f]+\\) <"
							   expr
							   ">:$")
						   nil t)
			    (match-beginning 0)
			  (error "Symbol %s not found" expr)))))
	  (t
	   (error "Couldn't parse %S" expr)))))

;; FIXME: If there's no objdump-file-name but the buffer has a real
;; file associated, revert from that file.
(defun objdump-revert ()
  "Rerun objdump on the (presumably changed) object file."
  (interactive)
  (if (not objdump-file-name)
      (error "No defined object file name for this buffer"))
  ;; XXX hack
  ;; Should retain position if possible, a la revert-file.
  (objdump objdump-file-name))

(define-key objdump-mode-map "s" 'objdump-find-address)
(define-key objdump-mode-map "g" 'objdump-revert)

;; Normally completion-ignored-extensions will rule out some of the
;; very files we want to be looking for in this case.
(defvar objdump-extensions
  '(".o"				; compiled object file
    ".so"				; shared library
    ".a"				; archive library
    ".ko"				; Linux kernel objects
    ;".dylib"				; Mac OS X libraries
    ;".dll"				; etc
    ;".obj"				;
    )
  "Extensions typically indicating object files we should disassemble")

(defun get-completion-ignored-extensions-for-objects ()
  (let ((extensions (apply 'list completion-ignored-extensions)))
    (mapc (lambda (e)
	    (setq extensions (delete e extensions)))
	  objdump-extensions)
    (append (list ".c" ".s" ".h" ".cc") extensions)))

;; Usually, this is the function that starts everything off.
;;;###autoload
(defun objdump (filename)
  "Run objdump to disassemble an object file, and invoke objdump-mode."
  (interactive (let ((completion-ignored-extensions (get-completion-ignored-extensions-for-objects)))
		 (list (read-file-name "Object file to disassemble: "
				       nil nil t))))
  (let ((output-buffer (get-buffer-create (concat "*Objdump " filename "*")))
	(command (concat objdump-command " -dCSlr " filename)))
    (with-current-buffer output-buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    (message "Running %s ..." command)
    (shell-command command output-buffer)
    (with-current-buffer output-buffer
      (objdump-mode)
      (setq objdump-file-name filename))
    (message "Running %s ... done" command)
    ))

;; No! This doesn't do disassembly; it assumes it's already a disassembly.
;;
;; doc-view-mode might be a better model for handling find-file on
;; object files.
;;
;; Perhaps objdump-mode should be a mode for examining object files,
;; and some other objdump-view-mode should be for looking at a text
;; file containing the disassembly output? Or object-file-mode and
;; objdump-mode, or something.
;
;(mapc (lambda (suffix)
;	(add-to-list 'auto-mode-alist
;		     (cons (concat "\\" suffix "\\'") 'objdump-mode)))
;      objdump-extensions)

(provide 'objdump)
