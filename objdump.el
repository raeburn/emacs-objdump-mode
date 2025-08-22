;;; objdump.el --- Disassemble and browse code -*- lexical-binding: t -*-

;; Author: Laluxx

;;; Commentary:

;; Keywords: tools

;;; Code:

(require 'cl-lib)
(require 'hexl)
(require 'marginalia)

;;; Faces

(defgroup objdump nil
  "Major mode for viewing object file disassembly."
  :group 'tools)

(defface objdump-address-face
  '((t :inherit font-lock-constant-face))
  "Face for memory addresses."
  :group 'objdump-faces)

(defface objdump-symbol-face
  '((t :inherit font-lock-function-name-face))
  "Face for symbol names."
  :group 'objdump-faces)

(defface objdump-instruction-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for assembly instructions."
  :group 'objdump-faces)

(defface objdump-register-face
  '((t :inherit font-lock-variable-name-face))
  "Face for CPU registers."
  :group 'objdump-faces)

(defface objdump-immediate-face
  '((t :inherit font-lock-constant-face))
  "Face for immediate values."
  :group 'objdump-faces)

(defface objdump-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments and file info."
  :group 'objdump-faces)

;;; Variables

(defvar objdump-file-name nil
  "Name of object file currently being examined with objdump-mode, if any.")
(make-variable-buffer-local 'objdump-file-name)

(defvar objdump-binary-buffer nil
  "Buffer containing the binary file in 'hexl-mode'.")
(make-variable-buffer-local 'objdump-binary-buffer)

(defvar objdump-symbol-table nil
  "Symbol table for current buffer.")
(make-variable-buffer-local 'objdump-symbol-table)

(defcustom objdump-command "objdump"
  "Command to run to disassemble object file."
  :type 'string
  :group 'objdump)

;;; Dired integration

(require 'dired-aux)
(defun dired-find-file-other-window-or-objdump ()
  "In Dired, open file in other window or show objdump for executables."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (and (file-executable-p file)
             (not (file-directory-p file)))
        (objdump file)
      (dired-find-file-other-window))))

(defun dired-find-file-or-objdump ()
  "In Dired, open file or show objdump for executables."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (and (file-executable-p file)
             (not (file-directory-p file)))
        (progn
          (objdump file)
          (delete-window))  ; Delete the extra window after opening objdump
      (dired-find-file))))

(defvar objdump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'imenu)
    (define-key map "a" 'objdump-goto-address)
    (define-key map "i" 'imenu)
    (define-key map "g" 'objdump-revert)
    (define-key map "p" 'objdump-previous-function)
    (define-key map "n" 'objdump-next-function)
    (define-key map "q" 'kill-this-buffer)
    (define-key map (kbd "C-n") 'objdump-next-line)
    (define-key map (kbd "C-p") 'objdump-previous-line)
    (define-key map (kbd "C-f") 'objdump-forward-byte)
    (define-key map (kbd "C-b") 'objdump-backward-byte)
    (define-key map (kbd "C-a") 'objdump-move-beginning-of-line)
    (define-key map (kbd "C-e") 'objdump-move-end-of-line)
    (define-key map (kbd "RET") 'objdump-visit-address-in-hexl)
    (define-key map (kbd "C-j") 'objdump-visit-address-in-hexl)
    (define-key dired-mode-map (kbd "o") 'dired-find-file-other-window-or-objdump)
    (define-key dired-mode-map (kbd "RET") 'dired-find-file-or-objdump)
    map)
  "Keymap for `objdump-mode'.")

(defun objdump-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a function.
With ARG, do it that many times.  Negative arg -N means move forward to
Nth following beginning of function."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((found t)
        (pos (point)))
    (if (< arg 0)
        ;; Moving forward
        (dotimes (_ (- arg) found)
          (end-of-line)
          (when (re-search-forward "^[0-9a-f]+ <[^>]+>:$" nil t)
            (beginning-of-line))
          (setq found (not (= pos (point)))))
      ;; Moving backward
      (dotimes (_ arg found)
        (unless (looking-at "^[0-9a-f]+ <[^>]+>:$")
          (end-of-line)
          (re-search-backward "^[0-9a-f]+ <[^>]+>:$" nil t))
        (setq found (not (= pos (point))))))
    found))

(defun objdump-end-of-defun (&optional arg)
  "Move forward to next end of function.
With ARG, do it that many times.  Negative argument -N means move
back to Nth preceding end of function."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((start-pos (point))
        (found t))
    ;; If we're not looking at the start of a function, move to one
    (unless (looking-at "^[0-9a-f]+ <[^>]+>:$")
      (objdump-beginning-of-defun 1))
    
    ;; Now find the end
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at "^[0-9a-f]+ <[^>]+>:$"))  ; next function
                (not (looking-at "^\\s-*$"))               ; blank line
                (not (looking-at "^Disassembly of")))      ; section header
      (forward-line 1))
    
    (setq found (not (= start-pos (point))))
    
    ;; If arg > 1, do it again
    (when (and found (> arg 1))
      (setq found (objdump-end-of-defun (1- arg))))
    found))

(defun objdump-mode-setup-defun ()
  "Set up function navigation for objdump-mode."
  (setq-local beginning-of-defun-function #'objdump-beginning-of-defun)
  (setq-local end-of-defun-function #'objdump-end-of-defun))

(add-hook 'objdump-mode-hook #'objdump-mode-setup-defun)


(defun objdump-get-address-at-point ()
  "Extract the hexadecimal address from the current line in objdump output.
Returns nil if no address is found."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^ *\\([0-9a-f]+\\):")
      (string-to-number (match-string 1) 16))))

(defun objdump-ensure-hexl-buffer ()
  "Ensure we have a 'hexl-mode' buffer for the binary file.
Returns the buffer or nil if the binary file cannot be found."
  (unless (and objdump-binary-buffer
               (buffer-live-p objdump-binary-buffer))
    (when (and objdump-file-name
               (file-exists-p objdump-file-name))  ; Add check for file existence
      (let ((buf (find-file-noselect objdump-file-name)))
        (with-current-buffer buf
          (unless (eq major-mode 'hexl-mode)
            (hexl-mode))
          (setq objdump-binary-buffer buf)))))
  objdump-binary-buffer)

(defvar-local objdump-hexl-window-shrunk nil
  "Flag indicating whether the hexl window has been shrunk.")

(defun objdump-get-byte-offset-in-line ()
  "Get the byte offset from the start of the line based on point position.
Returns nil if not on a hex byte."
  (when-let* ((range (objdump--get-hex-range))
              (start (car range))
              (end (cdr range))
              (point-in-range (and (>= (point) start) (< (point) end))))
    (let ((byte-count 0))
      (save-excursion
        (goto-char start)
        (while (< (point) (min (point) end))
          (when (looking-at "[0-9a-f]")
            (forward-char)
            (when (looking-at "[0-9a-f]")
              (setq byte-count (1+ byte-count)))
            (forward-char))
          (skip-chars-forward " ")))
      byte-count)))

(defvar-local objdump-hexl-window-shrunk nil
  "Flag indicating whether the hexl window has been shrunk.")

(defun objdump-visit-address-in-hexl ()
  "Visit the address from current objdump line in a 'hexl-mode' buffer."
  (interactive)
  (let ((addr (objdump-get-address-at-point))
        (hexl-buf-name (file-name-nondirectory objdump-file-name)))
    (unless addr
      (user-error "No valid address found on current line"))
    (unless objdump-file-name
      (user-error "No binary file path stored"))
    (unless (file-exists-p objdump-file-name)
      (user-error "Binary file %s not found" objdump-file-name))
    
    ;; Find or create the buffer
    (find-file-other-window objdump-file-name)
    (unless (eq major-mode 'hexl-mode)
      (hexl-mode))
    (unless objdump-hexl-window-shrunk
      (shrink-window-horizontally 27)
      (setq objdump-hexl-window-shrunk t))
    (hexl-goto-address addr)))

(defvar objdump-extensions
  '(".o"                                ; compiled object file
    ".so"                               ; shared library
    ".a"                                ; archive library
    ".ko"                               ; Linux kernel objects
                                        ;".dylib"                           ; Mac OS X libraries
                                        ;".dll"                             ; etc
                                        ;".obj"                             ; 
    )
  "Extensions typically indicating object files we should disassemble.")

(defvar objdump-font-lock-keywords
  `(
    ;; Addresses at start of line
    ("^ *\\([0-9a-f]+\\):" 1 'objdump-address-face)
    
    ;; Symbol definitions and references
    ("^[0-9a-f]+ <\\([^>]+\\)>:$" 1 'objdump-symbol-face)
    ("<\\([^>]+\\)>" 1 'objdump-symbol-face)
    
    ;; Instructions - note the pattern now requires a tab and whitespace
    ("\t[0-9a-f ]+\t\\([a-z][a-z0-9.]*\\)" 1 'objdump-instruction-face)
    
    ;; x86/x86_64 registers
    (,(concat "\\b\\(%[a-z][a-z0-9]*\\|[re][abcd]x\\|[re]sp\\|[re]bp\\|"
              "[re]si\\|[re]di\\|[re]ip\\|r[0-9]+\\|[xyz]mm[0-9]+\\)\\b")
     . 'objdump-register-face)
    
    ;; Immediate values
    ("\\b\\(\\$?-?0x[0-9a-fA-F]+\\|\\$[0-9]+\\)\\b" . 'objdump-immediate-face)
    
    ;; Comments and file info
    ("\\(#.*\\|File .*\\|\\.?\\.?L[A-Za-z0-9]*:\\)" . 'objdump-comment-face))
  "Syntax highlighting rules for objdump mode.")

;;; Helpers for address conversion

(defun convert-64bit-to-number (string)
  "Convert 64-bit hex STRING to number, handling sign extension."
  (if (and (eq (length string) 16)
           (string-match "^ff" string))
      (- (string-to-number (substring string 2) 16)
         (expt 2 56))
    (string-to-number string 16)))

(defun convert-number-to-64bit (number)
  "Convert NUMBER to 64-bit hex string, handling sign extension."
  (if (< number 0)
      (format "ff%14x" (+ number (expt 2 56)))
    (format "%x" number)))

;;; Symbol table management

(defun objdump--get-symbols ()
  "Get or build symbol table for current buffer."
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
  "Read an address with completion using PROMPT."
  (completing-read prompt (objdump--get-symbols) nil))

;;; Interactive commands

;;; ADDRESS

(defun objdump--collect-addresses ()
  "Collect all addresses from the current objdump buffer.
Returns an alist of (address . properties) pairs."
  (let ((addresses '())
        (max-addr-len 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\([0-9a-f]+\\):\\s-+\\([0-9a-f ]+\\)?\\(?:\t\\(.+\\)\\)?" nil t)
        (let* ((addr (match-string 1))
               (hex-bytes (or (match-string 2) ""))
               (instruction (or (match-string 3) ""))
               (marker (point-marker)))
          (setq max-addr-len (max max-addr-len (length addr)))
          ;; Store address with its context
          (push (list addr hex-bytes instruction marker) addresses))))
    (setq-local objdump--longest-addr-length (+ 2 max-addr-len)) ; +2 for "0x" prefix
    (nreverse addresses)))

(defun objdump--format-address-candidate (addr-info)
  "Format an address candidate for completion display.
ADDR-INFO is (addr hex-bytes instruction marker)."
  (let* ((addr (nth 0 addr-info))
         (completion-text (format "0x%s" addr)))
    ;; Store full info as text properties for marginalia
    (propertize completion-text
                'addr addr
                'hex-bytes (nth 1 addr-info)
                'instruction (nth 2 addr-info)
                'marker (nth 3 addr-info))))

(defun objdump-address-completion-annotator (cand)
  "Annotate address CAND with instruction info for marginalia."
  (let* ((hex-bytes (get-text-property 0 'hex-bytes cand))
         (instruction (get-text-property 0 'instruction cand))
         (addr-len (length cand))
         ;; Calculate padding to align columns
         (addr-padding (make-string 
                        (max 0 (- objdump--longest-addr-length addr-len))
                        ?\s)))
    (concat
     addr-padding
     "  "  ; Space after address
     (when hex-bytes
       (concat
        (propertize hex-bytes
                    'face 'font-lock-comment-face)
        "\t"))
     (when instruction
       ;; Apply syntax highlighting to the instruction
       (with-temp-buffer
         (insert instruction)
         (delay-mode-hooks
           (objdump-mode)
           (font-lock-ensure)
           (buffer-string)))))))

  (defun objdump-goto-raw-address (addr)
    "Go to an address in the objdump buffer."
    (let* ((clean-addr (replace-regexp-in-string "^0x" "" addr))
           (regexp (format "^\\s-*%s:" clean-addr)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward regexp nil t)
            (progn
              (goto-char (match-beginning 0))
              (set-window-point (selected-window) (point))
              (recenter))
          (message "Address %s not found" addr)))))

  (defun objdump-goto-address-at-point ()
    "Jump to the address referenced at point."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (when (or (looking-at "^\\s-*\\([0-9a-f]+\\):")               ; Direct address
                (looking-at ".*\\(0x[0-9a-f]+\\)"))                  ; Reference in instruction
        (let ((addr (match-string 1)))
          (objdump-goto-raw-address addr)))))

  (defun objdump-goto-address ()
    "Jump to any address in the objdump buffer using completion."
    (interactive)
    (let* ((addresses (objdump--collect-addresses))
           (candidates (mapcar #'objdump--format-address-candidate addresses)))
      ;; Register the annotator for this completion session
      (add-hook 'marginalia-annotator-registry
                (list 'objdump-address-completion
                      'objdump-address-completion-annotator
                      'marginalia-annotate-binding))
      (unwind-protect
          (let* ((completion-category-defaults
                  '((objdump-address-completion
                     (styles basic partial-completion))))
                 (completion-category-overrides
                  '((objdump-address-completion
                     (styles basic partial-completion))))
                 (completion (completing-read "Go to address: "
                                              (lambda (str pred action)
                                                (if (eq action 'metadata)
                                                    '(metadata
                                                      (category . objdump-address-completion))
                                                  (complete-with-action
                                                   action candidates str pred))))))
            (when-let ((marker (get-text-property 0 'marker completion)))
              (goto-char marker)
              (recenter)))
        ;; Clean up the annotator
        (setq marginalia-annotator-registry
              (assq-delete-all 'objdump-address-completion
                               marginalia-annotator-registry)))))

  
(defun objdump-revert ()
  "Rerun objdump on the (presumably changed) object file."
  (interactive)
  (if (not objdump-file-name)
      (error "No defined object file name for this buffer"))
  (let ((pos (point)))
    (objdump objdump-file-name)
    (goto-char pos)))

(defun get-completion-ignored-extensions-for-objects ()
  "Get completion ignored extensions, excluding object file extensions."
  (let ((extensions (apply 'list completion-ignored-extensions)))
    (mapc (lambda (e)
            (setq extensions (delete e extensions)))
          objdump-extensions)
    (append (list ".c" ".s" ".h" ".cc") extensions)))

;; Movement Functions

(defun objdump-next-function ()
  "Move to the start of the next function in objdump output."
  (interactive)
  (let ((old-point (point)))
    (end-of-line)
    (if (re-search-forward "^[0-9a-f]+ <[^>]+>:$" nil t)
        (progn
          (goto-char (line-beginning-position))
          (recenter))
      (goto-char old-point)
      (message "No more functions"))))

(defun objdump-previous-function ()
  "Move to the start of the previous function in objdump output."
  (interactive)
  (let ((old-point (point)))
    (beginning-of-line)
    (if (re-search-backward "^[0-9a-f]+ <[^>]+>:$" nil t)
        (progn
          (goto-char (line-beginning-position))
          (recenter))
      (goto-char old-point)
      (message "No previous functions"))))

(defun objdump--find-nearest-hex-position (target-column)
  "Find nearest hex position to TARGET-COLUMN in current line.
Returns point position of nearest hex digit, or nil if none found."
  (when-let* ((range (objdump--get-hex-range))
              (start (car range))
              (end (cdr range)))
    (save-excursion
      ;; Get column positions of all hex digits
      (let ((positions '())
            (min-diff nil)
            (best-pos nil))
        (goto-char start)
        (while (< (point) end)
          (when (looking-at "[0-9a-f]")
            (let* ((cur-col (current-column))
                   (diff (abs (- cur-col target-column))))
              (when (or (null min-diff) (<= diff min-diff))
                (setq min-diff diff)
                (setq best-pos (point)))))
          (forward-char))
        best-pos))))

(defun objdump-forward-byte ()
  "Smart forward movement through hex bytes."
  (interactive)
  (when-let ((range (objdump--get-hex-range)))
    (let ((hex-end (cdr range)))
      (cond
       ;; On first digit of a pair, move to second digit
       ((and (< (point) hex-end)
             (looking-at "[0-9a-f]")
             (not (looking-back "[0-9a-f]" (1- (point)))))
        (forward-char))
       
       ;; On second digit or space, move to next pair or wrap
       ((< (point) hex-end)
        (forward-char)
        (skip-chars-forward " ")
        (when (>= (point) hex-end)
          ;; At end of line, try to wrap
          (when (objdump--next-hex-line)
            (beginning-of-line)
            (when (looking-at "^\\s-*[0-9a-f]+:\\s-+")
              (goto-char (match-end 0))))))))))

(defun objdump-backward-byte ()
  "Move backward through hex bytes one character at a time, with wrapping."
  (interactive)
  (when-let ((range (objdump--get-hex-range)))
    (let ((hex-start (car range)))
      (cond
       ;; Case 1: We're after hex-start, handle normal backward movement
       ((> (point) hex-start)
        (backward-char)
        ;; If we landed on whitespace, skip back to previous hex digit
        (when (looking-at "\\s-")
          (skip-chars-backward " ")
          (when (looking-back "[0-9a-f]" (1- (point)))
            (backward-char))))
       
       ;; Case 2: We're at the start of hex range, need to wrap to previous line
       (t
        (when (save-excursion
                (forward-line -1)
                (objdump--line-has-hex-p))
          (forward-line -1)
          (when-let* ((prev-range (objdump--get-hex-range))
                      (prev-end (cdr prev-range)))
            ;; Go to last hex digit of previous line
            (goto-char prev-end)
            (backward-char)  ; Move off potential whitespace
            (while (and (> (point) (car prev-range))
                        (not (looking-at "[0-9a-f]")))
              (backward-char)))))))))


(defun objdump--next-hex-line ()
  "Move to next line with hex bytes, preserving column position if possible."
  (let ((target-column (current-column)))
    (forward-line)
    (while (and (not (eobp))
                (not (objdump--line-has-hex-p)))
      (forward-line))
    (when (objdump--line-has-hex-p)
      (when-let ((pos (objdump--find-nearest-hex-position target-column)))
        (goto-char pos)
        t))))

(defun objdump--prev-hex-line ()
  "Move to previous line with hex bytes, preserving column position if possible."
  (let ((target-column (current-column)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (objdump--line-has-hex-p)))
      (forward-line -1))
    (when (objdump--line-has-hex-p)
      (when-let ((pos (objdump--find-nearest-hex-position target-column)))
        (goto-char pos)
        t))))

(defun objdump-next-line ()
  "Move to next line preserving column when possible."
  (interactive)
  (let ((target-column (current-column)))
    (if (objdump--next-hex-line)
        t  ; Column already preserved by next-hex-line
      ;; Try to find next function
      (when (re-search-forward "^[0-9a-f]+ <.*>:$" nil t)
        (forward-line)
        (when (objdump--line-has-hex-p)
          (beginning-of-line)
          (when (looking-at "^\\s-*[0-9a-f]+:\\s-+")
            (goto-char (match-end 0))))))))

(defun objdump-previous-line ()
  "Move to previous line preserving column when possible."
  (interactive)
  (let ((target-column (current-column)))
    (if (objdump--prev-hex-line)
        t  ; Column already preserved by prev-hex-line
      ;; Try to find previous function
      (when (re-search-backward "^[0-9a-f]+ <.*>:$" nil t)
        (forward-line)
        (when (objdump--line-has-hex-p)
          (beginning-of-line)
          (when (looking-at "^\\s-*[0-9a-f]+:\\s-+")
            (goto-char (match-end 0))))))))

(defun objdump-move-beginning-of-line ()
  "Move to first hex character of the line."
  (interactive)
  (when-let ((range (objdump--get-hex-range)))
    (goto-char (car range))))

(defun objdump-move-end-of-line ()
  "Move to last hex character of the line."
  (interactive)
  (when-let ((range (objdump--get-hex-range)))
    (goto-char (cdr range))
    (backward-char)
    (while (and (> (point) (car range))
                (not (looking-at "[0-9a-f]")))
      (backward-char))))

;; Imenu support

(defgroup objdump-completion nil
  "Completion settings for objdump mode."
  :group 'objdump)

(defface objdump-completion-address
  '((t :inherit marginalia-documentation))
  "Face for objdump addresses in completion annotations."
  :group 'objdump-completion)

(defface objdump-completion-size
  '((t :inherit marginalia-size :weight bold))
  "Face for function size annotations."
  :group 'objdump-completion)

(defvar-local objdump--longest-symbol-length 0
  "Length of longest symbol name in current buffer.")

(defvar-local objdump--longest-addr-length 0
  "Length of longest address in current buffer.")

(defun objdump--compute-function-size (start-addr next-addr)
  "Compute function size from START-ADDR to NEXT-ADDR."
  (when (and start-addr next-addr)
    (- (string-to-number next-addr 16)
       (string-to-number start-addr 16))))

(defun objdump-imenu-create-index ()
  "Create imenu index for objdump buffer."
  (let ((index-alist '())
        (max-len 0)
        (max-addr-len 0)
        (prev-addr nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9a-f]+\\) <\\([^>]+\\)>:$" nil t)
        (let* ((addr (match-string 1))
               (name (match-string 2))
               (name-len (length name))
               (addr-len (+ 2 (length addr))) ; +2 for "0x" prefix
               (size (when prev-addr 
                       (objdump--compute-function-size prev-addr addr)))
               (location (point-marker)))
          (setq max-len (max max-len name-len)
                max-addr-len (max max-addr-len addr-len))
          (let ((completion-item name))
            (put-text-property 0 (length completion-item) 
                               'objdump-address addr completion-item)
            (when size
              (put-text-property 0 (length completion-item) 
                                 'objdump-size size completion-item))
            (push (cons completion-item location) index-alist))
          (setq prev-addr addr))))
    (setq objdump--longest-symbol-length (+ max-len 2)
          objdump--longest-addr-length (+ max-addr-len 2))
    (nreverse index-alist)))

(defun objdump-completion-annotator (cand)
  "Annotate imenu CAND with address and size info for marginalia."
  (when-let ((addr (get-text-property 0 'objdump-address cand)))
    (let* ((size (get-text-property 0 'objdump-size cand))
           (addr-str (format "0x%s" addr))
           (addr-padding (make-string 
                          (max 0 (- objdump--longest-addr-length (length addr-str))) 
                          ?\s)))
      (concat
       (make-string (max 0 (- objdump--longest-symbol-length (length cand))) ?\s)
       (propertize addr-str 'face 'objdump-completion-address)
       addr-padding
       "  "  ; Two spaces after address
       (when size
         (propertize (format "%d" size) 
                     'face 'objdump-completion-size))))))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(imenu objdump-completion-annotator marginalia-annotate-binding)))



(defun objdump--line-has-hex-p ()
  "Return t if current line has hex instruction bytes."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*[0-9a-f]+:\\s-+[0-9a-f]")))

(defun objdump--get-hex-range ()
  "Get the start and end positions of hex bytes on current line.
Returns (start . end) positions, or nil if not on a hex line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\s-*[0-9a-f]+:\\s-+\\([0-9a-f ]\\{2,\\}\\)\\s-+\\S-")
      (cons (match-beginning 1) (match-end 1)))))


;; Update keymap

;;;###autoload
(define-derived-mode objdump-mode text-mode "Objdump"
  "Major mode for viewing object file disassembly.
\\{objdump-mode-map}"
  (setq buffer-read-only t)
  (setq font-lock-defaults '(objdump-font-lock-keywords))
  (setq truncate-lines t)
  (setq imenu-create-index-function #'objdump-imenu-create-index)
  ;; Advice imenu to recenter after jumping
  (advice-add 'imenu :after
              (lambda (&rest _)
                (beginning-of-line)
                (recenter 0))))

;;;###autoload
(defun objdump (filename)
  "Run objdump to disassemble an object file, and invoke objdump-mode."
  (interactive
   (let ((completion-ignored-extensions
          (get-completion-ignored-extensions-for-objects)))
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
    (message "%s... %s" command 
             (propertize "DONE" 'face '(:inherit success :weight bold)))))



(provide 'objdump)
;;; objdump.el ends here
