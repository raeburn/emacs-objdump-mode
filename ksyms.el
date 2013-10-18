;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Linux kernel stack info printed out by the kmemleak support after a
;; module has been unloaded may include addresses with no symbolic
;; form. Use a previously saved /proc/kallsyms dump to fix it.
;;
;; (ksyms-parse) - parses the buffer (or narrowed region thereof)
;; looking for lines that look like:
;;
;;   ffffffff8135380e t foo_function_name
;;
;; ...and returns a sorted list of
;;
;;   (symbol addr-high-32-bits . addr-low-32-bits)
;;
;; (update-symbols-in-stack-trace SYMBOL-LIST) goes through the buffer
;; looking for 16-digit hex value prefixed with " 0x" and replaces
;; them with the best function name (less than or equal to the value,
;; but not more than ksyms-maximum-offset less) plus offset.

;; This implementation assumes a 64-bit Emacs and a 64-bit kernel.

(require 'objdump) ;; for address math

(defun ksyms--compare-address (addr-a addr-b)
  (if (= (car addr-a) (car addr-b))
      (< (cdr addr-a) (cdr addr-b))
    (< (car addr-a) (car addr-b))))

(defun ksyms--compare (entry-a entry-b)
  (let ((addr-a (cdr entry-a))
	(addr-b (cdr entry-b)))
    (ksyms--compare-address addr-a addr-b)))

(defun ksyms--subtract (addr-a addr-b)
  (let ((result (cons (- (car addr-a) (car addr-b))
		      (- (cdr addr-a) (cdr addr-b)))))
    (if (< (cdr result) 0)
	(progn
	  (setcar result (1- (car result)))
	  (setcdr result (+ #x100000000 (cdr result)))))
    result))

(defun ksyms--find-closest-symbol (sorted-symbol-list address)
  (while (and (cdr sorted-symbol-list)
	      (let* ((next-entry
		      (cadr sorted-symbol-list))
		     (next-addr (cdr next-entry)))
		(or (equal next-addr address)
		    (ksyms--compare-address next-addr
					    address))))
    (setq sorted-symbol-list
	  (cdr sorted-symbol-list)))
  (car sorted-symbol-list))

(defun ksyms--offset-to-number (offset-pair)
  (+ (* #x100000000 (car offset-pair))
     (cdr offset-pair)))

(defun ksyms--offset-to-string (offset-pair)
  (let ((offset (ksyms--offset-to-number offset-pair)))
    (if (= 0 offset)
	""
      (format "+0x%x" offset))))

(defvar ksyms-maximum-offset 4000)

;; return (SYMBOL-NAME . OFFSET)
(defun ksyms--find-match (sorted-symbol-list address)
  (let* ((matching-symbol (ksyms--find-closest-symbol sorted-symbol-list
						      address))
	 (offset (ksyms--subtract address (cdr matching-symbol))))
    (and (<= (ksyms--offset-to-number offset) ksyms-maximum-offset)
	 (cons (car matching-symbol) offset))))

(defun ksyms--parse-address (address-string)
  (if (and (> (length address-string) 2)
	   (string= (substring address-string 0 2) "0x"))
      (setq address-string (substring address-string 2)))
  (let ((len (length address-string)))
    (if (> len 8)
	(let ((value-upper (string-to-number
			    (substring address-string 0 (- len 8))
			    16))
	      (value-lower (string-to-number
			    (substring address-string (- len 8))
			    16)))
	  (cons value-upper value-lower))
      (let ((value (string-to-number address-string 16)))
	(cons 0 value)))))

(defun ksyms-parse ()
  (let ((symbol-list nil))
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(while (re-search-forward "^\\([0-9a-f]\\{16\\}\\) [tT] \\([a-zA-Z_0-9.]+\\)[\t]*\\(\\[.+\\]\\)?$"
				  nil t)
	  (let* ((symbol-name (match-string-no-properties 2))
		 (symbol-value-string (match-string 1))
		 (symbol-value-upper (string-to-number
				      (substring symbol-value-string 0 8)
				      16))
		 (symbol-value-lower (string-to-number
				      (substring symbol-value-string 8 16)
				      16))
		 )
	    (push (cons symbol-name
			(cons symbol-value-upper
			      symbol-value-lower))
		  symbol-list)))))
    (sort symbol-list 'ksyms--compare)))

(defun update-symbols-in-stack-trace (symbol-list)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward " 0x\\([0-9a-f]\\{16\\}\\)" nil t)
	(let* ((address (ksyms--parse-address (match-string 1)))
	       (match (ksyms--find-match symbol-list address)))
	  (if match
	      (let ((new-string (concat " "
					(car match)
					(ksyms--offset-to-string (cdr match)))))
		(replace-match new-string nil nil)
		;;(message "would replace-match 0x%S with %S"
		;;	 (match-string 1) new-string)
		)))))))
