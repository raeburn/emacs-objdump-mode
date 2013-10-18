all autoloads:
	emacs -Q -batch -l autoload \
		--eval '(setq generated-autoload-file (expand-file-name "loaddefs.el"))' \
		-f batch-update-autoloads .
