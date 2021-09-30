EMACS = emacs

ALL_TARGETS = help package clean test test-gui test-batch compile compile-warn

TAR_OPTIONS = cjvf
ifneq ($(shell uname -s), Darwin)
	TAR_OPTIONS += --mode 644
endif


.PHONY: ${ALL_TARGETS}

help:
	@echo Targets:
	@for t in ${ALL_TARGETS}; do echo "- "$$t; done

package: *.el
	@VERSION=$$(awk '/Version:/{print $$3;exit}' company.el); \
	FILES=$$(find . \! -name .\* -a \( -maxdepth 1 -name \*.el -o -name icons \) ); \
	tar ${TAR_OPTIONS} company-$$VERSION.tar.bz2 $$FILES

clean:
	@rm -rf company-*.tar.bz2 *.elc test/*.elc

test:
	${EMACS} -Q -nw -L . -l test/all.el \
	--eval "(let (pop-up-windows) (ert '(not (tag gui))))"

test-gui:
	${EMACS} -Q -L . -l test/all.el \
	--eval "(let (pop-up-windows) (ert t))"

test-batch:
	${EMACS} -Q --batch -L . -l test/all.el \
	--eval "(ert-run-tests-batch-and-exit '(not (or (tag interactive) (tag gui))))"

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile company.el company-*.el

compile-warn:
	${EMACS} -Q --batch -L . \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile company*.el test/*.el
