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
	@ver=`grep -o "Version: .*" company.el | cut -c 10-`; \
	tar ${TAR_OPTIONS} company-$$ver.tar.bz2 $$(find . -name \*.el)

clean:
	@rm -rf company-*/ company-*.tar.bz2 *.elc ert.el test/*.elc

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
