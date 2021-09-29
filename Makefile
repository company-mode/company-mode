EMACS=emacs

.PHONY: package elpa clean test test-gui test-batch compile compile-warn

package: *.el
	@ver=`grep -o "Version: .*" company.el | cut -c 10-`; \
	tar cjvf company-$$ver.tar.bz2 --mode 644 $$(find . -name \*.el)

elpa: *.el
	@version=`grep -o "Version: .*" company.el | cut -c 10-`; \
	dir=company-$$version; \
	mkdir -p "$$dir"; \
	cp $$(find . -name \*.el) company-$$version; \
	echo "(define-package \"company\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/company-pkg.el; \
	tar cvf company-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf company-*/ company-*.tar company-*.tar.bz2 *.elc ert.el test/*.elc

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
