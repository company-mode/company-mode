EMACS=emacs
CURL=curl --silent
ERT_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.3

.PHONY: ert test test-batch

package: *.el
	@ver=`grep -o "Version: .*" company.el | cut -c 10-`; \
	tar cjvf company-$$ver.tar.bz2 --mode 644 `git ls-files '*.el' | xargs`

elpa: *.el
	@version=`grep -o "Version: .*" company.el | cut -c 10-`; \
	dir=company-$$version; \
	mkdir -p "$$dir"; \
	cp `git ls-files '*.el' | xargs` company-$$version; \
	echo "(define-package \"company\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/company-pkg.el; \
	tar cvf company-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf company-*/ company-*.tar company-*.tar.bz2 *.elc ert.el

test:
	${EMACS} -Q -nw -L . -l company-tests.el -l company-elisp-tests.el \
	--eval "(let (pop-up-windows) (ert t))"

test-batch:
	${EMACS} -Q --batch -L . -l company-tests.el -l company-elisp-tests.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

downloads:
	${EMACS} -Q --batch -l ert || \
	${CURL} ${ERT_URL} > ert.el

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile company.el company-*.el
