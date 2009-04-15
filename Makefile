package: *.el
	@ver=`grep -o "Version: .*" company.el | cut -c 10-`; \
	tar cjvf company-$$ver.tar.bz2 --mode 644 `git ls-files '*.el' | xargs`

elpa: *.el
	@version=`grep -o "Version: .*" company.el | cut -c 10-`; \
	dir=company-$$version; \
	mkdir -p "$$dir"; \
	cp `git ls-files '*.el' | xargs` company-$$version; \
	echo "(define-package \"company\" \"$$version\" \
	\"extensible inline text completion mechanism\")" \
	> "$$dir"/company-pkg.el; \
	tar cvf company-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf company-*/ company-*.tar company-*.tar.bz2
