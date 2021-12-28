ALL_TARGETS = company.info install-info company.html/ install-html clean

.PHONY: ${ALL_TARGETS}

help:
	@echo Targets:
	@for t in ${ALL_TARGETS}; do echo "- "$$t; done

company.info: company.texi
	makeinfo $^ -o $@

# Alternatively, use C-u C-h i <company.info>.
# The path must be present in Emacs' Info-directory-list.
install-info: INFO_INSTALL_DIR ?= /usr/local/share/info/
install-info: company.info
	cp -R $^ images ${INFO_INSTALL_DIR}
	install-info --info-dir=${INFO_INSTALL_DIR} $^

company.html/: \
	HTML_CSS_URL ?= https://company-mode.github.io/stylesheets/stylesheet.css
	--css-ref=${HTML_CSS_URL}
#	HTML_CSS_FILE ?= ../../company-mode.github.com/stylesheets/stylesheet.css
#	--css-include=${HTML_CSS_FILE}
company.html/: company.texi
	makeinfo $^ --html --split=chapter --no-headers --no-number-sections -c \
	AFTER_BODY_OPEN="<div class=\"inner manual\">" -c PRE_BODY_CLOSE="</div>" \
	--css-ref=${HTML_CSS_URL} -o $@

install-html: HTML_INSTALL_DIR ?= ../../company-mode.github.com/manual/
	HTML_DIR = company.html/
	HTML_KEEP_FILES = $(addprefix ${HTML_DIR}, $(addsuffix .html, \
		index Overview Getting-Started Customization Frontends Backends \
		Troubleshooting))
	HTML_REMOVE_FILES = $(filter-out ${HTML_KEEP_FILES}, \
		$(wildcard ${HTML_DIR}*))
install-html: company.html/
	@for f in ${HTML_REMOVE_FILES}; do rm $$f; done
	@mkdir -p ${HTML_INSTALL_DIR}
	@rm -rf ${HTML_INSTALL_DIR}*
	cp -R $^ images ${HTML_INSTALL_DIR}

clean:
	@rm -rf *.info *.html
