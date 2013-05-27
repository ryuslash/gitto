.PHONY: export publish

all:

export: manual
	emacs -batch -l project.el -f org-publish-all

publish: export
	rsync -avuz --delete --exclude='*~' _publish/ \
		ryuslash.org:public_html/orgweb/projects/gitto

manual:
	$(MAKE) -C "$(CURDIR)"/../doc html

clean:
	rm -f *.elc
	rm -rf _publish
