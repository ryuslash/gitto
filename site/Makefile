.PHONY: export publish

all:

export:
	emacs -batch -l project.el -f org-publish-all

publish: export
	rsync -avuz --delete --exclude='*~' _publish/ \
		ryuslash.org:public_html/orgweb/projects/gitto
