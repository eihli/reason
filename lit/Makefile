##
# Literate Neural Kanren
#
# @file
# @version 0.1
index.html: index.lit
	srcweave \
		--tangle dist \
		--weave docs \
		--styler srcweave-html-styler \
		index.lit

init: FORCE
	srcweave-html-styler-init -m docs
	mkdir -p dist

deploy: FORCE
	echo "Nope"

FORCE:
# end
