
TESTS  = $(wildcard examples/*.mc)
ERRORS = $(wildcard errors/*.mc)
DEST   = padovani@pianeta.di.unito.it:public_html/Software/MCC/
NAME   = dist/build/mcc/mcc

all:
	cabal build
	ln -fs $(NAME) mcc

dist:
	cabal sdist

sync:
	make -C html
	scp html/*.* $(DEST)
	scp dist/*.tar.gz $(DEST)

%.check_ok:
	@$(NAME) --log $(@:%.check_ok=%) || echo

check_examples:
	@echo
	@echo "SCRIPTS THAT MUST PASS"
	@echo "–————————————————————–"
	@for i in $(TESTS); do make $$i.check_ok; done

%.check:
	@$(NAME) --log $(@:%.check=%) || echo

check_errors:
	@echo
	@echo "SCRIPTS THAT MUST FAIL"
	@echo "–————————————————————–"
	@for i in $(ERRORS); do make $$i.check; done

update_licence:
	for hs in `find src -name "*.hs"`; do \
		TEMP=`mktemp`; \
		cp LICENSE.hs $$TEMP; \
		tail -n +21 <$$hs >>$$TEMP; \
		mv $$TEMP $$hs; \
	done

check: check_examples check_errors
	@echo

.PHONY: dist clean check check_examples check_errors

clean:
	cabal clean
	rm -f mcc


