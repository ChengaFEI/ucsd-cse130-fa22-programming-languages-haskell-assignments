
#####################################################################################################
STACK=stack --allow-different-user
BUILD_OPTS=
#####################################################################################################

test: clean
	$(STACK) test $(BUILD_OPTS)

bin:
	$(STACK) build $(BUILD_OPTS)

repl: 
	$(STACK) run

clean: 
	$(STACK) clean
	rm -rf tests/input/*.log

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c src/

turnin:
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/cse130-assignments/05-classes.git

update:
	git pull upstream master --allow-unrelated-histories

ghci:
	$(STACK) ghci
