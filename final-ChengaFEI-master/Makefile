
#####################################################################################################
STACK=stack --allow-different-user
BUILD_OPTS=
#####################################################################################################

test: clean
	$(STACK) test $(BUILD_OPTS)

bin:
	$(STACK) build $(BUILD_OPTS)

clean: 
	$(STACK) clean
	rm -rf tests/input/*.log

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c src/

ghci:
	$(STACK) ghci
