
# Makefile
CC=erlc
ERL=erl
ERLC=erlc
SRC_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
ERLC_FLAGS=-W -I $(INCLUDE_DIR) -o $(EBIN_DIR)


all:
	#@$(ERL) -make
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/*.erl 



clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump
	#rm -rf ebin/*.boot ebin/*.rel ebin/*.script
	#rm -rf doc/*.html doc/*.css doc/erlang.png doc/edoc-info