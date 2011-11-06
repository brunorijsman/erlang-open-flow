ERL = erl

MAKE_ERL = 'case make:all() of \
              up_to_date ->    \
                halt(0);       \
              error ->         \
                halt(1)        \
            end.'			

all: compile

compile: ebin
	@$(ERL) -noinput -eval $(MAKE_ERL)

ebin:
	@mkdir -p ebin

clean:
	rm -f ebin/*.beam
	rm -f ebin/erl_crash.dump
	rm -f erl_crash.dump
