ERL = erl

MAKE_ERL = 'case make:all() of \
              up_to_date ->    \
                halt(0);       \
              error ->         \
                halt(1)        \
            end.'			

TEST_ERL = 'eunit:test("ebin", [])'

all: compile test

compile: ebin
	@${ERL} -noinput -eval ${MAKE_ERL}

test: compile
	@${ERL} -noshell -pa ebin -eval ${TEST_ERL} -s init stop

ebin:
	@mkdir -p ebin

clean:
	@rm -f ebin/*.beam
	@rm -f ebin/erl_crash.dump
	@rm -f erl_crash.dump
	@find . -name *~ | xargs rm -f
