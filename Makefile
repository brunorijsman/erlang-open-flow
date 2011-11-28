ERL = erl

MAKE_ERL = 'case make:all() of \
              up_to_date ->    \
                halt(0);       \
              error ->         \
                halt(1)        \
            end.'

TEST_ERL = 'eunit:test("ebin", [])'

COVER_ERL = 'case of_cover:coverage_dir("src", "cover") of  \
               ok ->                                        \
                 halt(0);                                   \
               _ ->                                         \
                 halt(1)                                    \
             end.'

all: compile test_with_coverage

all_targets: compile test_only test_with_coverage

compile: ebin
	@${ERL} -noinput -eval ${MAKE_ERL}

test_only: compile
	@${ERL} -noshell -pa ebin -eval ${TEST_ERL} -s init stop

test_with_coverage: compile
	@mkdir -p cover
	@cp src/style.css cover/style.css
	@${ERL} -noshell -pa ebin -eval ${COVER_ERL} -s init stop
	@mv *.COVER.html cover

ebin:
	@mkdir -p ebin

clean:
	@rm -f ebin/*.beam
	@rm -f ebin/erl_crash.dump
	@rm -f erl_crash.dump
	@rm -rf cover
	@find . -name *~ | xargs rm -f
