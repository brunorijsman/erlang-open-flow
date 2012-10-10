#=====================================================================================================================
# Copyright (c) 2012, Bruno Rijsman
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the 
# following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this list of conditions and the following 
#   disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the 
#   following disclaimer in the documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
# USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#=====================================================================================================================

ERL = erl

# TODO: tests are run every time, even if nothing changed

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
