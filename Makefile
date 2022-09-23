PROJECT = erlbox
PROJECT_DESCRIPTION = A small box to organize Erlmachine code
PROJECT_VERSION = 1.0.0

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck.git 0.9.0

include erlang.mk
