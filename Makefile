REBAR3=./rebar3
COVERTOOL=./covertool

##
## define the default release path in order to
## manage the core system. The release must be generated
## before try to start using `make rel`.
## TODO: Check if after creation release could be
## moved to another dir for easy access.
##
RELEASE_PATH=_build/default/rel/dasherl/bin

##
## define the main script that controls 
## the management of the release.
##
DASHERL=dasherl

.PHONY: dasherl compile all doc test

all: compile

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

rel:
	$(REBAR3) release

run:
	@sh $(RELEASE_PATH)/$(DASHERL) start

live:
	@sh $(RELEASE_PATH)/$(DASHERL) console

stop:
	@sh $(RELEASE_PATH)/$(DASHERL) stop

test:
	$(REBAR3) ct --cover
	$(REBAR3) cover
	$(COVERTOOL) -cover _build/test/cover/ct.coverdata -appname dasherl -output cobertura.xml

cover:
	$(REBAR3) cover
