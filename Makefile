TOOLS=pytest
ROOT=$(shell pwd)
TESTS=tests/algorithms
SOURCES=$(ROOT)/src:$(ROOT)/tests:

.PHONY: update-toolbox no-repeat all

update-toolbox:
	source venv/bin/activate; python -m pip install pip --upgrade
	source venv/bin/activate; python -m pip install $(TOOLS) --upgrade

no-repeat:
	source venv/bin/activate; export PYTHONPATH="$$PYTHONPATH:$(SOURCES)"; python -m pytest $(TESTS)/test_longest_sub_str_no_repeats.py

all: no-repeat
