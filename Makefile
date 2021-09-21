TOOLS=pytest
ROOT=$(shell pwd)
TESTS=tests/algorithms
SOURCES=$(ROOT)/src:$(ROOT)/tests:

NO_REPEAT=$(ROOT)/$(TESTS)/test_longest_sub_str_no_repeats.py

.PHONY: update-toolbox no-repeat all

update-toolbox:
	source venv/bin/activate; python -m pip install pip --upgrade
	source venv/bin/activate; python -m pip install $(TOOLS) --upgrade

no-repeat:
	@source venv/bin/activate; export PYTHONPATH="$$PYTHONPATH:$(SOURCES)"; python -m pytest $(TESTS)/test_longest_sub_str_no_repeats.py

no-repeat-benchmark:
	@source venv/bin/activate; export PYTHONPATH="$$PYTHONPATH:$(SOURCES)"; python $(NO_REPEAT)

all: no-repeat
