TOOLS="pytest"

TESTS="tests/algorithms"

.PHONY: update-toolbox no-repeat all

update-toolbox:
	source venv/bin/activate; python -m pip install pip --upgrade
	source venv/bin/activate; python -m pip install $(TOOLS) --upgrade

no-repeat:
	source venv/bin/activate; python -m pytest $(TESTS)/test_longest_sub_str_no_repeats.py

all: no-repeat
