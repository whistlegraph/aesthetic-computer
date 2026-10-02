CC = clang
CFLAGS = -std=c11 -O2 -Wall -Wextra -Werror -pthread

.PHONY: all test full-test sanitize full-sanitize clean
all: .build/aesel-c .build/aesel-native

.build/aesel-c: terminal.c
	@mkdir -p .build
	$(CC) $(CFLAGS) $< -o $@

.build/aesel-native: full-host.c
	@mkdir -p .build
	$(CC) $(CFLAGS) $< -o $@

test: all
	python3 test.py
	node --test bridge.test.mjs

full-test: all
	python3 full-host-test.py
	python3 full-feature-test.py
	AESEL_TEST_RUNTIME=bun python3 full-tests.py input-tui.py
	AESEL_TEST_RUNTIME=bun python3 full-tests.py network-tui.py
	AESEL_TEST_RUNTIME=bun python3 full-tests.py restart-tui.py
	python3 full-tests.py harness-tui.py
	python3 full-tests.py activity-tui.py

full-sanitize:
	@mkdir -p .build
	$(CC) -std=c11 -g -O1 -Wall -Wextra -Werror -pthread -fsanitize=address,undefined -fno-omit-frame-pointer full-host.c -o .build/aesel-native-sanitize
	AESEL_TEST_NATIVE_HOST="$(CURDIR)/.build/aesel-native-sanitize" python3 full-host-test.py

sanitize:
	@mkdir -p .build
	$(CC) -std=c11 -g -O1 -Wall -Wextra -Werror -pthread -fsanitize=address,undefined -fno-omit-frame-pointer terminal.c -o .build/aesel-c-sanitize
	C_TUI_BINARY="$(CURDIR)/.build/aesel-c-sanitize" python3 test.py

clean:
	rm -rf .build
