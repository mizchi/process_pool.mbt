# process_pool justfile

# Default task: run all tests
default: test

# Run tests for all targets
test: test-native test-js

# Run tests for native target
test-native:
    moon test --target native

# Run tests for js target
test-js:
    moon test --target js

# Build check
check:
    moon check --target native
    moon check --target js

# Clean build artifacts
clean:
    moon clean

# Run example (native)
run-example:
    cd example && moon run --target native .
