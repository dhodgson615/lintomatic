# Makefile for lintomatic

# Compiler settings
GHC = ghc
GHCFLAGS = -O2 --make
SOURCE = lintomatic.hs
TARGET = lintomatic

# Default target
all: $(TARGET)

# Build the main executable
$(TARGET): $(SOURCE)
	$(GHC) $(GHCFLAGS) $(SOURCE) -o $(TARGET)

# Build with debug information
debug: GHCFLAGS = --make -g
debug: $(TARGET)

# Clean build artifacts
clean:
	rm -f *.hi *.o $(TARGET)

# Install system dependencies (Ubuntu/Debian)
install-deps:
	@echo "Installing Haskell dependencies..."
	@if command -v apt-get >/dev/null 2>&1; then \
		sudo apt-get update && sudo apt-get install -y ghc cabal-install; \
	elif command -v brew >/dev/null 2>&1; then \
		brew install ghc cabal-install; \
	elif command -v pacman >/dev/null 2>&1; then \
		sudo pacman -S ghc cabal-install; \
	else \
		echo "Please install GHC manually. See README.md for instructions."; \
	fi

# Check Haskell environment
check-env:
	@echo "Checking Haskell environment..."
	@ghc --version || (echo "GHC not found. Run 'make install-deps' or see README.md"; exit 1)
	@echo "Environment check passed!"

# Run quick test
test: $(TARGET)
	@echo "Running basic functionality test..."
	@mkdir -p test_temp
	@echo '"""This is a test docstring that exceeds seventy two characters and should be flagged"""' > test_temp/test.py
	@echo 'def test():' >> test_temp/test.py
	@echo '    if True:' >> test_temp/test.py
	@echo '        pass' >> test_temp/test.py
	@echo '    else:' >> test_temp/test.py
	@echo '        print("test")' >> test_temp/test.py
	@cd test_temp && ../$(TARGET) || true
	@rm -rf test_temp
	@echo "Test completed!"

# Show help
help:
	@echo "Available targets:"
	@echo "  all         - Build lintomatic (default)"
	@echo "  debug       - Build with debug information"
	@echo "  clean       - Remove build artifacts"
	@echo "  install-deps- Install system dependencies"
	@echo "  check-env   - Verify Haskell environment"
	@echo "  test        - Run quick functionality test"
	@echo "  help        - Show this help message"

.PHONY: all debug clean install-deps check-env test help