# Makefile for lintomatic

# Default target
all: lintomatic

# Compile the main executable
lintomatic: lintomatic.hs
	ghc -o lintomatic lintomatic.hs

# Optimized build
optimized: lintomatic.hs
	ghc -O2 -o lintomatic lintomatic.hs

# Test with example files
test: lintomatic
	@echo "Testing lintomatic with example files..."
	./lintomatic

# Clean build artifacts
clean:
	rm -f lintomatic *.hi *.o

# Check Haskell environment
check-env:
	@echo "Checking Haskell environment..."
	@ghc --version || (echo "GHC not found! Please install Haskell."; exit 1)
	@echo "Environment check passed."

# Install (copy to /usr/local/bin)
install: lintomatic
	sudo cp lintomatic /usr/local/bin/

# Uninstall
uninstall:
	sudo rm -f /usr/local/bin/lintomatic

.PHONY: all optimized test clean check-env install uninstall