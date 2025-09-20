# lintomatic

A lightweight Python linter written in Haskell that focuses on docstring formatting and indentation style issues.

## Overview

lintomatic is a specialized linting tool that checks Python codebases for:
- **Docstring line length violations**: Identifies lines within docstrings that exceed 72 characters
- **Problematic indentation patterns**: Detects code dedentation without proper blank line separation

## Dependencies

### System Requirements
- **Haskell Platform**: GHC (Glasgow Haskell Compiler) version 8.10 or later
- **Operating System**: Linux, macOS, or Windows with GHC support
- **Memory**: Minimal requirements (< 100MB for typical projects)

### Haskell Dependencies
lintomatic uses only standard Haskell library modules:
- `System.Directory` - File system operations
- `System.FilePath` - Path manipulation
- `Data.List` - List processing functions
- `Control.Monad` - Monadic operations
- `System.Environment` - Environment access
- `Data.Char` - Character operations

No external packages or dependencies are required beyond the standard GHC installation.

## Installation and Setup

### Verifying Haskell Environment

Before installation, verify your Haskell environment:

```bash
# Check GHC installation
ghc --version
# Should output: The Glorious Glasgow Haskell Compilation System, version X.X.X

# Check that required modules are available
ghci -e ":module System.Directory System.FilePath Data.List"
# Should load without errors
```

### Compilation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/dhodgson615/lintomatic.git
   cd lintomatic
   ```

2. **Compile using Makefile (recommended)**:
   ```bash
   make check-env  # Verify Haskell environment
   make            # Compile the tool
   make test       # Test with example files
   ```

3. **Or compile manually**:
   ```bash
   ghc -o lintomatic lintomatic.hs
   ```

4. **Verify compilation**:
   ```bash
   ./lintomatic --help 2>/dev/null || echo "Compiled successfully - run './lintomatic' in a directory with Python files"
   ```

### Alternative Compilation Options

For optimized builds:
```bash
# Optimized compilation
make optimized

# Or manually with optimization
ghc -O2 -o lintomatic lintomatic.hs

# Static linking (for distribution)
ghc -static -o lintomatic lintomatic.hs
```

### Build Management

The included Makefile provides convenient build targets:

```bash
make            # Standard compilation
make optimized  # Optimized build
make test       # Compile and test with examples
make clean      # Remove build artifacts
make check-env  # Verify Haskell environment
make install    # Install to /usr/local/bin (requires sudo)
make uninstall  # Remove from /usr/local/bin (requires sudo)
```

## Usage

### Basic Usage

Run lintomatic in any directory containing Python files:

```bash
./lintomatic
```

The tool will:
1. Recursively find all `.py` files in the current directory and subdirectories
2. Analyze each file for linting issues
3. Report findings to stdout

### Example Output

```
File: src/example.py
        Docstring lines exceeding 72 characters:
            Line 15
            Line 28
        Lines with problematic indentation:
            Line 45

File: tests/test_module.py
        Docstring lines exceeding 72 characters:
            Line 8
```

### Integration with Build Systems

**Makefile integration**:
```makefile
lint:
	./lintomatic

check: lint
	@echo "Linting complete"
```

**CI/CD integration**:
```bash
#!/bin/bash
# In your CI script
./lintomatic
if [ $? -eq 0 ]; then
    echo "No critical linting issues found"
else
    echo "Linting issues detected - review output above"
fi
```

## Test Cases and Examples

### Docstring Length Issues

lintomatic flags docstring lines exceeding 72 characters:

```python
def example_function():
    """
    This is a short docstring that fits within the 72-character limit.
    This line is intentionally very long and exceeds the seventy-two character limit set by the linter.
    """
    pass
```

**Expected output**: Reports line number of the long docstring line.

### Indentation Issues

lintomatic detects problematic dedentation patterns:

```python
def problematic_function():
    if condition:
        do_something()
    print("This line should have a blank line above due to dedentation")
    
def good_function():
    if condition:
        do_something()
    
    print("This is properly formatted with blank line separation")
```

**Expected output**: Reports the line number where dedentation occurs without proper separation.

### Test File Examples

The repository includes example files in the `examples/` directory that demonstrate lintomatic's functionality:

**`examples/test_long_docstrings.py`** - Demonstrates docstring length violations:
```python
def test_function():
    """This function has a docstring line that is intentionally very long to trigger the seventy-two character limit check."""
    return True
```

**`examples/test_indentation.py`** - Demonstrates indentation issues:
```python
def test_problematic_indentation():
    if True:
        value = "indented properly"
    print("This line should be flagged - missing blank line above dedentation")
```

**Running tests with examples**:
```bash
make test  # Uses examples directory
# Or run manually:
./lintomatic  # Will scan examples/ directory and report issues
```

**Expected output**:
```
File: examples/test_long_docstrings.py
        Docstring lines exceeding 72 characters:
            Line 7
            Line 22

File: examples/test_indentation.py
        Lines with problematic indentation:
            Line 10
```

Create test files to verify lintomatic functionality:

**test_long_docstrings.py**:
```python
"""
This module demonstrates docstring length issues that lintomatic should detect and report.
"""

def test_function():
    """This function has a docstring line that is intentionally very long to trigger the seventy-two character limit check."""
    return True
```

**test_indentation.py**:
```python
def test_indentation():
    if True:
        value = "indented"
    print("Missing blank line above")  # Should be flagged
    
    if False:
        other_value = "also indented"
    
    print("Proper blank line above")  # Should not be flagged
```

## Use Cases

### When to Use lintomatic

**Ideal scenarios**:
- **Legacy Python codebases** with inconsistent docstring formatting
- **Team projects** requiring consistent indentation style
- **Pre-commit hooks** for basic style enforcement
- **Educational environments** teaching Python style guidelines
- **Continuous integration** pipelines needing lightweight linting

**Complementary tools**:
- Use alongside `flake8` for comprehensive style checking
- Combine with `black` for automated formatting
- Integrate with `mypy` for type checking

### Advantages

- **Lightweight**: Minimal resource usage and fast execution
- **Focused**: Targets specific style issues without overwhelming output
- **Zero configuration**: Works out of the box with sensible defaults
- **Cross-platform**: Runs anywhere GHC is supported

### Limitations

- **Limited scope**: Only checks docstring length and indentation patterns
- **No auto-fixing**: Reports issues but doesn't automatically fix them
- **Python-specific**: Only works with `.py` files
- **Fixed rules**: No configuration options for different style preferences

## Future Features and Roadmap

### Planned Enhancements

**Short-term (v0.2)**:
- [ ] Command-line options for custom line length limits
- [ ] Configuration file support (`.lintomaticrc`)
- [ ] Quiet mode and verbose output options
- [ ] Exit codes indicating issue severity

**Medium-term (v0.3)**:
- [ ] Additional Python style checks (import ordering, function spacing)
- [ ] JSON/XML output formats for tool integration
- [ ] Exclusion patterns for files/directories
- [ ] Color-coded output for better readability

**Long-term (v1.0)**:
- [ ] Plugin architecture for custom rules
- [ ] Integration with popular editors (VS Code, Vim)
- [ ] Performance optimizations for large codebases
- [ ] Support for other languages (JavaScript, TypeScript)

### Contributing

Contributions are welcome! Areas where help is needed:
- **Performance optimization** for large repositories
- **Additional linting rules** following Python style guides
- **Configuration options** for different team preferences
- **Documentation improvements** and examples
- **Test coverage** expansion

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Troubleshooting

### Common Issues

**"ghc: command not found"**:
- Install GHC using your system package manager or [GHCup](https://www.haskell.org/ghcup/)

**Compilation errors**:
- Ensure GHC version is 8.10 or later
- Check that all required modules are available

**No output when running**:
- Verify you're in a directory containing `.py` files
- Check that Python files contain docstrings or indentation to analyze

**Performance issues with large repositories**:
- Consider excluding build directories or virtual environments
- Run on specific subdirectories rather than entire repository

### Getting Help

- **Issues**: Report bugs or feature requests on [GitHub Issues](https://github.com/dhodgson615/lintomatic/issues)
- **Discussions**: General questions and usage help
- **Contributing**: See contribution guidelines above
