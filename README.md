# lintomatic

A specialized Python linter written in Haskell that focuses on docstring line length and indentation consistency.

## Features

- **Docstring Length Checking**: Identifies lines within Python docstrings that exceed 72 characters
- **Indentation Analysis**: Detects problematic indentation patterns where code dedentation occurs without proper separation
- **Recursive Directory Scanning**: Automatically finds and processes all `.py` files in a directory tree
- **Clear Output**: Provides precise line numbers and categorized issue reporting

## Dependencies

### Haskell Environment
- **GHC** (Glasgow Haskell Compiler) version 8.10 or later
- **Cabal** or **Stack** for dependency management (optional, but recommended for development)

### System Requirements
- Unix-like operating system (Linux, macOS)
- Windows (with appropriate Haskell installation)

### Installing Haskell Dependencies

#### Option 1: Using GHCup (Recommended)
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc
ghcup install cabal
```

#### Option 2: System Package Manager

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install ghc cabal-install
```

**macOS (Homebrew):**
```bash
brew install ghc cabal-install
```

**Arch Linux:**
```bash
sudo pacman -S ghc cabal-install
```

### Verifying Installation
```bash
ghc --version      # Should show version 8.10+
cabal --version    # Should show cabal-install version
```

## Installation

### Quick Start
1. Clone the repository:
   ```bash
   git clone https://github.com/dhodgson615/lintomatic.git
   cd lintomatic
   ```

2. Compile the program:
   ```bash
   ghc --make src/lintomatic.hs -o lintomatic
   ```

3. Run the linter:
   ```bash
   ./lintomatic
   ```

### Alternative Build Methods

#### Using Cabal
```bash
cabal build
cabal run lintomatic
```

#### Using Make (with provided Makefile)
```bash
make               # Build the executable
make test          # Run quick functionality test
make check-env     # Verify Haskell environment
make clean         # Clean build artifacts
```

#### Manual Compilation with Optimizations
```bash
ghc -O2 --make src/lintomatic.hs -o lintomatic
```

## Usage

### Basic Usage
Run lintomatic from any directory containing Python files:

```bash
./lintomatic
```

The linter will:
1. Recursively find all `.py` files in the current directory
2. Check each file for docstring line length and indentation issues
3. Report findings with specific line numbers

### Example Output
```
File: src/example.py
        Docstring lines exceeding 72 characters:
            Line 15
            Line 23
        Lines with problematic indentation:
            Line 45

File: tests/test_module.py
        Docstring lines exceeding 72 characters:
            Line 8
```

### Command Line Options
Currently, lintomatic runs with default settings:
- Maximum docstring line length: 72 characters
- Scans from current directory recursively

## Test Cases

### Manual Testing

Create test files to verify functionality:

#### Test 1: Docstring Length Issues
Create `test_docstring.py`:
```python
"""
This is a very long docstring line that definitely exceeds the 72 character limit
"""

def function():
    """Short docstring"""
    pass

class Example:
    """
    Another long line in a class docstring that should be flagged by the linter tool
    """
    pass
```

**Expected Output:**
```
File: test_docstring.py
        Docstring lines exceeding 72 characters:
            Line 2
            Line 10
```

#### Test 2: Indentation Problems
Create `test_indentation.py`:
```python
def example():
    if True:
        print("indented")
    print("dedented without blank line above")  # Should be flagged

    if False:
        pass
        
    print("properly separated")  # Should NOT be flagged
```

**Expected Output:**
```
File: test_indentation.py
        Lines with problematic indentation:
            Line 4
```

#### Test 3: Mixed Issues
Create `test_mixed.py`:
```python
"""
This docstring has a line that is way too long and exceeds the character limit
"""

def problematic_function():
    """Another very long docstring line that should definitely be caught by the linter"""
    x = 1
    if x:
        y = 2
    z = 3  # Problematic dedentation
```

**Expected Output:**
```
File: test_mixed.py
        Docstring lines exceeding 72 characters:
            Line 2
            Line 6
        Lines with problematic indentation:
            Line 10
```

### Automated Testing
Run the test suite by creating test files in a `test_files/` directory and running:
```bash
mkdir test_files
# Create test files as shown above
./lintomatic
```

## Haskell Environment Checks

### Pre-compilation Checks
Verify your Haskell environment before building:

```bash
# Check GHC installation
ghc --version

# Check if required modules are available
ghc-pkg list | grep -E "(directory|filepath|base)"

# Test basic compilation
echo 'main = putStrLn "Hello"' > test.hs
ghc test.hs -o test
./test
rm test test.hi test.o test.hs
```

### Common Issues and Solutions

#### Issue: "Module not found" errors
**Solution:** Ensure GHC base libraries are installed:
```bash
cabal update
cabal install --dependencies-only .
```

#### Issue: Permission denied when running executable
**Solution:** Make the binary executable:
```bash
chmod +x lintomatic
```

#### Issue: "ghc: command not found"
**Solution:** Add GHC to your PATH or use full paths:
```bash
export PATH="$HOME/.ghcup/bin:$PATH"
# or
/usr/local/bin/ghc --make src/lintomatic.hs -o lintomatic
```

## Use Cases

### 1. Pre-commit Hook
Integrate lintomatic into your git workflow:
```bash
# In .git/hooks/pre-commit
#!/bin/bash
cd "$(git rev-parse --show-toplevel)"
if [ -x "./lintomatic" ]; then
    ./lintomatic
    if [ $? -ne 0 ]; then
        echo "Linting issues found. Commit aborted."
        exit 1
    fi
fi
```

### 2. CI/CD Pipeline
Add to your GitHub Actions workflow:
```yaml
- name: Install Haskell
  uses: haskell/actions/setup@v2
  with:
    ghc-version: '9.2'

- name: Build lintomatic
  run: ghc --make src/lintomatic.hs -o lintomatic

- name: Run Python linting
  run: ./lintomatic
```

### 3. Development Workflow
- Run before committing Python code changes
- Integrate with IDE/editor as an external tool
- Use in code review processes to maintain consistency

### 4. Documentation Quality Assurance
- Ensure docstrings remain readable within line limits
- Maintain consistent Python code style across teams
- Automated enforcement of documentation standards

### 5. Legacy Code Cleanup
- Identify areas of existing codebases that need attention
- Gradual improvement of code quality
- Generate reports for technical debt assessment

## Future Features

### Planned Enhancements
- [ ] **Configurable Line Length**: Command-line option to set custom line limits
- [ ] **Configuration Files**: Support for `.lintomatic.yaml` configuration
- [ ] **Additional Python Checks**: 
  - Import statement organization
  - Function argument formatting
  - Comment line length validation
- [ ] **Output Formats**: JSON, XML, and CSV output options
- [ ] **IDE Integration**: LSP (Language Server Protocol) support
- [ ] **Performance Optimization**: Parallel file processing
- [ ] **Ignore Patterns**: `.lintomaticignore` file support
- [ ] **Fix Suggestions**: Automatic fixing capabilities for simple issues

### Possible Extensions
- [ ] **Multi-language Support**: Extend to other languages beyond Python
- [ ] **Custom Rule Engine**: Plugin system for user-defined rules
- [ ] **Git Integration**: Only check changed files in commits
- [ ] **Team Collaboration**: Shared configuration and team-specific rules
- [ ] **Metrics Dashboard**: Web interface for project linting statistics

### Contributing to Future Features
We welcome contributions! Areas where help is particularly needed:
- Performance optimization for large codebases
- Cross-platform testing and compatibility
- Additional linting rules based on Python PEP standards
- Integration with popular Python development tools

## Contributing

### Development Setup
1. Fork the repository
2. Clone your fork: `git clone https://github.com/yourusername/lintomatic.git`
3. Create a feature branch: `git checkout -b feature-name`
4. Make changes and test thoroughly
5. Submit a pull request

### Code Style
- Follow Haskell conventions and existing code style
- Add Haddock documentation for new functions
- Include test cases for new features
- Ensure compilation without warnings

### Reporting Issues
- Use GitHub Issues for bug reports and feature requests
- Include sample Python files that demonstrate the issue
- Specify your Haskell/GHC version and operating system

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- Built with the Glasgow Haskell Compiler
- Inspired by Python PEP 8 and documentation best practices
- Thanks to the Haskell community for excellent tooling and libraries
