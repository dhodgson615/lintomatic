# Lintomatic - Python Linter in Haskell

**ALWAYS follow these instructions first and only fallback to additional search and context gathering if the information here is incomplete or found to be in error.**

Lintomatic is a Python code quality tool written in Haskell that scans Python files for:
- Docstring lines exceeding 72 characters
- Problematic indentation (dedentation without blank lines above)

## Working Effectively

### Bootstrap and Build the Repository
- Verify GHC (Glasgow Haskell Compiler) is available: `which ghc && ghc --version`
- Build the linter: `ghc --make src/lintomatic.hs -o lintomatic` -- takes <1 second. NEVER CANCEL.
- Run basic functionality test: `./test/validate_lintomatic.sh` -- takes <5 seconds. NEVER CANCEL.

### Essential Build Information
- **Build time**: ~0.65 seconds (very fast)
- **No build configuration files**: This project uses direct GHC compilation, no .cabal or stack.yaml files
- **No external dependencies**: Uses only base Haskell libraries
- **Artifacts to ignore**: `*.hi`, `*.o`, `lintomatic` executable (already in .gitignore)

### Running the Linter
- Execute: `./lintomatic` (scans current directory recursively)
- Or specify directory: `./lintomatic /path/to/python/project`
- **Execution time**: Sub-second for typical projects
- **Output**: Reports file paths with line numbers for violations, no output means clean code

### Code Quality and Linting
- Install HLint: `cabal update && cabal install hlint` -- takes 15-20 minutes on first install. NEVER CANCEL. Set timeout to 30+ minutes.
- Run HLint: `hlint src/lintomatic.hs` -- takes <0.5 seconds
- HLint may suggest style improvements (exit code 1 with suggestions is normal)

## Validation Scenarios

**ALWAYS run these validation steps after making any changes:**

### Comprehensive Validation
1. **Build test**: `ghc --make src/lintomatic.hs -o lintomatic`
2. **Functionality test**: `./test/validate_lintomatic.sh`
3. **Style check**: `hlint src/lintomatic.hs`

### Manual Testing Scenarios
Create test Python files and verify correct behavior:

**Test 1 - Clean file (should produce no output):**
```python
"""Clean docstring under 72 chars."""

def clean_function():
    """Short docstring."""
    return "Hello"
```

**Test 2 - Long docstring violation:**
```python
"""This docstring exceeds the seventy-two character limit and should be flagged."""
def function():
    pass
```
Expected: Reports line number for long docstring

**Test 3 - Indentation problem:**
```python
def function():
    if True:
        print("indented")
    print("This should be flagged for bad indentation")
```
Expected: Reports line number for indentation issue

### Build and Test Timing Expectations
- **Build**: <1 second - NEVER CANCEL, immediate timeout okay
- **Linting execution**: <0.5 seconds - NEVER CANCEL, immediate timeout okay  
- **HLint analysis**: <0.5 seconds - NEVER CANCEL, immediate timeout okay
- **HLint installation**: 15-20 minutes first time - NEVER CANCEL, use 30+ minute timeout
- **Validation script**: <5 seconds - NEVER CANCEL, 30 second timeout okay

## Common Development Tasks

### Making Code Changes
1. Edit `src/lintomatic.hs` with your changes
2. Build: `ghc --make src/lintomatic.hs -o lintomatic`
3. Test: `./test/validate_lintomatic.sh`
4. Style check: `hlint src/lintomatic.hs`
5. Manual validation: Test with real Python files

### Adding New Linting Rules
- Modify the parsing logic in `checkDocstringLength` or `checkIndentation` functions
- The `findLongLines` function handles docstring detection
- The `findProblematicLines` function handles indentation analysis
- Always test with `./test/validate_lintomatic.sh` after changes

### Understanding the Codebase
- **Single source file**: `src/lintomatic.hs` (147 lines)
- **Main function**: Orchestrates file discovery and linting
- **Core functions**: 
  - `findPythonFiles`: Recursive directory traversal
  - `checkDocstringLength`: Detects long docstring lines
  - `checkIndentation`: Finds problematic dedentation
- **Helper functions**: `strip`, `lstrip`, `rstrip` for string processing

## Repository Information

### File Structure
```
.
├── README.md           # Basic project description  
├── LICENSE            # MIT license
├── src/
│   └── lintomatic.hs      # Main source code (only source file)
├── test/
│   └── validate_lintomatic.sh # Comprehensive test suite
└── .github/
    └── copilot-instructions.md # This file
```

### Key Facts
- **No CI/CD**: No GitHub Actions or build pipelines configured
- **No tests directory**: Use `./test/validate_lintomatic.sh` for testing
- **No package management**: Pure GHC compilation
- **Language**: Haskell with GHC 9.12.2
- **Target**: Python files (.py extension)
- **License**: MIT

### Common Command Reference
| Task | Command | Time | Timeout |
|------|---------|------|---------|
| Build | `ghc --make src/lintomatic.hs -o lintomatic` | <1s | 30s |
| Run | `./lintomatic` | <0.5s | 30s |
| Test | `./test/validate_lintomatic.sh` | <5s | 30s |
| Lint | `hlint src/lintomatic.hs` | <0.5s | 30s |
| HLint Install | `cabal update && cabal install hlint` | 15-20m | 30m |

### Expected Warnings
- GHC warning about `head` function usage (partial function) - this is known and acceptable
- HLint suggestions for style improvements (exit code 1 is normal)

## Critical Reminders
- **NEVER CANCEL** any build or test command even if it seems slow
- Build artifacts (`*.hi`, `*.o`, `lintomatic`) are automatically ignored by .gitignore
- The linter only examines `.py` files and scans directories recursively
- No output from the linter means all Python files are clean
- Always validate changes with the provided test script before committing