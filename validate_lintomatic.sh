#!/bin/bash

# Test script to validate lintomatic functionality
# This script creates test scenarios and validates the linter works correctly

echo "=== Lintomatic Validation Test Suite ==="

# Clean up any previous test files
rm -rf test_validation/

# Create test directory
mkdir -p test_validation/subdir

echo "Creating test Python files..."

# Test 1: Clean Python file (should produce no output)
cat > test_validation/clean.py << 'EOF'
"""
A clean Python file that should pass all linting checks.
This docstring is under 72 characters.
"""

def clean_function():
    """Short docstring."""
    return "Hello World"

class CleanClass:
    """Class docstring."""
    
    def method(self):
        """Method docstring."""
        pass

if __name__ == "__main__":
    print(clean_function())
EOF

# Test 2: File with docstring length violations
cat > test_validation/long_docstrings.py << 'EOF'
"""
This file has docstring lines that exceed the 72 character limit for testing purposes.
"""

def problematic_function():
    """This is an extremely long docstring line that definitely exceeds the 72 character limit and should be flagged."""
    pass

class TestClass:
    """Another very long docstring line that exceeds the maximum allowed character limit of 72 characters."""
    pass
EOF

# Test 3: File with indentation problems
cat > test_validation/bad_indentation.py << 'EOF'
"""
This file has indentation issues for testing.
"""

def function_with_issues():
    """Function docstring."""
    if True:
        print("indented line")
    print("This line has problematic indentation - no blank line above")
    
    for i in range(5):
        print(i)
    print("Another problematic indentation")
EOF

# Test 4: File with both issues
cat > test_validation/both_issues.py << 'EOF'
"""
File with both long docstrings and indentation problems for comprehensive testing.
"""

def problematic_function():
    """This is a very long docstring that exceeds the seventy-two character limit and should trigger a warning."""
    if True:
        print("some code")
    print("Bad indentation here")
    
    try:
        x = 1
    except:
        pass
    print("Another indentation issue")
EOF

# Test 5: Nested directory test
cat > test_validation/subdir/nested.py << 'EOF'
"""Testing nested directory scanning."""

def nested_function():
    """This docstring is way too long and should be detected even in nested directories for testing purposes."""
    pass
EOF

echo "Building lintomatic..."
if ! ghc --make lintomatic.hs -o lintomatic; then
    echo "ERROR: Failed to build lintomatic"
    exit 1
fi

echo "Running lintomatic on test files..."
./lintomatic test_validation

echo
echo "=== Expected Results ==="
echo "✓ clean.py: Should produce NO output"
echo "✓ long_docstrings.py: Should report lines 2, 6, 10"
echo "✓ bad_indentation.py: Should report indentation issues on lines 9, 13"
echo "✓ both_issues.py: Should report both docstring and indentation issues"
echo "✓ subdir/nested.py: Should report docstring issue on line 4"

echo
echo "=== Validation Test Complete ==="

# Clean up
rm -rf test_validation/