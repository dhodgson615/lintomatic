#!/bin/bash

# Simple test runner script for lintomatic

echo "=== Lintomatic Test Suite Runner ==="
echo

# Build and run the comprehensive test suite
echo "Building and running comprehensive test suite..."
cabal test --test-show-details=always

if [ $? -eq 0 ]; then
    echo
    echo "✓ All tests passed!"
    echo
    echo "Running original validation script for backward compatibility..."
    ./test/validate_lintomatic.sh
    echo
    echo "✓ All validation checks completed successfully!"
else
    echo
    echo "✗ Some tests failed. Please check the output above."
    exit 1
fi