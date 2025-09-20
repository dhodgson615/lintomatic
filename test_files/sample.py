#!/usr/bin/env python3
"""
This is a sample Python file with various linting issues.
This docstring line is intentionally very long to test the 72-character limit checker functionality.
"""

def example_function():
    """This is a short docstring."""
    if True:
        print("Indented properly")
    return "test"

class ExampleClass:
    """
    This is a class docstring that has some lines that are quite long and should trigger the linter.
    """
    
    def method(self):
        if True:
            value = "some value"
        print("This should trigger indentation warning")
        
    def another_method(self):
        """Another method with proper indentation."""
        return True