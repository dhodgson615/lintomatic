#!/usr/bin/env python3
"""
This module demonstrates docstring length issues that lintomatic should detect and report.
"""

def test_function():
    """This function has a docstring line that is intentionally very long to trigger the seventy-two character limit check."""
    return True

class ExampleClass:
    """
    This class demonstrates both good and problematic docstring formatting patterns.
    This line is intentionally very long and exceeds the seventy-two character limit set by the linter for testing purposes.
    This line is properly formatted and should not trigger any warnings from the tool.
    """
    
    def method_with_good_docstring(self):
        """This is a properly formatted docstring within the character limit."""
        pass
        
    def method_with_long_docstring(self):
        """This method has a docstring that contains a line which is intentionally very long and should be flagged by lintomatic."""
        pass