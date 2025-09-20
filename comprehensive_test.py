#!/usr/bin/env python3
"""
A sample Python module for comprehensive linter testing.
This docstring should be fine as it doesn't exceed the character limit.
"""

import os
import sys

def well_formatted_function():
    """Short docstring."""
    result = []
    for i in range(10):
        if i % 2 == 0:
            result.append(i)
    
    return result

def problematic_function():
    """This is another extremely long docstring that definitely exceeds the 72 character limit."""
    data = {
        'key1': 'value1',
        'key2': 'value2'
    }
    return data

class MyClass:
    """Class docstring."""
    
    def __init__(self):
        """Constructor docstring."""
        self.value = 42
        
    def method_with_issues(self):
        """Yet another docstring that is way too long and should be flagged by our linter."""
        if True:
            print("indented")
        print("This should be flagged for indentation")
        
def final_function():
    """Final function docstring."""
    pass