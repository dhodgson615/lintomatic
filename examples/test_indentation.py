#!/usr/bin/env python3
"""
This module demonstrates indentation issues that lintomatic detects.
"""

def test_problematic_indentation():
    """Function demonstrating problematic indentation patterns."""
    if True:
        value = "indented properly"
    print("This line should be flagged - missing blank line above dedentation")
    
    if False:
        other_value = "also indented properly"
    
    print("This line is properly formatted with blank line separation")

def test_good_indentation():
    """Function demonstrating proper indentation patterns."""
    if True:
        value = "indented"
    
    print("Proper blank line above")
    
    for i in range(3):
        print(f"Item {i}")
    
    print("Another proper blank line above")

class TestClass:
    """Class demonstrating mixed indentation patterns."""
    
    def method_with_issue(self):
        """Method with indentation issue."""
        if True:
            result = "nested"
        return result  # This should be flagged
        
    def method_without_issue(self):
        """Method with proper indentation."""
        if True:
            result = "nested"
        
        return result  # Proper spacing