"""
This is a test Python file with some docstring and indentation issues for testing the linter.
"""

def test_function():
    """This is a very long docstring line that exceeds the 72 character limit and should be flagged by the linter."""
    if True:
        print("Hello")
    print("This line has problematic indentation")

class TestClass:
    """Another docstring."""
    
    def method(self):
        """Short docstring."""
        pass
        
def another_function():
    pass