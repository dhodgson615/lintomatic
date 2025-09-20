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
