# Test comprehensive block statement scenarios

# Test 1: Block statements needing blank lines
x = 1
if x > 0:  # Should be flagged - no blank line before if
    pass

y = 2

if y > 0:  # Should NOT be flagged - has blank line before
    pass

# Test 2: Nested blocks (should not be flagged)
if True:
    if False:  # Should NOT be flagged - directly nested
        pass
    elif True:  # Should NOT be flagged - directly nested
        pass
    else:  # Should NOT be flagged - directly nested  
        pass

# Test 3: Sequential blocks at same level
if True:
    pass
if False:  # Should be flagged - no blank line before if
    pass

# Test 4: Different block types
for i in range(1):
    pass
while True:  # Should be flagged - no blank line before while
    break

def func():
    pass
class MyClass:  # Should be flagged - no blank line before class
    pass

# Test 5: Exception handling
try:
    pass
except:  # Should be flagged - no blank line before except
    pass
finally:  # Should be flagged - no blank line before finally
    pass

# Test 6: With statements
import os
with open('file') as f:  # Should be flagged - no blank line before with
    pass