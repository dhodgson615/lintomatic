a = 10
b = 9
c = 8
d = 7
assert b == 9
assert b != c
l = [i for i in range(10)]
if a > b:
    if b > c:
        if c > d:
            return d
        return c
    return b
return a + l[d]