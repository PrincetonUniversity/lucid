def f(c):
    if c=="ack":
        return 12
    if c=="ip0":
        return 0
    if c=="ip1":
        return 12
def g(c): 
    if c=="ack":
        return 2
    if c=="ip0":
        return 8
    if c=="ip1":
        return 9

def update2(curr, x, y):
    if ((curr + y)%16 < 3):
        return x^5
    else:
        return curr & 3
def update(curr, l):
    return update2(curr, f(l), g(l))
inlet = "ip1"
instate = 5
assert(update(0, "ack") == 9)
assert(update(0, "ip0") == 0)
assert(update(0, "ip1") == 0)
assert(update(9, "ack") == 1)
assert(update(9, "ip0") == 5)
assert(update(9, "ip1") == 9)
assert(update(1, "ack") == 1)
assert(update(1, "ip0") == 1)
assert(update(1, "ip1") == 1)
assert(update(5, "ack") == 1)
assert(update(5, "ip0") == 1)
assert(update(5, "ip1") == 1)
print("passed")