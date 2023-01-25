

def instr(mem, op1=None, op2=None, comment=None):
    s = f'\t{mem}'

    if op1 is not None:
        s += f'\t{op1}'

    if op2 is not None:
        s += f',\t{op2}'

    if comment is not None:
        if (comment[0] == 'R' or comment[0] == 'W') and comment[1] == ' ':
            s += f'\t;{comment}'
        else:
            s += f'\t; {comment}'

    return s



print(instr('RET'))
print(instr('JC', 'Skip1'))
print(instr('LDI', 'R1', '$AB', 'this is a test'))
print(instr('ST', '$1000', 'R1', 'W 1000 AB'))
