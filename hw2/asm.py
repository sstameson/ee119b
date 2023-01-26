# generate a comment to check data memory read or write result
def check(op, val, addr):
    assert op in ('R', 'W')
    return f'{op} {val:02X} {addr:02X}'


# format 8-bit immediate
def imm8(x):
    return f'${x:02X}'


# format 16-bit immediate
def imm16(x):
    return f'${x:04X}'


# format jump label
def label(name):
    return f'{name}:'


# format instruction
# instruction must have a memonic
# instruction may have 0, 1, or 2 operands
# instruction may be followed by a comment
def instr(mem, op1=None, op2=None, comment=None):
    s = f'\t{mem}'

    if op1 is not None:
        s += f'\t{op1}'

    if op2 is not None:
        s += f',\t{op2}'

    if comment is not None:
        # only add space if comment is not checking a data memory read/write
        if (comment[0] == 'R' or comment[0] == 'W') and comment[1] == ' ':
            s += f'\t;{comment}'
        else:
            s += f'\t; {comment}'

    return s


def test_add(a, b, r1='R16', r2='R17', addr=0x1000):
    val = a + b
    print(instr('LDI', r1, imm8(a)))
    print(instr('LDI', r2, imm8(b)))
    print(instr('ADD', r1, r2))
    print(instr('ST', imm16(addr), r1, check('W', val, addr)))

test_add(7, 8)
