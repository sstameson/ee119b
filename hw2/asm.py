
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
    s = f'    {mem:<5}'

    if op1 is not None:
        s += f'{op1}'

    if op2 is not None:
        s += f',{op2}'

    s = f'{s:<25}'

    if comment is not None:
        # only add space if comment is not checking a value
        if (comment[0] in ('R', 'W', 'S', 'J')):
            s += f';{comment}'
        else:
            s += f'; {comment}'

    return s


### ARITHMETIC TESTING ###


def test_add(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    res = (a + b) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('ADD', r1, r2, f'compute {r1} <- {r1} + {r2}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check add result'))

def test_adc(a, b, c, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF
    assert isinstance(c, bool)

    res = (a + b + c) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('ADC', r1, r2, f'compute {r1} <- {r1} + {r2} + C'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check add result (w/ carry)'))

def test_adiw(a, k, r1='R24', r2='R25', addr=0x1000):
    assert 0 <= a and a <= 0xFFFF
    assert 0 <= k and k <= 63

    a1 = (a & 0x00FF)
    a2 = (a & 0xFF00) >> 8

    res = (a + k) & 0xFFFF

    res1 = (res & 0x00FF)
    res2 = (res & 0xFF00) >> 8

    print(instr('LDI', r1, imm8(a1), f'load {r1} <- {a1:#x}'))
    print(instr('LDI', r2, imm8(a2), f'load {r2} <- {a2:#x}'))
    print(instr('ADIW', f'{r2}:{r1}', imm8(k),
               f'compute {r2}:{r1} <- {r2}:{r1} + {k:#x}'))
    print(instr('STS', imm16(addr), r1, check('W', res1, addr) + ' check add result (lower byte)'))
    print(instr('STS', imm16(addr), r2, check('W', res2, addr) + ' check add result (upper byte)'))

def test_sub(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    res = (a - b) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('SUB', r1, r2, f'compute {r1} <- {r1} - {r2}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check sub result'))

def test_subi(a, k, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= k and k <= 0xFF

    res = (a - k) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('SUBI', r1, imm8(k), f'compute {r1} <- {r1} - {k:#x}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check sub result'))

def test_sbc(a, b, c, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF
    assert isinstance(c, bool)

    res = (a - b - c) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('SBC', r1, r2, f'compute {r1} <- {r1} - {r2} - C'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check sub result (w/ carry)'))

def test_sbci(a, k, c, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= k and k <= 0xFF
    assert isinstance(c, bool)

    res = (a - k - c) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('SBCI', r1, imm8(k), f'compute {r1} <- {r1} - {k:#x} - C'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check sub result (w/ carry)'))

def test_sbiw(a, k, r1='R24', r2='R25', addr=0x1000):
    assert 0 <= a and a <= 0xFFFF
    assert 0 <= k and k <= 63

    a1 = (a & 0x00FF)
    a2 = (a & 0xFF00) >> 8

    res = (a - k) & 0xFFFF

    res1 = (res & 0x00FF)
    res2 = (res & 0xFF00) >> 8

    print(instr('LDI', r1, imm8(a1), f'load {r1} <- {a1:#x}'))
    print(instr('LDI', r2, imm8(a2), f'load {r2} <- {a2:#x}'))
    print(instr('SBIW', f'{r2}:{r1}', imm8(k),
               f'compute {r2}:{r1} <- {r2}:{r1} - {k:#x}'))
    print(instr('STS', imm16(addr), r1, check('W', res1, addr) + ' check sub result (lower byte)'))
    print(instr('STS', imm16(addr), r2, check('W', res2, addr) + ' check sub result (upper byte)'))

def test_inc(a, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF

    res = (a + 1) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('INC', r1, comment=f'compute {r1} <- {r1} + 1'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check increment result'))

def test_dec(a, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF

    res = (a - 1) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('DEC', r1, comment=f'compute {r1} <- {r1} - 1'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check decrement result'))


def test_neg(a, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF

    res = (-a) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('NEG', r1, comment=f'compute {r1} <- -{r1}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check negation result'))

### FLAG TESTING ###


g_skip_num = 1
def label_name():
    global g_skip_num
    name = 'Skip' + str(g_skip_num)
    g_skip_num += 1
    return name

flags = ['C', 'Z', 'N', 'V', 'S', 'H', 'T', 'I']
flag_idx = {}
for i in range(len(flags)):
    flag_idx[flags[i]] = i

g_org_addr    = 0x0040
ORG_ADDR_INC  = 0x0010

def test_flag_set(flag):
    global g_org_addr
    name = label_name()
    print(instr('BRBS', str(flag_idx[flag]), name, f'J flag {flag} set'))
    print(instr('.ORG', imm16(g_org_addr)))
    print(label(name))
    g_org_addr += ORG_ADDR_INC

def test_flag_clear(flag):
    global g_org_addr
    name = label_name()
    print(instr('BRBC', str(flag_idx[flag]), name, f'J flag {flag} clear'))
    print(instr('.ORG', imm16(g_org_addr)))
    print(label(name))
    g_org_addr += ORG_ADDR_INC


# generate the program
if __name__ == '__main__':
    print(';')
    print('; set all flags')
    print(';')
    for i in range(8):
        print(instr('BSET', f'{i}'))
    print()

    print(';')
    print('; check all flags set')
    print(';')
    for flag in flags:
        test_flag_set(flag)
    print()

    print(';')
    print('; clear all flags')
    print(';')
    for i in range(8):
        print(instr('BCLR', f'{i}'))
    print()

    print(';')
    print('; check all flags clear')
    print(';')
    for flag in flags:
        test_flag_clear(flag)
    print()

    print(';')
    print('; check add variants')
    print(';')

    print('; add without carry')
    test_add(7, 8)

    print('; add with carry')
    print('; generate a carry')
    test_adc(0xff, 1, False)
    print('; make sure carry is used')
    test_adc(37, 89, True)

    print('; add immediate to word')
    test_adiw(0xBEEF, 1)

    print(';')
    print('; check sub variants')
    print(';')

    print('; subtract without carry')
    test_sub(56, 99)

    print('; subtract immediate')
    test_subi(32, 8)

    print('; subtract with carry')
    test_sbc(0x80, 1, False)
    print('; generate a carry')
    test_adc(0xff, 1, False)
    print('; make sure carry is used')
    test_sbc(32, 8, True)

    print('; subtract immediate with carry')
    test_sbci(0x80, 1, False)
    print('; generate a carry')
    test_adc(0xff, 1, False)
    print('; make sure carry is used')
    test_sbci(32, 8, True)

    print('; subtract immediate from word')
    test_sbiw(0xABCD, 7)

    print(';')
    print('; check unary arithmetic')
    print(';')
    print('; check increment')
    test_inc(0)
    test_inc(0xff)
    print('; check decrement')
    test_dec(0)
    test_dec(1)
    print('; check negation')
    test_neg(1)
    test_neg(0x80)
