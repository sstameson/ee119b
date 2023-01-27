# fibonacci numbers
def fib(n):
    curr = 0
    next = 1
    for _ in range(n):
        curr, next = next, curr + next
    return curr


# generate a comment to check data memory read or write result
def check(op, val, addr):
    assert op in ('R', 'W')
    return f'{op} {val:02X} {addr:02X}'


# format 8-bit immediate
def imm8(x):
    return f'${(x & 0xFF):02X}'


# format 16-bit immediate
def imm16(x):
    return f'${(x & 0xFFFF):04X}'


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


def test_and(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    res = (a & b) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('AND', r1, r2, f'compute {r1} <- {r1} & {r2}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check and result'))

def test_andi(a, k, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= k and k <= 0xFF

    res = (a & k) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('ANDI', r1, imm8(k), f'compute {r1} <- {r1} & {k:#x}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check and result'))

def test_or(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    res = (a | b) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('OR', r1, r2, f'compute {r1} <- {r1} | {r2}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check or result'))

def test_ori(a, k, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= k and k <= 0xFF

    res = (a | k) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('ORI', r1, imm8(k), f'compute {r1} <- {r1} & {k:#x}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check or result'))

def test_eor(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    res = (a ^ b) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('EOR', r1, r2, f'compute {r1} <- {r1} ^ {r2}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check eor result'))

def test_com(a, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF

    res = (~a) & 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('COM', r1, comment=f'compute {r1} <- ~{r1}'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check complement result'))


def test_asr(a, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))

    if a & (1 << 7):
        a = a - (1 << 8)
    res = (a >> 1) & 0xFF # arithmetic shift

    print(instr('ASR', r1, comment=f'compute {r1} <- {r1} >> 1'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check arithmetic shift result'))

def test_lsr(a, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF

    res = (a & 0xFF) >> 1 # logical shift

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LSR', r1, comment=f'compute {r1} <- {r1} >>> 1'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check logical shift result'))

def test_ror(a, c, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert isinstance(c, bool)

    res = ((a & 0xFF) >> 1) | (c << 7)

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('ROR', r1, comment=f'compute {r1} <- (c, {r1} >> 1)'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check rotate through carry result'))

def test_swap(a, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF

    res = ((a & 0x0F) << 4) | ((a & 0xF0) >> 4)

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('SWAP', r1, comment=f'compute {r1} <- ({r1}(3:0), {r1}(7:4))'))
    print(instr('STS', imm16(addr), r1, check('W', res, addr) + ' check swap nibble result'))


def test_cp(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('CP', r1, r2, f'compute {r1} - {r2}'))

def test_cpc(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('CPC', r1, r2, f'compute {r1} - {r2} - C'))

def test_cpi(a, k, r1='R16', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= k and k <= 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('CPI', r1, imm8(k), f'compute {r1} - {k:#x}'))


def test_cpse(a, b, r1='R16', r2='R17', addr=0x1000):
    assert 0 <= a and a <= 0xFF
    assert 0 <= b and b <= 0xFF

    print(instr('LDI', r1, imm8(a), f'load {r1} <- {a:#x}'))
    print(instr('LDI', r2, imm8(b), f'load {r2} <- {b:#x}'))
    print(instr('CPSE', r1, r2, f'compare {r1} to {r2}'))
    if a == b:
        print(instr('NOP', comment='S skip this NOP'))
    else:
        print(instr('NOP', comment='run this NOP'))


def skip_clr(r, b):
    assert 0 <= b and b <= 8

    print(instr('SBRS', r, imm8(b), f'{r}({b}) is clear'))
    print(instr('NOP', comment=f'run this NOP'))
    print(instr('SBRC', r, imm8(b), f'{r}({b}) is clear'))
    print(instr('NOP', comment=f'S skip this NOP'))

def skip_set(r, b):
    assert 0 <= b and b <= 8

    print(instr('SBRC', r, imm8(b), f'{r}({b}) is set'))
    print(instr('NOP', comment=f'run this NOP'))
    print(instr('SBRS', r, imm8(b), f'{r}({b}) is set'))
    print(instr('NOP', comment=f'S skip this NOP'))


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
ORG_ADDR_INC  = 0x003F

def test_flag_set(flag):
    global g_org_addr
    name = label_name()
    print(instr('BRBC', str(flag_idx[flag]), name, f'branch not taken'))
    print(instr('NOP', comment='no-op after missed branch'))
    print(instr('BRBS', str(flag_idx[flag]), name, f'J flag {flag} set so branch taken'))
    print(instr('.ORG', imm16(g_org_addr)))
    print(label(name))
    g_org_addr += ORG_ADDR_INC

def test_flag_clr(flag):
    global g_org_addr
    name = label_name()
    print(instr('BRBS', str(flag_idx[flag]), name, f'branch not taken'))
    print(instr('NOP', comment='no-op after missed branch'))
    print(instr('BRBC', str(flag_idx[flag]), name, f'J flag {flag} clear so branch taken'))
    print(instr('.ORG', imm16(g_org_addr)))
    print(label(name))
    g_org_addr += ORG_ADDR_INC


# generate the program
if __name__ == '__main__':

    print(';' * 75)
    print(';')
    print('; AVR test program #1')
    print(';')
    print('; Hayward Melton')
    print('; Spiro Stameson')
    print(';')
    print('; This a test program for an AVR CPU. This file contains code that')
    print('; checks all ALU instructions, conditional branches, skip instructions,')
    print('; and the no-op instruction. There are annotated to enable the')
    print('; future generation of test vectors.')
    print(';')
    print('; `; W <val> <addr>` means that the 8-bit value <val> is written')
    print('; to the 16-bit address <addr> on the data memory bus')
    print('; `; R <val> <addr>` means that the 8-bit value <val> is read')
    print('; from the 16-bit address <addr> on the data memory bus')
    print('; `; J` means that a jump is taken')
    print('; `; S` means that the next instruction is skipped')
    print(';')
    print('; This is one test program from a two-person group. The second')
    print('; program contains exhaustive test code for data movement and')
    print('; flow control instructions')
    print(';')
    print(';' * 75)

    print()
    print()
    print()

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
        test_flag_clr(flag)
    print()

    print(';')
    print('; check T flag operations')
    print(';')

    print('; check bit set')
    print(instr('LDI', 'R16', imm8(0xFF), f'load R16 <- 0xff'))
    print(instr('BLD', 'R16', imm8(1), f'load R16(1) <- T'))
    print(instr('BLD', 'R16', imm8(3), f'load R16(3) <- T'))
    print(instr('STS', imm16(0x1000), 'R16', check('W', 0xF5, 0x1000) + ' check bit load from T flag result'))

    print('; check bit load')
    print(instr('BST', 'R16', imm8(0), f'set T <- R16(0)'))
    test_flag_set('T')
    print(instr('BST', 'R16', imm8(1), f'set T <- R16(1)'))
    test_flag_clr('T')
    print()

    print(';')
    print('; check compare operations')
    print(';')

    print('; check compare')
    print('; test carry flag')
    test_cp(2, 1)
    test_flag_clr('C')
    test_cp(0, 0x80)
    test_flag_set('C')
    print('; test zero flag')
    test_cp(2, 1)
    test_flag_clr('Z')
    test_cp(0, 0)
    test_flag_set('Z')
    print('; test negative flag')
    test_cp(2, 1)
    test_flag_clr('N')
    test_cp(0, 1)
    test_flag_set('N')
    print('; test signed overflow flag')
    test_cp(2, 1)
    test_flag_clr('V')
    test_cp(0x80, 1)
    test_flag_set('V')
    print('; test corrected signed flag')
    test_cp(2, 1)
    test_flag_clr('S')
    test_cp(0, 1)
    test_flag_set('S')
    print('; test half carry flag')
    test_cp(2, 1)
    test_flag_clr('H')
    test_cp(0, 1)
    test_flag_set('H')

    print('; check compare with intermediate')
    test_cpi(2, 1)
    test_flag_clr('C')
    test_cpi(0, 0x80)
    test_flag_set('C')
    print('; test zero flag')
    test_cpi(2, 1)
    test_flag_clr('Z')
    test_cpi(0, 0)
    test_flag_set('Z')
    print('; test negative flag')
    test_cpi(2, 1)
    test_flag_clr('N')
    test_cpi(0, 1)
    test_flag_set('N')
    print('; test signed overflow flag')
    test_cpi(2, 1)
    test_flag_clr('V')
    test_cpi(0x80, 1)
    test_flag_set('V')
    print('; test corrected signed flag')
    test_cpi(2, 1)
    test_flag_clr('S')
    test_cpi(0, 1)
    test_flag_set('S')
    print('; test half carry flag')
    test_cpi(2, 1)
    test_flag_clr('H')
    test_cpi(0, 1)
    test_flag_set('H')

    print('; check compare with carry')
    print('; clear carry')
    test_cp(0, 0)
    test_cpc(0, 0)
    test_flag_set('Z')
    print('; generate a carry')
    test_add(0xff, 1)
    print('; make sure carry is used')
    test_cpc(0, 0)
    test_flag_set('N')
    print()

    print(';')
    print('; check add variants')
    print(';')

    print('; add without carry')
    test_add(7, 8)
    test_add(0xf, 1)
    test_add(0xCD, 0x1f)
    test_add(0, 0)

    print('; add with carry')
    print('; clear carry')
    test_add(0, 0)
    test_adc(37, 89, False)
    print('; generate carry')
    test_add(0xff, 1)
    print('; make sure carry is used')
    test_adc(37, 89, True)

    print('; add immediate to word')
    test_adiw(0xBEEF, 1)
    test_adiw(0xABCD, 0x3F)
    print()

    print(';')
    print('; check sub variants')
    print(';')

    print('; subtract without carry')
    test_sub(56, 99)
    test_sub(0xAB, 32)

    print('; subtract immediate')
    test_subi(27, 4)
    test_subi(8, 9)

    print('; subtract with carry')
    print('; clear carry')
    test_add(0, 0)
    test_sbc(0x80, 1, False)
    print('; generate a carry')
    test_add(0xff, 1)
    print('; make sure carry is used')
    test_sbc(32, 8, True)

    print('; subtract immediate with carry')
    print('; clear carry')
    test_add(0, 0)
    test_sbci(0x80, 1, False)
    print('; generate a carry')
    test_add(0xff, 1)
    print('; make sure carry is used')
    test_sbci(32, 8, True)

    print('; subtract immediate from word')
    test_sbiw(0xBEEF, 30)
    test_sbiw(0xABCD, 7)
    print()

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
    print()

    print(';')
    print('; check logic operations')
    print(';')

    print('; check and')
    test_and(0xFF, 0xAA)

    print('; check and with immediate')
    test_andi(0xFF, 0xAA)

    print('; check or')
    test_or(0xFF, 0xAA)

    print('; check or with immediate')
    test_ori(0xFF, 0xAA)

    print('; check exclusive or')
    test_eor(0xFF, 0xAA)

    print('; check complement')
    test_com(0xAF)

    print()

    print(';')
    print('; check shift operations')
    print(';')

    print('; check arithmetic right shift')
    test_asr(0xAA)
    test_asr(0x5F)

    print('; check logical right shift')
    test_lsr(0xAA)
    test_asr(0x5F)

    print('; check rotate right through carry')
    print('; remove carry')
    test_add(0, 0)
    test_ror(0xAA, False)
    print('; generate a carry')
    test_add(0xff, 1)
    test_ror(0xAA, True)

    print('; check swap nibbles')
    test_swap(0xAB)
    print()

    print(';')
    print('; check fibonacci program')
    print(';')

    print(instr('LDI', 'R16', imm8(0), f'load R16 <- {0:#x}'))
    print(instr('LDI', 'R17', imm8(1), f'load R16 <- {1:#x}'))
    print(instr('MOV', 'R0', 'R16', f'load R0 <- R16'))
    print(instr('MOV', 'R1', 'R17', f'load R1 <- R17'))
    for i in range(2, 32):
        print(instr('MOV', f'R{i}', f'R{i-2}', f'load R{i} <- R{i-2}'))
        print(instr('ADD', f'R{i}', f'R{i-1}', f'compute R{i} <- R{i} + R{i-1}'))
    for i in range(32):
        print(instr('STS', imm16(0x1000), f'R{i}', check('W', fib(i) & 0xFF, 0x1000) + f' check R{i} = fib({i}) % 256'))
    print()

    print(';')
    print('; check skip instructions')
    print(';')

    print('; check skip if equal')
    test_cpse(1, 2)
    test_cpse(0, 0)

    print('; check skip if bit set')
    for i in range(8):
        print(instr('LDI', f'R{i + 16}', imm8(1 << i), f'load R{i + 16} <- {(1 << i) & 0xFF:#02x}'))
        skip_set(f'R{i + 16}', i)

    print('; check skip if bit clear')
    for i in range(8):
        print(instr('LDI', f'R{i + 24}', imm8(~(1 << i)), f'load R{i + 24} <- {~(1 << i) & 0xFF:#02x}'))
        skip_clr(f'R{i + 24}', i)
