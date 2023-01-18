import numpy as np


def float_to_fixed(x: float, NUM_FRAC=18, NUM_BITS=22) -> str:
    if not np.isfinite(x):
        x = 0
    return f'{round(x * (1 << NUM_FRAC)) % (1 << NUM_BITS):0{NUM_BITS}b}'

def fixed_to_float(x: int, NUM_FRAC=18) -> float:
    return x / (1 << NUM_FRAC)

K0 = 1
K1 = 1 / np.sqrt(2)
K2 = 1
for i in range(1, 1000):
    a = 1 / (1 << i)
    K1 *= 1 / np.sqrt(1 + a**2)
    K2 *= 1 / np.sqrt(1 - a**2)

# K1 = 0.6072529350
print(f'K(0) = {K0}')
print(f'K(1) = {K1}')
print(f'K(2) = {K2}')

print( 'constant K: <T> :=')
print('    (', end="")
print(f'"{float_to_fixed(K0)}", ', end="")
print(f'"{float_to_fixed(K1)}", ', end="")
print(f'"{float_to_fixed(K2)}");')

print('constant consts: <T> := (')
for i in range(16):
    a = 1 / (1 << i)
    print('    (', end="")
    print(f'"{float_to_fixed(a)}", ', end="")
    print(f'"{float_to_fixed(np.arctan(a))}", ', end="")
    print(f'"{float_to_fixed(np.arctanh(a))}"', end="")
    print('),')
print(');')

print(f'x = {float_to_fixed(0.5, NUM_FRAC=14, NUM_BITS=16)}')
print(f'y = {float_to_fixed(0.86602, NUM_FRAC=14, NUM_BITS=16)}')
