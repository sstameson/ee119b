import numpy as np


def float_to_fixed(x: float, NUM_FRAC=18, NUM_BITS=22) -> str:
    return f'{round(x * (1 << NUM_FRAC)) % (1 << NUM_BITS):0{NUM_BITS}b}'

K = 0.6072529350

print( 'constant K: std_logic_vector(21 downto 0) :=')
print(f'    "{float_to_fixed(K)}";')

print('constant consts: calc_vector(0 to 15) := (')
for i in range(16):
    print(f'    "{float_to_fixed(np.arctan(1 / (1 << i)))}",')
print(');')

print(f'x = {float_to_fixed(45 * np.pi / 180, NUM_FRAC=14, NUM_BITS=16)}')
