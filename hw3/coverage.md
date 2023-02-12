# Coverage Report

This document summarizes the test coverage for the four different subsystems.

## ALU Coverage

The testbench for the ALU works by comparing the DUT against a behavioral
model. The inputs are fully randomized with three seperate coverage bins.

1. F-Block Coverage
2. Add Coverage
3. Shift Coverage

The F-Block section contains cross coverage for OpA, OpB, and FCmd. OpA and
OpB are split into four equal sized bins, so all combinations of big/small
operands are tested for every FCmd.

The Add section contains cross coverage for OpA, OpB, Cin, CinCmd, and two
cases of FCmd corresponding to b and ~b. Again, the operands are split into
four bins each to test combinations of big/small operands for every carry
type. The two FCmd options cover subtraction, which requires inverting the
second operand.

The Shift section contains cross coverage for OpA, Cin, CinCmd, SCmd. As
before the operand is split into four bins. Each value of OpA is tested for
all carry types and every shift command.

Every section checks the Result, Cout, Zero flag, and Sign flag against
the behavioral model. The HalfCout and Overflow flags are only checked in
the adder section (since those flags only matter for the adder).

Each coverage point has a coverage goal of 16 to cover several different
operand values in the range.

## StatusReg Coverage

This testbench covers all bit transitions on all status registers with two
different mask configurations.

1. Walking 1's test
2. Walking 0's test

In the walking 1's test only one mask bit is high and the status register
is tested for all four bit transitions (0 -> 0, 0 -> 1, 1 -> 1, 1 -> 0)

In the walking 0's test only one mask bit is low and all other status registers
are tested for all four bit transitions.

This testbench does not use any randomization.

## RegArray Coverage

The testbench for the register array has three main parts.

1. Data loading in the register file
2. Random access with the RegA and RegB signal width ports
3. Data loading and random access for the double-width RegD port

To test loading data, we perform the following test cases:

- clear all registers
- load walking 1's patterns into all registers
- set all register
- load walking 0's patterns into all registers

This ensures that all bits are toggled and all bit transitions occur.

Random access is tested by loading the register data with their corresponding
number and reading all possible pairs of RegA and RegB. This ensures that the
register selection is done properly.

Double-width registers are tested by loading 10 random values and reading
the random values on the corresponding double-width bus, as well as RegA/RegB.

## Memory Access Unit Coverage

TODO
