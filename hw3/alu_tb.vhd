--
--  ALU
--
--  This is a behavioral ALU model for the CPU.
--
--  Generics:
--    wordsize - width of the ALU in bits (default 8)
--
--  Inputs:
--    ALUOpA   - first operand
--    ALUOpB   - second operand
--    Cin      - carry in (from status register)
--    FCmd     - F-Block operation to perform (4 bits)
--    CinCmd   - adder carry in operation for carry in (2 bits)
--    SCmd     - shift operation to perform (3 bits)
--    ALUCmd   - ALU operation to perform - selects result (2 bits)
--
--  Outputs:
--    Result   - ALU result
--    Cout     - carry out from the operation
--    HalfCOut - half carry out for addition
--    Overflow - signed overflow for addition
--    Zero     - zero result
--    Sign     - result sign (1 negative, 0 positive)
--

library ieee;
use ieee.std_logic_1164.all;

entity  ALUModel  is

    generic (
        wordsize : integer := 8      -- default width is 8-bits
    );

    port(
        ALUOpA   : in      std_logic_vector(wordsize - 1 downto 0);   -- first operand
        ALUOpB   : in      std_logic_vector(wordsize - 1 downto 0);   -- second operand
        Cin      : in      std_logic;                                 -- carry in
        FCmd     : in      std_logic_vector(3 downto 0);              -- F-Block operation
        CinCmd   : in      std_logic_vector(1 downto 0);              -- carry in operation
        SCmd     : in      std_logic_vector(2 downto 0);              -- shift operation
        ALUCmd   : in      std_logic_vector(1 downto 0);              -- ALU result select
        Result   : buffer  std_logic_vector(wordsize - 1 downto 0);   -- ALU result
        Cout     : out     std_logic;                                 -- carry out
        HalfCout : out     std_logic;                                 -- half carry out
        Overflow : out     std_logic;                                 -- signed overflow
        Zero     : out     std_logic;                                 -- result is zero
        Sign     : out     std_logic                                  -- sign of result
    );

end  ALUModel;


architecture  behavioral  of  ALUModel  is
begin

    -- TODO

end  behavioral;


entity  ALU_TB  is
end  ALU_TB;

architecture  testbench  of  ALU_TB  is
begin

    -- TODO

end  testbench;
