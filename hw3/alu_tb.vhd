--
--  ALUModel
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
use ieee.numeric_std.all;
use work.ALUConstants.all;

entity ALUModel is

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

end ALUModel;

architecture behavioral of ALUModel is
    constant halfsize: integer := wordsize / 2;

    signal FResult: std_logic_vector(wordsize - 1 downto 0);

    signal carry: std_logic;
    signal sum: std_logic_vector(wordsize downto 0);
    signal halfsum: std_logic_vector(halfsize downto 0);
    signal AResult: std_logic_vector(wordsize - 1 downto 0);
    signal ACout: std_logic;

    signal SResult: std_logic_vector(wordsize - 1 downto 0);
    signal SCout: std_logic;
begin

    -- compute FBlock
    FBlock: for i in FResult'range generate
        FResult(i) <= FCmd(0) when ALUOpA(i) = '0' and ALUOpB(i) = '0' else
                      FCmd(1) when ALUOpA(i) = '0' and ALUOpB(i) = '1' else
                      FCmd(2) when ALUOpA(i) = '1' and ALUOpB(i) = '0' else
                      FCmd(3) when ALUOpA(i) = '1' and ALUOpB(i) = '1' else
                      'X';
    end generate FBlock;

    -- get the carry in based on CinCmd
    carry <= '0'     when CinCmd = CinCmd_ZERO else
             '1'     when CinCmd = CinCmd_ONE else
             Cin     when CinCmd = CinCmd_CIN else
             not Cin when CinCmd = CinCmd_CINBAR else
             'X';

    sum <=
        std_logic_vector(unsigned("0" & ALUOpA) + unsigned("0" & FResult))
            when carry = '0' else
        std_logic_vector(unsigned("0" & ALUOpA) + unsigned("0" & FResult) + 1);

    halfsum <=
        std_logic_vector(unsigned("0" & ALUOpA(halfsize - 1 downto 0)) +
                         unsigned("0" & FResult(halfsize - 1 downto 0)))
            when carry = '0' else
        std_logic_vector(unsigned("0" & ALUOpA(halfsize - 1 downto 0)) +
                         unsigned("0" & FResult(halfsize - 1 downto 0)) + 1);

    AResult <= sum(wordsize - 1 downto 0);

    ACout <= sum(wordsize);

    -- compute shift result
    SResult <=
        ALUOpA sll 1                             when SCmd = SCmd_LSL  else
        ALUOpA(halfsize - 1 downto 0) &
            ALUOpA(wordsize - 1 downto halfsize) when SCmd = SCmd_SWAP else
        ALUOpA rol 1                             when SCmd = SCmd_ROL  else
        ALUOpA(wordsize - 2 downto 0) & Cin      when SCmd = SCmd_RLC  else
        ALUOpA srl 1                             when SCmd = SCmd_LSR  else
        std_logic_vector(signed(ALUOpA) sra 1)   when SCmd = SCmd_ASR  else
        ALUOpA ror 1                             when SCmd = SCmd_ROR  else
        Cin & ALUOpA(wordsize - 1 downto 1)      when SCmd = SCmd_RRC  else
        (others => 'X');

    -- compute the carry out of shift
    SCout  <= ALUOpA(0)            when std_match(SCmd, SCmd_RIGHT) else
              ALUOpA(wordsize - 1) when std_match(SCmd, SCmd_LEFT)  else
              'X';

    -- select correct result
    Result <= FResult when ALUCmd = ALUCmd_FBLOCK else
              AResult when ALUCmd = ALUCmd_ADDER  else
              SResult when ALUCmd = ALUCmd_SHIFT  else
              (others => 'X');  -- unknown command

    -- select the carry out
    COut <= '0'   when ALUCmd = ALUCmd_FBLOCK else
            ACout when ALUCmd = ALUCmd_ADDER  else
            SCout when ALUCmd = ALUCmd_SHIFT  else
            'X';  -- unknown command

    HalfCout <= halfsum(halfsize);

    -- overflow if sign of two inputs are the same
    -- and don't match add result
    Overflow <= (ALUOpA(wordsize - 1) xnor ALUOpB(wordsize - 1)) and
                (ALUOpA(wordsize - 1) xor  AResult(wordsize - 1));

    -- zero flag is set when the result is 0
    Zero <= '1' when Result = (Result'range => '0') else
            '0';

    -- compute the sign flag value
    Sign <= Result(Result'high);

end behavioral;


library ieee;
use ieee.std_logic_1164.all;
use work.ALUConstants.all;

library osvvm;

--
-- ALU_TB
--
-- This is a testbench for the ALU that generates randomized inputs
-- and checks them against a behavioral model.
--

entity ALU_TB is
end ALU_TB;

architecture testbench of ALU_TB is
    constant wordsize : integer := 8;

    -- stimulus signals for DUT and MDL
    signal ALUOpA   :  std_logic_vector(wordsize - 1 downto 0);   -- first operand
    signal ALUOpB   :  std_logic_vector(wordsize - 1 downto 0);   -- second operand
    signal Cin      :  std_logic;                                 -- carry in
    signal FCmd     :  std_logic_vector(3 downto 0);              -- F-Block operation
    signal CinCmd   :  std_logic_vector(1 downto 0);              -- carry in operation
    signal SCmd     :  std_logic_vector(2 downto 0);              -- shift operation
    signal ALUCmd   :  std_logic_vector(1 downto 0);              -- ALU result select

    -- Design Under Test (DUT) observed signals
    signal dut_Result   :  std_logic_vector(wordsize - 1 downto 0);   -- ALU result
    signal dut_Cout     :  std_logic;                                 -- carry out
    signal dut_HalfCout :  std_logic;                                 -- half carry out
    signal dut_Overflow :  std_logic;                                 -- signed overflow
    signal dut_Zero     :  std_logic;                                 -- result is zero
    signal dut_Sign     :  std_logic;                                 -- sign of result

    -- Behavioral Model (MDL) observed signals
    signal mdl_Result   :  std_logic_vector(wordsize - 1 downto 0);   -- ALU result
    signal mdl_Cout     :  std_logic;                                 -- carry out
    signal mdl_HalfCout :  std_logic;                                 -- half carry out
    signal mdl_Overflow :  std_logic;                                 -- signed overflow
    signal mdl_Zero     :  std_logic;                                 -- result is zero
    signal mdl_Sign     :  std_logic;                                 -- sign of result
begin

    -- instantiate Design Under Test
    DUT: entity work.ALU
        generic map (
            wordsize => wordsize
        )
        port map (
            ALUOpA   => ALUOpA,
            ALUOpB   => ALUOpB,
            Cin      => Cin,
            FCmd     => FCmd,
            CinCmd   => CinCmd,
            SCmd     => SCmd,
            ALUCmd   => ALUCmd,
            Result   => dut_Result,
            Cout     => dut_Cout,
            HalfCout => dut_HalfCout,
            Overflow => dut_Overflow,
            Zero     => dut_Zero,
            Sign     => dut_Sign
        );

    -- instantiate Behavioral Model
    MDL: entity work.ALUModel
        generic map (
            wordsize => wordsize
        )
        port map (
            ALUOpA   => ALUOpA,
            ALUOpB   => ALUOpB,
            Cin      => Cin,
            FCmd     => FCmd,
            CinCmd   => CinCmd,
            SCmd     => SCmd,
            ALUCmd   => ALUCmd,
            Result   => mdl_Result,
            Cout     => mdl_Cout,
            HalfCout => mdl_HalfCout,
            Overflow => mdl_Overflow,
            Zero     => mdl_Zero,
            Sign     => mdl_Sign
        );

    process
        variable i: integer := 0;
    begin

        while i < 10 loop
            ALUOpA <= X"7F";
            ALUOpB <= X"01";
            Cin    <= '0';
            FCmd   <= "1010"; -- B
            CinCmd <= CinCmd_CIN;
            SCmd   <= SCmd_LEFT;
            ALUCmd <= ALUCmd_ADDER;

            report "iteration " & to_string(i);
            i := i + 1;

            wait for 10 ns;

        end loop;

        std.env.stop;
    end process;


end testbench;
