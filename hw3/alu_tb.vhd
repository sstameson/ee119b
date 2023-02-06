---------------------------------------------------------------------------
--
-- ALU Testbench
--
-- This file contains the testbench for the ALU subsystem of the AVR CPU.
-- There is a behavioral implementation of the ALU, which the testbench
-- uses to compare with the structural implementation. The testbench
-- covers all F-Block, add, and shift operations with a wide range
-- of random inputs.
--
---------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ALUConstants.all;
use work.ALU;

architecture behavioral of ALU is
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

    -- compute F-Block result
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

    -- compute add result
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
    Overflow <= (ALUOpA(wordsize - 1) xnor FResult(wordsize - 1)) and
                (ALUOpA(wordsize - 1) xor  AResult(wordsize - 1));

    -- zero flag is set when the result is 0
    Zero <= '1' when Result = (Result'range => '0') else
            '0';

    -- compute the sign flag value
    Sign <= Result(Result'high);

end behavioral;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library osvvm;
use osvvm.RandomPkg.all;
use osvvm.CoveragePkg.all;
use osvvm.AlertLogPkg.all;
use work.ALUConstants.all;

--
-- ALU_TB
--
-- This is a testbench for the ALU that generates randomized inputs
-- and checks them against a behavioral model.
--

entity ALU_TB is
end entity ALU_TB;

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

    signal AddCov, FBlockCov, ShiftCov: CoverageIDType;

    signal AddID, FBlockID, ShiftID: AlertLogIDType;
begin

    -- instantiate Design Under Test
    DUT: entity work.ALU(structural)
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
    MDL: entity work.ALU(behavioral)
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
        variable RandOpA, RandOpB, RandCin,
                 RandCinCmd, RandFCmd, RandSCmd: integer;
    begin

        SetTestName("ALU");
        -- uncomment to log all passing tests
        -- SetLogEnable(PASSED, true);

        FBlockCov <= NewID("ALU F-Block Coverage (OpA x OpB x FCmd)");
        AddCov    <= NewID("ALU Add Coverage (OpA x OpB x Cin x CinCmd x FCmd)");
        ShiftCov  <= NewID("ALU Shift Coverage (OpA x Cin x CinCmd x SCmd)");
        FBlockID  <= NewID("ALU FBlock");
        AddID     <= NewID("ALU Add");
        ShiftID   <= NewID("ALU Shift");
        wait for 1 ns; -- Update IDs

        --
        -- FBlock Testing
        --

        -- check big/small operand for all F-Block modes
        AddCross(FBlockCov, 16,
                 GenBin(0, 2**ALUOpA'length - 1, 4),
                 GenBin(0, 2**ALUOpB'length - 1, 4),
                 GenBin(0, 2**FCmd'length - 1));

        while not IsCovered(FBlockCov) loop

            (RandOpA, RandOpB, RandFCmd) := GetRandPoint(FBlockCov);

            -- test F-Block
            ALUCmd <= ALUCmd_FBLOCK;

            -- generate arbitrary random operand inputs
            ALUOpA <= std_logic_vector(to_unsigned(RandOpA, ALUOpA'Length));
            ALUOpB <= std_logic_vector(to_unsigned(RandOpB, ALUOpA'Length));

            -- test all command types
            FCmd   <= std_logic_vector(to_unsigned(RandFCmd, FCmd'length));

            -- carry and shift should have no effect on F-Block
            Cin    <= 'X';
            CinCmd <= (others => 'X');
            SCmd   <= (others => 'X');

            wait for 1 ns; -- update outputs

            -- check result and relevant output flags
            AffirmIfEqual(FBlockID, dut_Result,   mdl_Result,   "Result, ");
            AffirmIfEqual(FBlockID, dut_Cout,     mdl_Cout,     "Cout, ");
            AffirmIfEqual(FBlockID, dut_Zero,     mdl_Zero,     "Zero, ");
            AffirmIfEqual(FBlockID, dut_Sign,     mdl_Sign,     "Sign, ");

            ICover(FBlockCov, (RandOpA, RandOpB, RandFCmd));

        end loop;

        WriteBin(FBlockCov);

        --
        -- Add Testing
        --

        -- check big/small operands for all carry modes
        -- and either inverting or buffering the second operand
        AddCross(AddCov, 16,
                 GenBin(0, 2**ALUOpA'length - 1, 4),
                 GenBin(0, 2**ALUOpB'length - 1, 4),
                 GenBin(0, 1),
                 GenBin(0, 2**CinCmd'length - 1),
                 GenBin(0, 1));

        while not IsCovered(AddCov) loop

            (RandOpA, RandOpB, RandCin, RandCinCmd, RandFCmd)
                := GetRandPoint(AddCov);

            -- test adder
            ALUCmd <= ALUCmd_ADDER;

            -- generate arbitrary random operand inputs
            ALUOpA <= std_logic_vector(to_unsigned(RandOpA, ALUOpA'Length));
            ALUOpB <= std_logic_vector(to_unsigned(RandOpB, ALUOpA'Length));

            -- generate random carry in and command
            Cin    <= '1' when RandCin = 1 else '0';
            CinCmd <= std_logic_vector(to_unsigned(RandCinCmd, CinCmd'length));

            -- either buffer or invert second operand
            -- inverting is used for subtraction
            FCmd   <= "1010" when RandFCmd = 1 else "0101";

            -- shift command has no effect on adder
            SCmd   <= (others => 'X');

            wait for 1 ns; -- update outputs

            -- check all outputs
            AffirmIfEqual(AddID, dut_Result,   mdl_Result,   "Result, ");
            AffirmIfEqual(AddID, dut_Cout,     mdl_Cout,     "Cout, ");
            AffirmIfEqual(AddID, dut_HalfCout, mdl_HalfCout, "HalfCout, ");
            AffirmIfEqual(AddID, dut_Overflow, mdl_Overflow, "Overflow, ");
            AffirmIfEqual(AddID, dut_Zero,     mdl_Zero,     "Zero, ");
            AffirmIfEqual(AddID, dut_Sign,     mdl_Sign,     "Sign, ");

            ICover(AddCov, (RandOpA, RandOpB, RandCin, RandCinCmd, RandFCmd));

        end loop;

        WriteBin(AddCov);

        --
        -- Shift Testing
        --

        -- check big/small operand for all shift and carry modes
        AddCross(ShiftCov, 16,
                 GenBin(0, 2**ALUOpA'length - 1, 4),
                 GenBin(0, 1),
                 GenBin(0, 2**CinCmd'length - 1),
                 GenBin(0, 2**SCmd'length - 1));

        while not IsCovered(ShiftCov) loop

            (RandOpA, RandCin, RandCinCmd, RandSCmd) := GetRandPoint(ShiftCov);

            -- test shifter
            ALUCmd <= ALUCmd_SHIFT;

            -- generate arbitrary random operand input
            ALUOpA <= std_logic_vector(to_unsigned(RandOpA, ALUOpA'Length));
            -- second operand has no effect on shifter
            ALUOpB <= (others => 'X');

            -- generate random carry in and command
            Cin    <= '1' when RandCin = 1 else '0';
            CinCmd <= std_logic_vector(to_unsigned(RandCinCmd, CinCmd'length));

            -- generate random shift command
            SCmd   <= std_logic_vector(to_unsigned(RandSCmd, SCmd'length));

            -- F-Block command has no effect on shifter
            FCmd   <= (others => 'X');

            wait for 1 ns; -- update outputs

            -- check result and relevant output flags
            AffirmIfEqual(ShiftID, dut_Result,   mdl_Result,   "Result, ");
            AffirmIfEqual(ShiftID, dut_Cout,     mdl_Cout,     "Cout, ");
            AffirmIfEqual(ShiftID, dut_Zero,     mdl_Zero,     "Zero, ");
            AffirmIfEqual(ShiftID, dut_Sign,     mdl_Sign,     "Sign, ");

            ICover(ShiftCov, (RandOpA, RandCin, RandCinCmd, RandSCmd));

        end loop;

        WriteBin(ShiftCov);

        ReportAlerts;

        std.env.stop;
    end process;

end testbench;
