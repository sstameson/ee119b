----------------------------------------------------------------------------
--
--  Atmel AVR CPU Entity Declaration
--
--  This is the entity declaration for the complete AVR CPU.  The design
--  should implement this entity to make testing possible.
--
--  Revision History:
--     11 May 98  Glen George       Initial revision.
--      9 May 00  Glen George       Updated comments.
--      7 May 02  Glen George       Updated comments.
--     21 Jan 08  Glen George       Updated comments.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package ControlConstants is

    constant CYCLE1 : std_logic_vector(3 downto 0) := "0001";
    constant CYCLE2 : std_logic_vector(3 downto 0) := "0010";
    constant CYCLE3 : std_logic_vector(3 downto 0) := "0100";
    constant CYCLE4 : std_logic_vector(3 downto 0) := "1000";

    constant RegInMux_ALU : std_logic_vector(1 downto 0) := "00";
    constant RegInMux_IMM : std_logic_vector(1 downto 0) := "01";
    constant RegInMux_REG : std_logic_vector(1 downto 0) := "10";
    constant RegInMux_MEM : std_logic_vector(1 downto 0) := "11";

    constant OpAMux_REGA : std_logic := '0';
    constant OpAMux_ZERO : std_logic := '1';

    constant OpBMux_REGA : std_logic_vector(1 downto 0) := "00";
    constant OpBMux_REGB : std_logic_vector(1 downto 0) := "01";
    constant OpBMux_IMM  : std_logic_vector(1 downto 0) := "10";
    constant OpBMux_TRN  : std_logic_vector(1 downto 0) := "11";

    constant StatusInMux_CLR : std_logic_vector(1 downto 0) := "00";
    constant StatusInMux_SET : std_logic_vector(1 downto 0) := "01";
    constant StatusInMux_TRN : std_logic_vector(1 downto 0) := "10";
    constant StatusInMux_ALU : std_logic_vector(1 downto 0) := "11";

    constant PCMux_INC : std_logic_vector(1 downto 0) := "00";
    constant PCMux_REL : std_logic_vector(1 downto 0) := "01";
    constant PCMux_MEM : std_logic_vector(1 downto 0) := "10";
    constant PCMux_NOP : std_logic_vector(1 downto 0) := "11";

    constant SPMux_NXT : std_logic := '0';
    constant SPMux_NOP : std_logic := '1';

    constant I_FLAG: integer := 7;
    constant T_FLAG: integer := 6;
    constant H_FLAG: integer := 5;
    constant S_FLAG: integer := 4;
    constant V_FLAG: integer := 3;
    constant N_FLAG: integer := 2;
    constant Z_FLAG: integer := 1;
    constant C_FLAG: integer := 0;

    constant FCmd_ZERO : std_logic_vector(3 downto 0) := "0000";
    constant FCmd_ONE  : std_logic_vector(3 downto 0) := "1111";
    constant FCmd_B    : std_logic_vector(3 downto 0) := "1010";
    constant FCmd_NOTB : std_logic_vector(3 downto 0) := "0101";
    constant FCmd_NOTA : std_logic_vector(3 downto 0) := "0011";
    constant FCmd_AND  : std_logic_vector(3 downto 0) := "1000";
    constant FCmd_OR   : std_logic_vector(3 downto 0) := "1110";
    constant FCmd_EOR  : std_logic_vector(3 downto 0) := "0110";

    constant StatusMask_ZERO : std_logic_vector(7 downto 0) := "00000000";
    constant StatusMask_ARITH: std_logic_vector(7 downto 0) := "00111111";
    constant StatusMask_LOGIC: std_logic_vector(7 downto 0) := "00011110";
    constant StatusMask_SHIFT: std_logic_vector(7 downto 0) := "00011111";

    constant DataSrcSel_REG: integer := 0;
    constant DataSrcSel_SP: integer  := 1;
    constant DataSrcSel_MEM: integer := 2;

    constant DataOffsetSel_ZERO : integer := 0;
    constant DataOffsetSel_DISP : integer := 1;

end package ControlConstants;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.MemUnitConstants.all;
use work.ALUConstants.all;
use work.opcodes.all;
use work.ControlConstants.all;

entity ControlUnit is

    port (
        -- program memory data bus
        ProgDB : in  std_logic_vector(15 downto 0);

        -- system clock
        clock  : in  std_logic;

        -- control bus inputs
        Reset  : in  std_logic; -- reset signal (active low)
        INT0   : in  std_logic; -- interrupt signal (active low)
        INT1   : in  std_logic; -- interrupt signal (active low)


        -- datapath mux controls
        OpAMux      : out std_logic; -- select ALUOpA as reg or zero
        OpBMux      : out std_logic_vector(1 downto 0); -- select ALUOpB as reg or imm or T Flag
        StatusInMux : out std_logic_vector(1 downto 0); -- select StatusIn
        RegInMux    : out std_logic_vector(1 downto 0); -- select reg input from datapath
        PCMux       : out std_logic_vector(1 downto 0); -- select next PC
        SPMux       : out std_logic; -- select next SP

        -- decoded values
        DataImm     : out std_logic_vector(7 downto 0); -- ALU immediate
        DataMemDisp : out std_logic_vector(5 downto 0); -- data memory displacement q
        BitIdx      : out integer range 7 downto 0; -- bottom 3 bit index

        -- ALU control signals
        FCmd   : out std_logic_vector(3 downto 0); -- F-Block operation
        CinCmd : out std_logic_vector(1 downto 0); -- carry in operation
        SCmd   : out std_logic_vector(2 downto 0); -- shift operation
        ALUCmd : out std_logic_vector(1 downto 0); -- ALU result select

        -- status flag control signals
        StatusMask : out std_logic_vector(7 downto 0); -- write mask

        -- register control signals
        RegInSel  : out integer  range 31 downto 0; -- register to write
        RegStore  : out std_logic;                  -- register write enable
        RegASel   : out integer  range 31 downto 0; -- register to read on bus A
        RegBSel   : out integer  range 31 downto 0; -- register to read on bus B
        RegDInSel : out integer  range 15 downto 0; -- double-register to write
        RegDStore : out std_logic;                  -- double-register write enable
        RegDSel   : out integer  range 15 downto 0; -- double-register to read

        -- data memory interface control signals
        DataSrcSel     : out integer range 2 downto 0; -- address source select
        DataOffsetSel  : out integer range 1 downto 0; -- address offset select
        DataIncDecSel  : out std_logic;                -- increment/decrement control
        DataPrePostSel : out std_logic;                -- pre/post control

        -- program memory interface control signals
        ProgSrcSel     : out integer range 0 downto 0; -- address source select
        ProgOffsetSel  : out integer range 0 downto 0; -- address offset select
        ProgIncDecSel  : out std_logic;                -- increment/decrement control
        ProgPrePostSel : out std_logic;                -- pre/post control

        -- control bus outputs
        DataWrEn : out std_logic; -- data memory write enable
        DataRdEn : out std_logic  -- data memory read enable
    );

begin
end entity ControlUnit;

architecture dataflow of ControlUnit is
    signal LastCycle : std_logic;
    signal nextstate : std_logic_vector(3 downto 0);
    signal state     : std_logic_vector(3 downto 0);

    signal IR: std_logic_vector(15 downto 0);

    signal R1    : integer range 31 downto 0;
    signal R2    : integer range 31 downto 0;

    signal ChangeStatusIdx : integer range 7 downto 0;
begin

    --
    -- State Machine
    --

    nextstate <= CYCLE1 when LastCycle = '1' else
                 CYCLE2 when state = CYCLE1  else
                 CYCLE3 when state = CYCLE2  else
                 CYCLE4;
    process (clock)
    begin
        if rising_edge(clock) then
            state <= nextstate;
        end if;
    end process;

    --
    -- Instruction Register
    --

    process (clock)
    begin
        if rising_edge(clock) then
            if LastCycle = '1' then
                IR <= ProgDB;
            end if;
        end if;
    end process;

    --
    -- Decoder
    --

    -- decoded immediates
    DataImm     <= "00" & IR(7 downto 6) & IR(3 downto 0)
                       when std_match(IR, OpADIW) or
                            std_match(IR, OpSBIW) else
                   IR(11 downto 8) & IR(3 downto 0);
    DataMemDisp <= IR(13) & IR(11 downto 10) & IR(2 downto 0);
    BitIdx      <= to_integer(unsigned(IR(2 downto 0)));

    -- decoded register indicies
    R1 <= to_integer(unsigned("1" & IR(7 downto 4)))
             when std_match(IR, OpANDI) or
                  std_match(IR, OpCPI)  or
                  std_match(IR, OpORI)  or
                  std_match(IR, OpSBCI) or
                  std_match(IR, OpSUBI) else
          to_integer(unsigned("11" & IR(5 downto 4) & "0"))
             when (std_match(IR, OpADIW) and state = CYCLE1) or
                  (std_match(IR, OpSBIW) and state = CYCLE1) else
          to_integer(unsigned("11" & IR(5 downto 4) & "1"))
             when std_match(IR, OpADIW) or
                  std_match(IR, OpSBIW) else
          to_integer(unsigned(IR(8 downto 4)));
    R2 <= to_integer(unsigned(IR(9) & IR(3 downto 0)));

    -- decoded status reg index
    ChangeStatusIdx <= to_integer(unsigned(IR(6 downto 4)));

    --
    -- Control Signal Generator
    --

    process (all)
    begin

        --
        -- assign control signals for a NOP
        --

        -- default datapath is for a register-register ALU Op
        -- this simplifies the control logic for ALU ops

        -- state machine control signals
        LastCycle <= '1'; -- NOP is only one cycle

        -- datapath mux controls
        OpAMux      <= OpAMux_REGA;
        OpBMux      <= OpBMux_REGB;
        StatusInMux <= StatusInMux_ALU;
        RegInMux    <= RegInMux_ALU;
        PCMux       <= PCMux_INC; -- increment PC
        SPMux       <= SPMux_NOP; -- don't change SP

        -- ALU controls
        FCmd           <= (others => '0');
        CinCmd         <= (others => '0');
        SCmd           <= (others => '0');
        ALUCmd         <= (others => '0');

        -- StatusReg controls
        StatusMask     <= StatusMask_ZERO; -- don't change status flags

        -- RegArray controls
        RegInSel       <= R1;
        RegStore       <= '0'; -- don't change registers
        RegASel        <= R1;
        RegBSel        <= R2;
        RegDInSel      <= 0;
        RegDStore      <= '0'; -- don't change double-registers
        RegDSel        <= 0;

        -- Data MemUnit controls
        DataSrcSel     <= DataSrcSel_REG;
        DataOffsetSel  <= DataOffsetSel_ZERO;
        DataIncDecSel  <= MemUnit_INC;
        DataPrePostSel <= MemUnit_POST;

        -- Program MemUnit controls
        ProgSrcSel     <= 0;
        ProgOffsetSel  <= 0;
        ProgIncDecSel  <= MemUnit_INC;  -- increment PC
        ProgPrePostSel <= MemUnit_POST; -- PC must be post-incremented

        -- control bus outputs
        -- TODO: Should these be registered?
        DataWrEn       <= '0'; -- don't read from memory
        DataRdEn       <= '0'; -- don't write to memory

        --
        -- ALU Opcodes
        --

        if std_match(IR, OpADC) then
            FCmd       <= FCmd_B;
            CinCmd     <= CinCmd_CIN;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpADD) then
            FCmd       <= FCmd_B;
            CinCmd     <= CinCmd_ZERO;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpADIW) then

            if state = CYCLE1 then
                LastCycle  <= '0';
                PCMux      <= PCMux_NOP;

                OpBMux     <= OpBMux_IMM;
                FCmd       <= FCmd_B;
                CinCmd     <= CinCmd_ZERO;
                ALUCmd     <= ALUCmd_ADDER;
                RegStore   <= '1';
                StatusMask <= StatusMask_SHIFT; -- adiw/sbiw have same mask as shift
            end if;

            if state = CYCLE2 then
                FCmd       <= FCmd_ZERO;
                CinCmd     <= CinCmd_CIN;
                ALUCmd     <= ALUCmd_ADDER;
                RegStore   <= '1';
                StatusMask <= StatusMask_SHIFT; -- adiw/sbiw have same mask as shift
            end if;

        end if;

        if std_match(IR, OpAND) then
            FCmd       <= FCmd_AND;
            ALUCmd     <= ALUCmd_FBLOCK;
            RegStore   <= '1';
            StatusMask <= StatusMask_LOGIC;
        end if;

        if std_match(IR, OpANDI) then
            OpBMux     <= OpBMux_IMM;
            FCmd       <= FCmd_AND;
            ALUCmd     <= ALUCmd_FBLOCK;
            RegStore   <= '1';
            StatusMask <= StatusMask_LOGIC;
        end if;

        if std_match(IR, OpASR) then
            SCmd       <= SCmd_ASR;
            ALUCmd     <= ALUCmd_SHIFT;
            RegStore   <= '1';
            StatusMask <= StatusMask_SHIFT;
        end if;

        if std_match(IR, OpBCLR) then
            StatusInMux                 <= StatusInMux_CLR;
            StatusMask(ChangeStatusIdx) <= '1';
        end if;

        if std_match(IR, OpBLD) then
            OpBMux     <= OpBMux_TRN;
            FCmd       <= FCmd_AND;
            ALUCmd     <= ALUCmd_FBLOCK;
            RegStore   <= '1';
        end if;

        if std_match(IR, OpBSET) then
            StatusInMux                 <= StatusInMux_SET;
            StatusMask(ChangeStatusIdx) <= '1';
        end if;

        if std_match(IR, OpBST) then
            StatusInMux        <= StatusInMux_TRN;
            StatusMask(T_FLAG) <= '1';
        end if;

        if std_match(IR, OpCOM) then
            FCmd       <= FCmd_NOTA;
            ALUCmd     <= ALUCmd_FBLOCK;
            RegStore   <= '1';
            StatusMask <= StatusMask_LOGIC;
        end if;

        if std_match(IR, OpCP) then
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_ONE;
            ALUCmd     <= ALUCmd_ADDER;
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpCPC) then
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_CINBAR;
            ALUCmd     <= ALUCmd_ADDER;
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpCPI) then
            OpBMux     <= OpBMux_IMM;
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_ONE;
            ALUCmd     <= ALUCmd_ADDER;
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpDEC) then
            FCmd       <= FCmd_ONE;
            CinCmd     <= CinCmd_ZERO;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpEOR) then
            FCmd       <= FCmd_EOR;
            ALUCmd     <= ALUCmd_FBLOCK;
            RegStore   <= '1';
            StatusMask <= StatusMask_LOGIC;
        end if;

        if std_match(IR, OpINC) then
            FCmd       <= FCmd_ZERO;
            CinCmd     <= CinCmd_ONE;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpLSR) then
            SCmd       <= SCmd_LSR;
            ALUCmd     <= ALUCmd_SHIFT;
            RegStore   <= '1';
            StatusMask <= StatusMask_SHIFT;
        end if;

        if std_match(IR, OpNEG) then
            OpAMux     <= OpAMux_ZERO;
            OpBMux     <= OpBMux_REGA;
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_ONE;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpOR) then
            FCmd       <= FCmd_OR;
            ALUCmd     <= ALUCmd_FBLOCK;
            RegStore   <= '1';
            StatusMask <= StatusMask_LOGIC;
        end if;

        if std_match(IR, OpORI) then
            OpBMux     <= OpBMux_IMM;
            FCmd       <= FCmd_OR;
            ALUCmd     <= ALUCmd_FBLOCK;
            RegStore   <= '1';
            StatusMask <= StatusMask_LOGIC;
        end if;

        if std_match(IR, OpROR) then
            SCmd       <= SCmd_ROR;
            ALUCmd     <= ALUCmd_SHIFT;
            RegStore   <= '1';
            StatusMask <= StatusMask_SHIFT;
        end if;

        if std_match(IR, OpSBC) then
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_CINBAR;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpSBCI) then
            OpBMux     <= OpBMux_IMM;
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_CINBAR;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpSBIW) then

            if state = CYCLE1 then
                LastCycle  <= '0';
                PCMux      <= PCMux_NOP;

                OpBMux     <= OpBMux_IMM;
                FCmd       <= FCmd_NOTB;
                CinCmd     <= CinCmd_ONE;
                ALUCmd     <= ALUCmd_ADDER;
                RegStore   <= '1';
                StatusMask <= StatusMask_SHIFT; -- adiw/sbiw have same mask as shift
            end if;

            if state = CYCLE2 then
                FCmd       <= FCmd_ONE;
                CinCmd     <= CinCmd_CIN;
                ALUCmd     <= ALUCmd_ADDER;
                RegStore   <= '1';
                StatusMask <= StatusMask_SHIFT; -- adiw/sbiw have same mask as shift
            end if;

        end if;

        if std_match(IR, OpSUB) then
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_ONE;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpSUBI) then
            OpBMux     <= OpBMux_IMM;
            FCmd       <= FCmd_NOTB;
            CinCmd     <= CinCmd_ONE;
            ALUCmd     <= ALUCmd_ADDER;
            RegStore   <= '1';
            StatusMask <= StatusMask_ARITH;
        end if;

        if std_match(IR, OpSWAP) then
            SCmd       <= SCmd_SWAP;
            ALUCmd     <= ALUCmd_SHIFT;
            RegStore   <= '1';
            StatusMask <= StatusMask_SHIFT;
        end if;

    end process;

end architecture dataflow;

--
--  AVR_CPU
--
--  This is the complete entity declaration for the AVR CPU.  It is used to
--  test the complete design.
--
--  Inputs:
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    INT0   - active low interrupt
--    INT1   - active low interrupt
--    clock  - the system clock
--
--  Outputs:
--    ProgAB - program memory address bus (16 bits)
--    DataAB - data memory address bus (16 bits)
--    DataWr - data write signal
--    DataRd - data read signal
--
--  Inputs/Outputs:
--    DataDB - data memory data bus (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.opcodes.all;
use work.MemUnitConstants.all;
use work.ControlConstants.all;


entity  AVR_CPU  is

    port (
        ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
        Reset   :  in     std_logic;                       -- reset signal (active low)
        INT0    :  in     std_logic;                       -- interrupt signal (active low)
        INT1    :  in     std_logic;                       -- interrupt signal (active low)
        clock   :  in     std_logic;                       -- system clock
        ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
        DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
        DataWr  :  out    std_logic;                       -- data memory write enable (active low)
        DataRd  :  out    std_logic;                       -- data memory read enable (active low)
        DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
    );

end  AVR_CPU;

architecture structural of AVR_CPU is

    constant wordsize: integer := 8;
    constant addrsize: integer := 16;
    constant regcnt: integer   := 32;

    --
    -- Decoded Immediates
    --
    signal DataImm     : std_logic_vector(wordsize - 1 downto 0); -- ALU immediate value
    signal DataMemDisp : std_logic_vector(5 downto 0); -- memory displacement q
    signal BitIdx      : integer range 7 downto 0; -- bottom 3 bit index

    --
    -- Datapath Mux Controls
    --

    signal OpAMux      : std_logic;
    signal OpBMux      : std_logic_vector(1 downto 0);
    signal StatusInMux : std_logic_vector(1 downto 0);
    signal RegInMux    : std_logic_vector(1 downto 0);
    signal PCMux       : std_logic_vector(1 downto 0);
    signal SPMux       : std_logic;

    --
    -- Control Bus Outputs
    --

    signal DataWrEn : std_logic;
    signal DataRdEn : std_logic;

    --
    -- ALU
    --

    -- inputs
    signal ALUOpA   : std_logic_vector(wordsize - 1 downto 0); -- first operand
    signal ALUOpB   : std_logic_vector(wordsize - 1 downto 0); -- second operand
    signal Cin      : std_logic;                    -- carry in
    signal FCmd     : std_logic_vector(3 downto 0); -- F-Block operation
    signal CinCmd   : std_logic_vector(1 downto 0); -- carry in operation
    signal SCmd     : std_logic_vector(2 downto 0); -- shift operation
    signal ALUCmd   : std_logic_vector(1 downto 0); -- ALU result select
    -- outputs
    signal Result   : std_logic_vector(wordsize - 1 downto 0); -- ALU result
    signal Cout     : std_logic;                    -- carry out
    signal HalfCout : std_logic;                    -- half carry out
    signal Overflow : std_logic;                    -- signed overflow
    signal Zero     : std_logic;                    -- result is zero
    signal Sign     : std_logic;                    -- sign of result

    --
    -- StatusReg
    --

    -- inputs
    signal StatusIn   : std_logic_vector(wordsize - 1 downto 0); -- data to write to register
    signal StatusMask : std_logic_vector(wordsize - 1 downto 0); -- write mask
    --- outputs
    signal StatusOut  : std_logic_vector(wordsize - 1 downto 0); -- current register value

    --
    -- RegArray
    --

    -- inputs
    signal RegIn     : std_logic_vector(wordsize - 1 downto 0);
    signal RegInSel  : integer  range regcnt - 1 downto 0;
    signal RegStore  : std_logic;
    signal RegASel   : integer  range regcnt - 1 downto 0;
    signal RegBSel   : integer  range regcnt - 1 downto 0;
    signal RegDIn    : std_logic_vector(2 * wordsize - 1 downto 0);
    signal RegDInSel : integer  range regcnt/2 - 1 downto 0;
    signal RegDStore : std_logic;
    signal RegDSel   : integer  range regcnt/2 - 1 downto 0;
    -- outputs
    signal RegA      : std_logic_vector(wordsize - 1 downto 0);
    signal RegB      : std_logic_vector(wordsize - 1 downto 0);
    signal RegD      : std_logic_vector(2 * wordsize - 1 downto 0);

    --
    -- Data MemUnit
    --

    -- inputs
    signal DataAddrSrc    : std_logic_vector(3*addrsize - 1 downto 0);
    signal DataAddrOff    : std_logic_vector(2*addrsize - 1 downto 0);
    signal DataSrcSel     : integer range 2 downto 0;
    signal DataOffsetSel  : integer range 1 downto 0;
    signal DataIncDecSel  : std_logic;
    signal DataPrePostSel : std_logic;
    -- outputs
    signal DataAddress    : std_logic_vector(addrsize - 1 downto 0);
    signal DataAddrSrcOut : std_logic_vector(addrsize - 1 downto 0);

    --
    -- Program MemUnit
    --

    -- inputs
    signal ProgAddrSrc    : std_logic_vector(addrsize - 1 downto 0);
    signal ProgAddrOff    : std_logic_vector(addrsize - 1 downto 0);
    signal ProgSrcSel     : integer range 0 downto 0;
    signal ProgOffsetSel  : integer range 0 downto 0;
    signal ProgIncDecSel  : std_logic;
    signal ProgPrePostSel : std_logic;
    -- outputs
    signal ProgAddress    : std_logic_vector(addrsize - 1 downto 0);
    signal ProgAddrSrcOut : std_logic_vector(addrsize - 1 downto 0);

    -- stack pointer
    signal SP: std_logic_vector(addrsize - 1 downto 0);

    -- program counter
    signal PC: std_logic_vector(addrsize - 1 downto 0);
begin
    -- control bus outputs
    ProgAB <= ProgAddress;
    DataAB <= DataAddress;
    DataWr <= not DataWrEn;
    DataRd <= not DataRdEn;
    DataDB <= RegA when DataWrEn = '1' else
              (others => 'Z');

    ALUOpA <= RegA when OpAMux = OpAMux_REGA else
              (others => '0');
    ALUOpB <= RegA    when OpBMux = OpBMux_REGA else
              RegB    when OpBMux = OpBMux_REGB else
              DataImm when OpBMux = OpBMux_IMM else
              (wordsize - 1 downto BitIdx + 1 => '1') &
              (BitIdx                         => StatusOut(T_FLAG)) &
              (BitIdx - 1   downto 0          => '1');
    Cin    <= StatusOut(C_FLAG);
    ALU: entity work.ALU
        port map (
            -- datapath inputs
            ALUOpA   => ALUOpA  ,
            ALUOpB   => ALUOpB  ,
            Cin      => Cin     ,
            -- datapath outputs
            Result   => Result  ,
            Cout     => Cout    ,
            HalfCout => HalfCout,
            Overflow => Overflow,
            Zero     => Zero    ,
            Sign     => Sign    ,
            -- controls
            FCmd     => FCmd    ,
            CinCmd   => CinCmd  ,
            SCmd     => SCmd    ,
            ALUCmd   => ALUCmd
        );

    StatusIn <= (others => '0') when StatusInMux = StatusInMux_CLR else
                (others => '1') when StatusInMux = StatusInMux_SET else
                "0" & RegA(BitIdx) & "000000"
                                when StatusInMux = StatusInMux_TRN else
                "00" & HalfCout &
                (Sign xor Overflow) &
                Overflow &
                Sign &
                Zero &
                Cout;
    FLAGS: entity work.StatusReg
        port map (
            -- datapath inputs
            RegIn   => StatusIn  ,
            -- datapath outputs
            RegOut  => StatusOut ,
            -- controls
            RegMask => StatusMask,
            clock   => clock
        );

    RegIn  <= Result  when RegInMux = RegInMux_ALU else
              DataImm when RegInMux = RegInMux_IMM else
              RegB    when RegInMux = RegInMux_REG else
              DataDB;
    RegDIn <= DataAddrSrcOut;
    REGS: entity work.RegArray
        port map (
            -- datapath inputs
            RegIn     => RegIn    ,
            RegDIn    => RegDIn   ,
            -- datapath outputs
            RegA      => RegA     ,
            RegB      => RegB     ,
            RegD      => RegD     ,
            -- controls
            RegInSel  => RegInSel ,
            RegStore  => RegStore ,
            RegASel   => RegASel  ,
            RegBSel   => RegBSel  ,
            RegDInSel => RegDInSel,
            RegDStore => RegDStore,
            RegDSel   => RegDSel  ,
            clock     => clock
        );

    process (clock)
    begin
        if rising_edge(clock) then
            if Reset = '0' then
                SP <= (others => '1');
            else
                case SPMux is
                    when SPMux_NXT => SP <= DataAddrSrcOut;
                    when others    => SP <= SP;
                end case;
            end if;
        end if;
    end process;

    -- X, Y, or Z reg
    DataAddrSrc(1*addrsize - 1 downto 0*addrsize)
        <= RegD;
    -- stack pointer
    DataAddrSrc(2*addrsize - 1 downto 1*addrsize)
        <= SP;
    -- second word of instruction
    DataAddrSrc(3*addrsize - 1 downto 2*addrsize)
        <= ProgDB;
    -- zero offset
    DataAddrOff(1*addrsize - 1 downto 0*addrsize)
        <= (others => '0');
    -- memory displacement for ldd and std
    DataAddrOff(2*addrsize - 1 downto 1*addrsize)
        <= (addrsize - 1 downto DataMemDisp'length => '0') & DataMemDisp;
    DATA_MAU: entity work.MemUnit
        generic map (
            srcCnt    => 3,
            offsetCnt => 2
        )
        port map (
            -- datapath inputs
            AddrSrc    => DataAddrSrc   ,
            AddrOff    => DataAddrOff   ,
            -- datapath outputs
            Address    => DataAddress   ,
            AddrSrcOut => DataAddrSrcOut,
            -- controls
            SrcSel     => DataSrcSel    ,
            OffsetSel  => DataOffsetSel ,
            IncDecSel  => DataIncDecSel ,
            IncDecBit  => 0             ,
            PrePostSel => DataPrePostSel
        );

    process (clock)
    begin
        if rising_edge(clock) then
            if Reset = '0' then
                PC <= (others => '0');
            else
                case PCMux is
                    when PCMux_INC => PC <= ProgAddrSrcOut;
                    when PCMux_REL => PC <= ProgAddress;
                    when PCMux_MEM => PC <= ProgDB;
                    when others    => PC <= PC;
                end case;
            end if;
        end if;
    end process;

    ProgAddrSrc <= PC;
    -- TODO: need to decode different offsets for PC
    ProgAddrOff <= (others => '0');
    PROG_MAU: entity work.MemUnit
        generic map (
            srcCnt    => 1,
            offsetCnt => 1
        )
        port map (
            -- datapath inputs
            AddrSrc    => ProgAddrSrc   ,
            AddrOff    => ProgAddrOff   ,
            -- datapath outputs
            Address    => ProgAddress   ,
            AddrSrcOut => ProgAddrSrcOut,
            -- controls
            SrcSel     => ProgSrcSel    ,
            OffsetSel  => ProgOffsetSel ,
            IncDecSel  => ProgIncDecSel ,
            IncDecBit  => 0             ,
            PrePostSel => ProgPrePostSel
        );

    CONTROL: entity work.ControlUnit
        port map (
            -- control bus inputs
            ProgDB         => ProgDB        ,
            clock          => clock         ,
            Reset          => Reset         ,
            INT0           => INT0          ,
            INT1           => INT1          ,

            -- decoded immediates
            DataImm        => DataImm       ,
            BitIdx         => BitIdx        ,
            DataMemDisp    => DataMemDisp   ,

            -- datapath mux controls
            OpAMux         => OpAMux        ,
            OpBMux         => OpBMux        ,
            StatusInMux    => StatusInMux   ,
            RegInMux       => RegInMux      ,
            PCMux          => PCMux         ,
            SPMux          => SPMux         ,

            -- ALU controls
            FCmd           => FCmd          ,
            CinCmd         => CinCmd        ,
            SCmd           => SCmd          ,
            ALUCmd         => ALUCmd        ,

            -- StatusReg controls
            StatusMask     => StatusMask    ,

            -- RegArray controls
            RegInSel       => RegInSel      ,
            RegStore       => RegStore      ,
            RegASel        => RegASel       ,
            RegBSel        => RegBSel       ,
            RegDInSel      => RegDInSel     ,
            RegDStore      => RegDStore     ,
            RegDSel        => RegDSel       ,

            -- Data MemUnit controls
            DataSrcSel     => DataSrcSel    ,
            DataOffsetSel  => DataOffsetSel ,
            DataIncDecSel  => DataIncDecSel ,
            DataPrePostSel => DataPrePostSel,

            -- Program MemUnit Controls
            ProgSrcSel     => ProgSrcSel    ,
            ProgOffsetSel  => ProgOffsetSel ,
            ProgIncDecSel  => ProgIncDecSel ,
            ProgPrePostSel => ProgPrePostSel,

            -- control bus outputs
            DataWrEn       => DataWrEn      ,
            DataRdEn       => DataRdEn
        );

end architecture structural;
