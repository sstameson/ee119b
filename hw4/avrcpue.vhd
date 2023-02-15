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

    constant RegDInMux_SRC : std_logic := '0';
    constant RegDInMux_ADR : std_logic := '1';

    constant OpBMux_REG : std_logic := '0';
    constant OpBMux_IMM : std_logic := '1';

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

    constant DataOffsetSel_ZERO : integer := 0;
    constant DataOffsetSel_K    : integer := 1;
    constant DataOffsetSel_KBAR : integer := 2;
    constant DataOffsetSel_Q    : integer := 3;

    constant DataSrcSel_REG: integer := 0;
    constant DataSrcSel_SP: integer  := 1;
    constant DataSrcSel_MEM: integer := 2;

end package ControlConstants;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.std_match;
use work.MemUnitConstants.all;
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
        OpBMux      : out std_logic; -- select ALUOpB as reg or imm
        StatusInMux : out std_logic_vector(1 downto 0); -- select StatusIn
        RegInMux    : out std_logic_vector(1 downto 0); -- select reg input from datapath
        RegDInMux   : out std_logic; -- select double reg input from datapath
        PCMux       : out std_logic_vector(1 downto 0); -- select next PC
        SPMux       : out std_logic; -- select next SP

        -- decoded values
        DataImm : out std_logic_vector(7 downto 0); -- ALU immediate
        BitIdx  : out std_logic_vector(2 downto 0); -- T flag bit index
        WordImm : out std_logic_vector(5 downto 0); -- word immediate (adiw/sbiw)
        MemDisp : out std_logic_vector(5 downto 0); -- memory displacement q

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
        DataOffsetSel  : out integer range 3 downto 0; -- address offset select
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
begin

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

    process (clock)
    begin
        if rising_edge(clock) then
            if LastCycle = '1' then
                IR <= ProgDB;
            end if;
        end if;
    end process;

    -- decoded immediates
    DataImm <= IR(11 downto 8) & IR(3 downto 0);
    BitIdx  <= IR(2 downto 0);
    WordImm <= IR(7 downto 6) & IR(3 downto 0);
    MemDisp <= IR(13) & IR(11 downto 10) & IR(2 downto 0);

    process (all)
    begin

        --
        -- assign control signals for a NOP
        --

        -- state machine control signals
        LastCycle <= '1'; -- NOP is only one cycle

        -- datapath mux controls
        OpBMux      <= '0';
        StatusInMux <= (others => '0');
        RegInMux    <= (others => '0');
        RegDInMux   <= '0';
        PCMux       <= PCMux_INC;
        SPMux       <= SPMux_NOP;

        -- ALU controls
        FCmd           <= (others => '0');
        CinCmd         <= (others => '0');
        SCmd           <= (others => '0');
        ALUCmd         <= (others => '0');

        -- StatusReg controls
        StatusMask     <= (others => '0'); -- don't change status flags

        -- RegArray controls
        RegInSel       <= 0;
        RegStore       <= '0'; -- don't change registers
        RegASel        <= 0;
        RegBSel        <= 0;
        RegDInSel      <= 0;
        RegDStore      <= '0'; -- don't change double-registers
        RegDSel        <= 0;

        -- Data MemUnit controls
        DataSrcSel     <= 0;
        DataOffsetSel  <= 0;
        DataIncDecSel  <= '0';
        DataPrePostSel <= '0';

        -- Program MemUnit controls
        ProgSrcSel     <= 0;
        ProgOffsetSel  <= 0;
        ProgIncDecSel  <= MemUnit_INC;
        ProgPrePostSel <= MemUnit_POST;

        -- control bus outputs
        DataWrEn       <= '0'; -- don't read from memory
        DataRdEn       <= '0'; -- don't write to memory

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
    signal DataImm : std_logic_vector(wordsize - 1 downto 0); -- ALU immediate value
    signal BitIdx  : std_logic_vector(2 downto 0); -- T flag bit index
    signal WordImm : std_logic_vector(5 downto 0); -- word immediate (adiw/sbiw)
    signal MemDisp : std_logic_vector(5 downto 0); -- memory displacement q

    --
    -- Datapath Mux Controls
    --

    signal OpBMux      : std_logic;
    signal StatusInMux : std_logic_vector(1 downto 0);
    signal RegInMux    : std_logic_vector(1 downto 0);
    signal RegDInMux   : std_logic;
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
    signal DataAddrOff    : std_logic_vector(4*addrsize - 1 downto 0);
    signal DataSrcSel     : integer range 2 downto 0;
    signal DataOffsetSel  : integer range 3 downto 0;
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

    ALUOpA <= RegA;
    ALUOpB <= RegB when OpBMux = OpBMux_REG else
              DataImm;
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
                "0" & RegA(to_integer(unsigned(BitIdx))) & "000000"
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
    RegDIn <= DataAddrSrcOut when RegDInMux = RegDInMux_SRC else
              DataAddress;
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
    -- word immediate for adiw
    DataAddrOff(2*addrsize - 1 downto 1*addrsize)
        <= (addrsize - 1 downto WordImm'length => '0') & WordImm;
    -- word immediate for sbiw
    DataAddrOff(3*addrsize - 1 downto 2*addrsize)
        <= not DataAddrOff(2*addrsize - 1 downto 1*addrsize);
    -- memory displacement for ldd and std
    DataAddrOff(4*addrsize - 1 downto 3*addrsize)
        <= (addrsize - 1 downto MemDisp'length => '0') & MemDisp;
    DATA_MAU: entity work.MemUnit
        generic map (
            srcCnt    => 3,
            offsetCnt => 4
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
            WordImm        => WordImm       ,
            MemDisp        => MemDisp       ,

            -- datapath mux controls
            OpBMux         => OpBMux        ,
            StatusInMux    => StatusInMux   ,
            RegInMux       => RegInMux      ,
            RegDInMux      => RegDInMux     ,
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
