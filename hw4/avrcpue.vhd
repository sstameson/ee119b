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

        -- ALU control signals
        FCmd   : out std_logic_vector(3 downto 0); -- F-Block operation
        CinCmd : out std_logic_vector(1 downto 0); -- carry in operation
        SCmd   : out std_logic_vector(2 downto 0); -- shift operation
        ALUCmd : out std_logic_vector(1 downto 0); -- ALU result select
        ALUImm : out std_logic_vector(7 downto 0); -- ALU immediate
        OpBSel : out std_logic;                    -- select OpB as reg or imm

        -- status flag control signals
        RegMask : out std_logic_vector(7 downto 0); -- write mask

        -- register control signals
        RegInSel  : out integer  range 31 downto 0; -- register to write
        RegStore  : out std_logic;                  -- register write enable
        RegASel   : out integer  range 31 downto 0; -- register to read on bus A
        RegBSel   : out integer  range 31 downto 0; -- register to read on bus B
        RegDInSel : out integer  range 15 downto 0; -- double-register to write
        RegDStore : out std_logic;                  -- double-register write enable
        RegDSel   : out integer  range 15 downto 0; -- double-register to read

        -- memory interface control signals
        SrcSel      : out integer  range 1 downto 0;     -- address source select bit
        AddrOff     : out std_logic_vector(15 downto 0); -- address offset
        IncDecSel   : out std_logic;                     -- increment/decrement control
        PrePostSel  : out std_logic;                     -- pre/post control

        -- control bus outputs
        DataWr : out std_logic; -- data memory write enable (active low)
        DataRd : out std_logic  -- data memory read enable (active low)
    );

begin
end entity ControlUnit;

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
use work.opcodes.all;


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

    constant C_FLAG: integer := 0;
    constant Z_FLAG: integer := 1;
    constant N_FLAG: integer := 2;
    constant V_FLAG: integer := 3;
    constant S_FLAG: integer := 4;
    constant H_FLAG: integer := 5;
    constant T_FLAG: integer := 6;
    constant I_FLAG: integer := 7;

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
    signal StatIn   : std_logic_vector(wordsize - 1 downto 0); -- data to write to register
    signal StatMask : std_logic_vector(wordsize - 1 downto 0); -- write mask
    --- outputs
    signal StatOut  : std_logic_vector(wordsize - 1 downto 0); -- current register value

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
    -- MemUnit
    --

    -- inputs
    signal AddrSrc    : std_logic_vector(2*addrsize - 1 downto 0);
    signal SrcSel     : integer  range 1 downto 0;
    signal AddrOff    : std_logic_vector(addrsize - 1 downto 0);
    signal IncDecSel  : std_logic;
    signal PrePostSel : std_logic;
    -- outputs
    signal Address    : std_logic_vector(addrsize - 1 downto 0);
    signal AddrSrcOut : std_logic_vector(addrsize - 1 downto 0);
begin

    ALUOpA <= RegA;
    ALUOpB <= RegB when OpBSel = '0' else
              ALUImm;
    Cin    <= StatOut(C_FLAG);
    ALU: entity work.ALU
        port map (
            ALUOpA   => ALUOpA  ,
            ALUOpB   => ALUOpB  ,
            Cin      => Cin     ,
            FCmd     => FCmd    ,
            CinCmd   => CinCmd  ,
            SCmd     => SCmd    ,
            ALUCmd   => ALUCmd  ,
            Result   => Result  ,
            Cout     => Cout    ,
            HalfCout => HalfCout,
            Overflow => Overflow,
            Zero     => Zero    ,
            Sign     => Sign
        );

    FLAGS: entity work.StatusReg
        port map (
            RegIn   => StatIn  ,
            RegMask => StatMask,
            clock   => clock   ,
            RegOut  => StatOut
        );

    REGS: entity work.RegArray
        port map (
            RegIn     => RegIn    ,
            RegInSel  => RegInSel ,
            RegStore  => RegStore ,
            RegASel   => RegASel  ,
            RegBSel   => RegBSel  ,
            RegDIn    => RegDIn   ,
            RegDInSel => RegDInSel,
            RegDStore => RegDStore,
            RegDSel   => RegDSel  ,
            clock     => clock    ,
            RegA      => RegA     ,
            RegB      => RegB     ,
            RegD      => RegD
        );

    MIU: entity work.MemUnit
        generic map (
            srcCnt    => 2,
            offsetCnt => 1
        )
        port map (
            AddrSrc    => AddrSrc   ,
            SrcSel     => SrcSel    ,
            AddrOff    => AddrOff   ,
            OffsetSel  => 0         ,
            IncDecSel  => IncDecSel ,
            IncDecBit  => 0         ,
            PrePostSel => PrePostSel,
            Address    => Address   ,
            AddrSrcOut => AddrSrcOut
        );

end architecture structural;
