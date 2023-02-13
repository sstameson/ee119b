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

