library ieee;
use ieee.std_logic_1164.all;

entity RegArray_TB is
end entity RegArray_TB;

architecture testbench of RegArray_TB is
    constant regcnt   : integer := 32;    -- default number of registers is 32
    constant wordsize : integer := 8;     -- default width is 8-bits

    -- stimulus signals for Design Under Test (DUT)
    signal RegIn     : std_logic_vector(wordsize - 1 downto 0);
    signal RegInSel  : integer  range regcnt - 1 downto 0;
    signal RegStore  : std_logic;
    signal RegASel   : integer  range regcnt - 1 downto 0;
    signal RegBSel   : integer  range regcnt - 1 downto 0;
    signal RegDIn    : std_logic_vector(2 * wordsize - 1 downto 0);
    signal RegDInSel : integer  range regcnt/2 - 1 downto 0;
    signal RegDStore : std_logic;
    signal RegDSel   : integer  range regcnt/2 - 1 downto 0;
    signal clock     : std_logic;

    -- output signals for Design Under Test (DUT)
    signal RegA      : std_logic_vector(wordsize - 1 downto 0);
    signal RegB      : std_logic_vector(wordsize - 1 downto 0);
    signal RegD      : std_logic_vector(2 * wordsize - 1 downto 0);
begin

    DUT: entity work.RegArray
        generic map (
            regcnt   => regcnt,
            wordsize => wordsize
        )
        port map (
            RegIn     => RegIn,
            RegInSel  => RegInSel,
            RegStore  => RegStore,
            RegASel   => RegASel,
            RegBSel   => RegBSel,
            RegDIn    => RegDIn,
            RegDInSel => RegDInSel,
            RegDStore => RegDStore,
            RegDSel   => RegDSel,
            clock     => clock,
            RegA      => RegA,
            RegB      => RegB,
            RegD      => RegD
        );

    -- generate clock signal
    process
    begin
        clock <= '0';
        wait for 10 ns;

        clock <= '1';
        wait for 10 ns;
    end process;

    --
    -- single width register tests
    --

    process
    begin

        for i in 0 to 10 loop

            RegIn <= X"AB";
            wait for 20 ns;

        end loop;

        std.env.stop;

    end process;

    -- clear all registers

    -- walking 1's test

    -- set all registers

    -- walking 0's test

    --
    -- double width register tests
    --

end architecture testbench;
