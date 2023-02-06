library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library osvvm;
use osvvm.RandomPkg.all;
use osvvm.CoveragePkg.all;
use osvvm.AlertLogPkg.all;

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

    signal RegCov: CoverageIDType;
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


    process
        variable RandRegA, RandRegB: integer range regcnt - 1 downto 0;
        variable Expected: std_logic_vector(wordsize - 1 downto 0);
    begin

        SetTestName("Register Array");
        SetLogEnable(PASSED, true);

        --
        -- single width register tests
        --

        -- clear all registers
        for i in 0 to regcnt - 1 loop
            RegStore <= '1';
            RegIn    <= (others => '0');
            RegInSel <= i;
            wait for 25 ns; -- latch new values

            RegStore <= '0';
            RegASel  <= i;
            wait for 10 ns; -- wait for new output
            Expected := (others => '0');
            AffirmIfEqual(RegA, Expected, "RegA, ");
        end loop;

        -- walking 1's test
        for i in 0 to regcnt - 1 loop
            for j in 0 to wordsize - 1 loop
                RegStore <= '1';
                RegIn    <= (others => '0');
                RegIn(j) <= '1';
                RegInSel <= i;
                wait for 25 ns; -- latch new values

                RegStore <= '0';
                RegASel  <= i;
                wait for 10 ns; -- wait for new output
                Expected    := (others => '0');
                Expected(j) := '1';
                AffirmIfEqual(RegA, Expected, "RegA, ");
            end loop;
        end loop;

        -- set all registers
        for i in 0 to regcnt - 1 loop
            RegStore <= '1';
            RegIn    <= (others => '1');
            RegInSel <= i;
            wait for 25 ns; -- latch new values

            RegStore <= '0';
            RegASel  <= i;
            wait for 10 ns; -- wait for new output
            Expected := (others => '1');
            AffirmIfEqual(RegA, Expected, "RegA, ");
        end loop;

        -- walking 0's test
        for i in 0 to regcnt - 1 loop
            for j in 0 to wordsize - 1 loop
                RegStore <= '1';
                RegIn    <= (others => '1');
                RegIn(j) <= '0';
                RegInSel <= i;
                wait for 25 ns; -- latch new values

                RegStore <= '0';
                RegASel  <= i;
                wait for 10 ns; -- wait for new output
                Expected    := (others => '1');
                Expected(j) := '0';
                AffirmIfEqual(RegA, Expected, "RegA, ");
            end loop;
        end loop;

        -- random access
        -- generate all random pairs of RegASel, RegBSel to make sure
        -- that indexing is correct

        -- value of the register is just the register index number
        for i in 0 to regcnt - 1 loop
            RegStore <= '1';
            RegIn    <= std_logic_vector(to_unsigned(i, RegIn'length));
            RegInSel <= i;
            wait for 25 ns; -- latch new values
        end loop;

        RegCov <= NewId("Register Array (RegA x RegB)");
        wait for 10 ns; -- update RegCov
        AddCross(RegCov,
                 GenBin(0, regcnt - 1),
                 GenBin(0, regcnt - 1));

        while not IsCovered(RegCov) loop

            (RandRegA, RandRegB) := GetRandPoint(RegCov);

            RegStore <= '0';
            RegASel  <= RandRegA;
            RegASel  <= RandRegB;
            wait for 10 ns; -- wait for new output
            Expected := std_logic_vector(to_unsigned(RegASel, Expected'length));
            AffirmIfEqual(RegA, Expected, "RegA, ");
            Expected := std_logic_vector(to_unsigned(RegBSel, Expected'length));
            AffirmIfEqual(RegB, Expected, "RegB, ");

            ICover(RegCov, (RandRegA, RandRegB));

        end loop;

        WriteBin(RegCov);

        --
        -- double width register tests
        --

        -- TODO

        ReportAlerts;

        std.env.stop;

    end process;

end architecture testbench;
