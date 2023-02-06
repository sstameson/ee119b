---------------------------------------------------------------------------
--
-- Status Register Testbench
--
-- This file contains a testbench for the status register subsystem of the
-- AVR cpu. The testbench iterates through a walking 1's and walking 0's
-- pattern on the input mask and tests all four bit transitions from
-- 0 --> 0, 0 --> 1, 1 --> 1, and 1 --> 0.
-- 
---------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library osvvm;
use osvvm.RandomPkg.all;
use osvvm.AlertLogPkg.all;

entity StatusReg_TB is
end entity StatusReg_TB;

architecture testbench of StatusReg_TB is
    constant wordsize : integer := 8;

    -- stimulus signals for Design Under Test (DUT)
    signal RegIn   : std_logic_vector(wordsize - 1 downto 0);   -- data to write to register
    signal RegMask : std_logic_vector(wordsize - 1 downto 0);   -- write mask
    signal clock   : std_logic;                                 -- system clock

    -- output signals for Design Under Test (DUT)
    signal RegOut  : std_logic_vector(wordsize - 1 downto 0);   -- current register value
begin

    DUT: entity work.StatusReg
        generic map (
            wordsize => wordsize
        )
        port map (
            RegIn   => RegIn,
            RegMask => RegMask,
            clock   => clock,
            RegOut  => RegOut
        );

    -- generate clock signal
    process
    begin
        clock <= '0';
        wait for 10 ns;

        clock <= '1';
        wait for 10 ns;
    end process;

    -- generate register input/mask signals and check output
    process
        variable Transitions: std_logic_vector(3 downto 0) := "0110";
        variable Expected: std_logic_vector(wordsize - 1 downto 0);
        variable RV: RandomPType;
    begin

        SetTestName("Status Register");
        SetLogEnable(PASSED, true);

        RegIn    <= (others => '0');
        RegMask  <= (others => '1');
        Expected := (others => '0');
        wait for 20 ns;
        AffirmIfEqual(RegOut, Expected, "RegOut, ");

        -- walking 1's test
        for i in 0 to wordsize - 1 loop

            -- only bit i of mask is high
            RegMask    <= (others => '0');
            RegMask(i) <= '1';

            -- RegIn is initially some random value
            RegIn <= RV.RandSlv(0, 2**RegIn'length - 1, RegIn'length);

            -- test all four transitions on bit i of RegOut
            -- 0 to 0
            -- 0 to 1
            -- 1 to 1
            -- 1 to 0

            for j in Transitions'range loop
                RegIn(i)    <= Transitions(j);
                Expected(i) := Transitions(j);
                wait for 20 ns;
                AffirmIfEqual(RegOut, Expected, "RegOut, ");
            end loop;

        end loop;

        -- walking 0's test
        for i in 0 to wordsize - 1 loop

            -- only bit i of mask is low
            RegMask    <= (others => '1');
            RegMask(i) <= '0';

            -- test all four transitions of every bit except bit i of RegOut
            -- 0 to 0
            -- 0 to 1
            -- 1 to 1
            -- 1 to 0

            for j in Transitions'range loop
                RegIn <= (others => Transitions(j));
                Expected(wordsize-1 downto i+1) := (others => Transitions(j));
                Expected(i-1 downto 0)          := (others => Transitions(j));
                wait for 20 ns;
                AffirmIfEqual(RegOut, Expected, "RegOut, ");
            end loop;

        end loop;

        ReportAlerts;

        std.env.stop;

    end process;

end architecture testbench;
