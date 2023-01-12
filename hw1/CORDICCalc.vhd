library ieee;
use ieee.std_logic_1164.all;

entity CORDICCalc is
    port (
        clk : in std_logic;

        x : in std_logic_vector(15 downto 0);
        y : in std_logic_vector(15 downto 0);

        f : in  std_logic_vector(15 downto 0);

        r : out std_logic_vector(15 downto 0)
    );
end entity CORDICCalc;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddSub is
    port (
        a : in std_logic_vector(21 downto 0);
        b : in std_logic_vector(21 downto 0);

        f : in std_logic_vector(1 downto 0);

        r : out std_logic_vector(21 downto 0)
    );
end entity AddSub;

architecture synth of AddSub is
begin
    r <= std_logic_vector(signed(a) + signed(b)) when f = "01" else
         std_logic_vector(signed(a) - signed(b)) when f = "11" else
         a;
end architecture synth;


library ieee;
use ieee.std_logic_1164.all;

entity CORDICSlice is
    port (
        x_prev : in std_logic_vector(21 downto 0);
        y_prev : in std_logic_vector(21 downto 0);
        z_prev : in std_logic_vector(21 downto 0);
        const  : in std_logic_vector(21 downto 0);
        m      : in std_logic;

        x_next : out std_logic_vector(21 downto 0);
        y_next : out std_logic_vector(21 downto 0);
        z_next : out std_logic_vector(21 downto 0)
    );
end entity CORDICSlice;

architecture synth of CORDICSlice is
    signal f0, f1, f2: std_logic_vector(1 downto 0)
    signal d: std_logic_vector(1 downto 0);
begin
    -- TODO: control signals

    -- d is 1 when z_prev is positive and -1 when z_prev is negative

    -- f0 <= -md
    -- f1 <=  d
    -- f2 <= -d

    AS0: entity work.AddSub
        port map (
            a => x_prev,
            b => y_prev,
            f => f0,
            r => x_next
        );

    AS1: entity work.AddSub
        port map (
            a => x_prev,
            b => y_prev,
            f => f1,
            r => y_next
        );

    AS2: entity work.AddSub
        port map (
            a => z_prev,
            b => const,
            f => f2,
            r => z_next
        );
end architecture;
