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

entity CORDICSlice is
    port (
        x_in  : in std_logic_vector(21 downto 0);
        y_in  : in std_logic_vector(21 downto 0);
        z_in  : in std_logic_vector(21 downto 0);
        const : in std_logic_vector(21 downto 0);

        x_out : out std_logic_vector(21 downto 0);
        y_out : out std_logic_vector(21 downto 0);
        z_out : out std_logic_vector(21 downto 0)
    );
end entity CORDICSlice;


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
