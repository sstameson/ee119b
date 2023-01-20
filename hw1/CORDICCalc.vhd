---------------------------------------------------------------------------
--
-- CORDIC Calculator
--
-- This file contains the implementation of a CORDIC calculator for
-- computing sin, cos, sinh, cosh, multiplication, and division.
-- The inputs and output of the calculator are stored in DFFs and the
-- computation is fully parallel, so it takes 1 cycle of latency.
-- All inputs and outputs are interpreted as Q1.14 fixed point numbers.
-- The implemented algorithm works for only for non-negative inputs.
--
---------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

--
-- FullAdder entity declaration
--
-- This entity implements a 1-bit full adder
-- a - the first 1-bit input
-- b - the second 1-bit input
-- Cin - the carry input
-- s - the sum output
-- Cout - the carry output
--

entity FullAdder is
    port (
        a   : in  std_logic;
        b   : in  std_logic;
        Cin : in  std_logic;

        s    : out std_logic;
        Cout : out std_logic
    );
end entity FullAdder;

architecture synth of FullAdder is
begin
    s    <= a xor b xor Cin;
    Cout <= (a and b) or (Cin and (a xor b));
end architecture synth;

--
-- AddSub entity declaration
--
-- This entity implements a 22-bit adder/subtractor for the intermediate
-- CORDIC calculations
-- a - the first input
-- b - the second input
-- f - the function to be computed
--     f = 00 outputs the input a unchanged
--     f = 01 preforms the addition a + b
--     f = 10 performs the subtraction a - b
-- r - the result of the computation
--

library ieee;
use ieee.std_logic_1164.all;

entity AddSub is
    port (
        a : in  std_logic_vector(21 downto 0);
        b : in  std_logic_vector(21 downto 0);

        -- f = 00 -- r <= a
        -- f = 01 -- r <= a + b
        -- f = 10 -- r <= a - b
        f : in  std_logic_vector(1 downto 0);

        r : out std_logic_vector(21 downto 0)
    );
end entity AddSub;

architecture synth of AddSub is
    signal x: std_logic_vector(a'range);
    signal y: std_logic_vector(b'range);
    signal s: std_logic_vector(r'range);
    signal carry: std_logic_vector(a'high + 1 downto 0);
begin
    y <= a;
    x <= b when carry(0) = '0' else
         not b;

    carry(0) <= f(1);
    ripple: for i in a'range generate
        FAx: entity work.FullAdder
            port map (
                a    => x(i),
                b    => y(i),
                Cin  => carry(i),
                s    => s(i),
                Cout => carry(i + 1)
            );
    end generate ripple;

    r <= s when (f(1) xor f(0)) = '1' else
         a;
end architecture synth;

--
-- CORDICSlice entity declaration
--
-- This entity implements one layer of the CORDIC calculation, which consists
-- of three 21-bit add/sub/no-op operations selected by the control signals
-- to compute the next x, y, z values for the next slice.
-- d - the decision variable
--     d can be -1 or 1
--     "01" represents 1
--     "10" represents -1
--     d = 1 when z is positive in rotation mode or
--                y is negative in vectoring mode
--           otherwise d is -1
-- m - the type of computation
--     m can be 1, 0, or -1
--     "01" represents 1 (circular)
--     "00" represents 0 (linear)
--     "10" represents -1 (hyperbolic)
-- x_prev - the previous x
-- x_shft - the previous x shifted by the layer depth
-- y_prev - the previous y
-- y_shft - the previous y shifted by the layer depth
-- z_prev - the previous z
-- z_shft - the previous z shifted by the layer depth
-- const  - the constant used to compute z_next
-- x_next - the x value for the next slice
-- y_next - the y value for the next slice
-- z_next - the z value for the next slice
--

library ieee;
use ieee.std_logic_1164.all;

entity CORDICSlice is
    port (
        d      : in  std_logic_vector(1 downto 0);
        m      : in  std_logic_vector(1 downto 0);

        x_prev : in  std_logic_vector(21 downto 0);
        x_shft : in  std_logic_vector(21 downto 0);
        y_prev : in  std_logic_vector(21 downto 0);
        y_shft : in  std_logic_vector(21 downto 0);
        z_prev : in  std_logic_vector(21 downto 0);
        const  : in  std_logic_vector(21 downto 0);

        x_next : out std_logic_vector(21 downto 0);
        y_next : out std_logic_vector(21 downto 0);
        z_next : out std_logic_vector(21 downto 0)
    );
end entity CORDICSlice;

architecture synth of CORDICSlice is
    signal f0, f1, f2: std_logic_vector(1 downto 0);
begin

    --
    -- d can be -1 or 1
    -- m can be -1, 0, or 1
    -- "01" represents 1
    -- "00" represents 0
    -- "10" represents -1
    -- we compute the following
    -- f0 = -md
    -- f1 =  d
    -- f2 = -d
    --

    f0 <= "01" when m = "10" and d = "01" else
          "10" when m = "01" and d = "01" else
          m;

    f1 <= d;

    f2 <= not d;

    AS0: entity work.AddSub
        port map (
            a => x_prev,
            b => y_shft,
            f => f0,
            r => x_next
        );

    AS1: entity work.AddSub
        port map (
            a => y_prev,
            b => x_shft,
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

end architecture synth;


library ieee;
use ieee.std_logic_1164.all;

--
-- CORDICCalc entity declaration
--
-- This implements a CORDIC calculator for computing sin, cos, sinh, cosh,
-- multiplication, and division. Inputs/outputs are stored in DFFs. The
-- computation is fully parallel and completes in one cycle. All inputs and
-- outputs are interpreted as Q1.14 fixed point numbers
--
-- clk - the clock for storage of inputs x, y, f, and output r
-- x - the first input (see meaning below), must be a non-negative
--     Q1.14 fixed point number
-- y - the second input (see meaning below), must be a non-negative
--     Q1.14 fixed point number
-- f - the function to perform
--     f = 00001 - computes cos(x)
--     f = 00101 - computes sin(x)
--     f = 00100 - computes x * y
--     f = 00010 - computes cosh(x)
--     f = 00110 - computes sinh(x)
--     f = 01100 - computes y / x
-- r - the result of the computation as a Q1.14 fixed point number
--

entity CORDICCalc is
    port (
        clk : in  std_logic;

        x   : in  std_logic_vector(15 downto 0);
        y   : in  std_logic_vector(15 downto 0);

        f   : in  std_logic_vector(4 downto 0);

        r   : out std_logic_vector(15 downto 0)
    );
end entity CORDICCalc;

architecture synth of CORDICCalc is
    type calc_vector    is array(natural range <>)
                        of std_logic_vector(21 downto 0);
    type const_vector   is array(natural range <>, natural range <>)
                        of std_logic_vector(21 downto 0);
    type control_vector is array(natural range <>)
                        of std_logic_vector(1 downto 0);

    -- intermediate x, y, and z values
    signal xs, ys, zs: calc_vector(0 to 20);

    signal x1, y1, z1: std_logic_vector(21 downto 0);

    signal x_shfts, y_shfts: calc_vector(0 to 19);

    -- decision variables
    signal ds: control_vector(0 to 19);

    -- constants (see below)
    signal cs: calc_vector(0 to 19);

    signal K: std_logic_vector(21 downto 0);

    -- registered inputs
    signal x_reg: std_logic_vector(x'range);
    signal y_reg: std_logic_vector(y'range);
    signal f_reg: std_logic_vector(f'range);

    -- sign-extended inputs to Q3.18 fixed point form
    signal x_ext, y_ext: std_logic_vector(21 downto 0);

    -- control signals
    signal m: std_logic_vector(1 downto 0);
    signal result: std_logic;
    signal vectoring: std_logic;
    signal composite: std_logic;

    -- pre-computed constants for the computation
    -- all constants are in Q3.18 fixed point form

    -- Ks(0) = 1
    -- Ks(1) = 1/sqrt(1 + (1/2^i)^2)
    -- Ks(2) = 1/sqrt(1 - (1/2^i)^2)
    constant Ks: calc_vector(0 to 2) :=
        ("0001000000000000000000", "0000100110110111010100", "0001001101001000001111");

    -- consts(i, 0) = 1/2^i
    -- consts(i, 1) = arctan(1/2^i)
    -- consts(i, 2) = arctanh(1/2^i)
    constant consts: const_vector(0 to 19, 0 to 2) := (
        ("0001000000000000000000", "0000110010010000111111", "0000000000000000000000"),
        ("0000100000000000000000", "0000011101101011000110", "0000100011001001111101"),
        ("0000010000000000000000", "0000001111101011011100", "0000010000010110001011"),
        ("0000001000000000000000", "0000000111111101010111", "0000001000000010101100"),
        ("0000000100000000000000", "0000000011111111101011", "0000000100000000010101"),
        ("0000000010000000000000", "0000000001111111111101", "0000000010000000000011"),
        ("0000000001000000000000", "0000000001000000000000", "0000000001000000000000"),
        ("0000000000100000000000", "0000000000100000000000", "0000000000100000000000"),
        ("0000000000010000000000", "0000000000010000000000", "0000000000010000000000"),
        ("0000000000001000000000", "0000000000001000000000", "0000000000001000000000"),
        ("0000000000000100000000", "0000000000000100000000", "0000000000000100000000"),
        ("0000000000000010000000", "0000000000000010000000", "0000000000000010000000"),
        ("0000000000000001000000", "0000000000000001000000", "0000000000000001000000"),
        ("0000000000000000100000", "0000000000000000100000", "0000000000000000100000"),
        ("0000000000000000010000", "0000000000000000010000", "0000000000000000010000"),
        ("0000000000000000001000", "0000000000000000001000", "0000000000000000001000"),
        ("0000000000000000000100", "0000000000000000000100", "0000000000000000000100"),
        ("0000000000000000000010", "0000000000000000000010", "0000000000000000000010"),
        ("0000000000000000000001", "0000000000000000000001", "0000000000000000000001"),
        ("0000000000000000000000", "0000000000000000000000", "0000000000000000000001")
    );
begin

    -- store inputs in a DFF
    process (clk)
    begin
        if rising_edge(clk) then
            x_reg <= x;
            y_reg <= y;
            f_reg <= f;
        end if;
    end process;

    -- decode f
    m         <= f_reg(1 downto 0);
    result    <= f_reg(2);
    vectoring <= f_reg(3);
    composite <= f_reg(4);

    -- ds(i) = 1 when the decision variable is non-negative
    -- otherwise ds(i) = -1
    decisions: for i in ds'range generate
        ds(i) <= "01" when (zs(i)(zs(i)'high) = '0' and vectoring = '0') or
                           (ys(i)(ys(i)'high) = '1' and vectoring = '1') else
                 "10";
    end generate decisions;

    -- cs(i) = consts(i, m)
    -- pick constants based on mode (circular, linear, or hyperbolic)
    -- the cs array must exist so that expressions in the port map
    -- of the CORDIC slice are static
    constants: for i in cs'range generate
        cs(i) <= consts(i, 0) when m = "00" else
                 consts(i, 1) when m = "01" else
                 consts(i, 2);
    end generate constants;

    -- compute the following
    -- x_shfts(i) <= xs(i) >>> i
    -- y_shfts(i) <= ys(i) >>> i
    -- where >>> is right arithmetic shift
    x_shfts(0) <= xs(0);
    y_shfts(0) <= ys(0);
    shifts: for i in 1 to x_shfts'high generate
        x_shfts(i) <=
           (xs(i)'high downto xs(i)'high - i + 1 => xs(i)(xs(i)'high)) &
            xs(i)(xs(i)'high downto i);
        y_shfts(i) <=
           (ys(i)'high downto ys(i)'high - i + 1 => ys(i)(ys(i)'high)) &
            ys(i)(ys(i)'high downto i);
    end generate shifts;

    -- K = Ks(m)
    -- pick K based on mode (circular, linear, or hyperbolic)
    K <= Ks(0) when m = "00" else
         Ks(1) when m = "01" else
         Ks(2);

    -- sign extend the input values
    x_ext <= x_reg(x_reg'high) & x_reg(x_reg'high) & x_reg & "0000";
    y_ext <= y_reg(y_reg'high) & y_reg(y_reg'high) & y_reg & "0000";

    xs(0) <= K when vectoring = '0' and (m = "01" or m = "10") else
             x_ext;
    ys(0) <= (others => '0') when vectoring = '0' else
             y_ext;
    zs(0) <= x_ext  when vectoring = '0' and (m = "01" or m = "10") else
             y_ext  when vectoring = '0' else
             (others => '0');

    slices: for i in 1 to xs'high generate

        -- skip the first CORDIC slice in hyperbolic mode
        -- must skip the first hyperbolic slice because the
        -- constant arctanh(1) does not exist
        i1: if i = 1 generate
            slice1: entity work.CORDICSlice
                port map (
                    d      => ds(i - 1),
                    m      => m,
                    x_prev => xs(i - 1),
                    x_shft => x_shfts(i - 1),
                    y_prev => ys(i - 1),
                    y_shft => y_shfts(i - 1),
                    z_prev => zs(i - 1),
                    const  => cs(i - 1),
                    x_next => x1,
                    y_next => y1,
                    z_next => z1
                );

            xs(i) <= xs(i - 1) when m = "10" else
                     x1;
            ys(i) <= ys(i - 1) when m = "10" else
                     y1;
            zs(i) <= zs(i - 1) when m = "10" else
                     z1;
        end generate i1;

        -- generate the remaining CORDIC slices
        iX: if i > 1 generate
            sliceX: entity work.CORDICSlice
                port map (
                    d      => ds(i - 1),
                    m      => m,
                    x_prev => xs(i - 1),
                    x_shft => x_shfts(i - 1),
                    y_prev => ys(i - 1),
                    y_shft => y_shfts(i - 1),
                    z_prev => zs(i - 1),
                    const  => cs(i - 1),
                    x_next => xs(i),
                    y_next => ys(i),
                    z_next => zs(i)
                );
        end generate iX;

    end generate slices;

    -- register the output
    process (clk)
    begin
        if rising_edge(clk) then
            if vectoring = '0' and result = '0' then
                r <= xs(xs'high)(19 downto 4);
            elsif vectoring = '0' and result = '1' then
                r <= ys(ys'high)(19 downto 4);
            else
                r <= zs(zs'high)(19 downto 4);
            end if;
        end if;
    end process;

end architecture synth;
