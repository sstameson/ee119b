library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddSub is
    port (
        a : in  std_logic_vector(21 downto 0);
        b : in  std_logic_vector(21 downto 0);

        f : in  std_logic_vector(1 downto 0);

        r : out std_logic_vector(21 downto 0)
    );
end entity AddSub;

architecture synth of AddSub is
begin
    -- TODO: replace this with a custom adder
    r <= std_logic_vector(signed(a) + signed(b)) when f = "01" else
         std_logic_vector(signed(a) - signed(b)) when f = "10" else
         a;
end architecture synth;


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
use ieee.numeric_std.all;

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

    signal xs, ys, zs: calc_vector(0 to 16);

    signal x1, y1, z1: std_logic_vector(21 downto 0);

    signal x_shfts, y_shfts: calc_vector(0 to 15);

    signal ds: control_vector(0 to 15);

    signal cs: calc_vector(0 to 15);

    signal K: std_logic_vector(21 downto 0);

    signal x_reg: std_logic_vector(x'range);
    signal y_reg: std_logic_vector(y'range);
    signal f_reg: std_logic_vector(f'range);

    signal x_ext, y_ext: std_logic_vector(21 downto 0);

    signal m: std_logic_vector(1 downto 0);
    signal result: std_logic;
    signal vectoring: std_logic;
    signal composite: std_logic;

    constant Ks: calc_vector(0 to 2) :=
        ("0001000000000000000000", "0000100110110111010100", "0001001101001000001111");
    constant consts: const_vector(0 to 15, 0 to 2) := (
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
        ("0000000000000000001000", "0000000000000000001000", "0000000000000000001000")
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
        cs(i) <= consts(i, to_integer(unsigned(m)));
    end generate constants;

    -- K = Ks(m)
    -- pick K based on mode (circular, linear, or hyperbolic)
    K <= Ks(to_integer(unsigned(m)));

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


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity CORDICCalc_TB is
end entity CORDICCalc_TB;

architecture testbench of CORDICCalc_TB is

    function real2fixed(r: real) return std_logic_vector is
        variable result: std_logic_vector(15 downto 0);
    begin
        result := std_logic_vector(to_signed(integer(round(r * real(2**14))),
                                             result'length));
        return result;
    end;

    type fixed_vector is array(natural range <>)
                      of std_logic_vector(15 downto 0);
    type control_vector is array(natural range<>) 
                        of std_logic_vector(4 downto 0);

    constant F_COS: std_logic_vector(4 downto 0)  := "00001";
    constant F_SIN: std_logic_vector(4 downto 0)  := "00101";
    constant F_MUL: std_logic_vector(4 downto 0)  := "00100";
    constant F_COSH: std_logic_vector(4 downto 0) := "00010";
    constant F_SINH: std_logic_vector(4 downto 0) := "00110";
    constant F_DIV: std_logic_vector(4 downto 0)  := "01100";

    constant NUM_TESTS: integer := 5;

    constant test_xs: fixed_vector(0 to NUM_TESTS-1) := (
        real2fixed(0.0),
        real2fixed(MATH_PI / 6.0),
        real2fixed(MATH_PI / 4.0),
        real2fixed(MATH_PI / 3.0),
        real2fixed(MATH_PI / 2.0)
    );

    constant test_ys: fixed_vector(0 to NUM_TESTS-1) := (
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X')
    );

    constant test_fs: control_vector(0 to NUM_TESTS-1) := (
        F_COS,
        F_COS,
        F_COS,
        F_COS,
        F_COS
    );

    signal clk: std_logic;
    signal x, y: std_logic_vector(15 downto 0);
    signal f: std_logic_vector(4 downto 0);
    signal r: std_logic_vector(15 downto 0);

    signal x_reg, y_reg: fixed_vector(0 to 1);
    signal f_reg: control_vector(0 to 1);

    signal x_r, y_r: std_logic_vector(15 downto 0);
    signal f_r: std_logic_vector(4 downto 0);

    signal idx: integer := 0;
begin
    -- f <= "00001";
    -- x <= "0001111101000111";
    -- y <= "0001111101000111";
    -- f <= "01101";
    -- x <= "0010000000000000";
    -- y <= "0011011101101101";

    UUT: entity work.CORDICCalc
        port map (
                clk => clk,
                x => x,
                y => y,
                f => f,
                r => r
            );

    x_r <= x_reg(0);
    y_r <= y_reg(0);
    f_r <= f_reg(0);

    process(clk)
    begin
        if rising_edge(clk) then
            if idx < NUM_TESTS then
                x <= test_xs(idx);
                y <= test_ys(idx);
                f <= test_fs(idx);
            else
                -- TODO: assign random x, y, f
            end if;

            x_reg <= x_reg(1) & x;
            y_reg <= y_reg(1) & y;
            f_reg <= f_reg(1) & f;

            idx <= idx + 1;

        end if;
    end process;


    process(clk)
    begin
        if falling_edge(clk) then
            if idx > 2 then
                -- TODO: insert assertions
                -- compute correct value use x_r, y_r, f_r
            end if;
        end if;
    end process;

    process
    begin
        clk <= '0';
        wait for 10 ns;

        clk <= '1';
        wait for 10 ns;
    end process;

end architecture testbench;
