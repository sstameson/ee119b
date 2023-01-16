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
    -- d can be -1 or 1
    -- m can be -1, 0, or 1
    -- "01" represents 1
    -- "00" represents 0
    -- "10" represents -1
    -- we compute the following
    -- f0 = -md
    -- f1 =  d
    -- f2 = -d

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
            a => x_shft,
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

end architecture synth;


library ieee;
use ieee.std_logic_1164.all;

entity CORDICCalc is
    port (
        clk : in  std_logic;

        x   : in  std_logic_vector(15 downto 0);
        y   : in  std_logic_vector(15 downto 0);

        f   : in  std_logic_vector(15 downto 0);

        r   : out std_logic_vector(15 downto 0)
    );
end entity CORDICCalc;

architecture synth of CORDICCalc is
    type calc_vector    is array(natural range <>)
                        of std_logic_vector(21 downto 0);
    type control_vector is array(natural range <>)
                        of std_logic_vector(1 downto 0);

    signal xs, ys, zs: calc_vector(0 to 16);

    signal x_shfts, y_shfts: calc_vector(0 to 15);

    -- TODO: ROM of constants
    signal consts: calc_vector(0 to 15);

    signal ds: control_vector(0 to 15);

    signal x_ext, y_ext: std_logic_vector(21 downto 0);

    signal m: std_logic_vector(1 downto 0);
    signal result: std_logic;
    signal vectoring: std_logic;
    signal composite: std_logic;
begin

    -- decode f
    m         <= f(1 downto 0);
    result    <= f(2);
    vectoring <= f(3);
    composite <= f(4);

    decisions: for i in ds'range generate
        -- ds(i) = 1 when the decision variable is non-negative
        -- otherwise ds(i) = -1
        ds(i) <= "01" when (zs(i)(zs(i)'high) = '0' and vectoring = '0') or
                           (ys(i)(ys(i)'high) = '0' and vectoring = '1') else
                 "10";
    end generate decisions;

    -- x_shfts(i) <= xs(i) >>> i
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

    -- TODO: Latch in DFF and make this more general
    x_ext <= x(x'high) & x(x'high) & x & "0000";
    y_ext <= y(y'high) & y(y'high) & y & "0000";

    xs(0) <= K when vectoring = '0' and (m = "01" or m = "10") else
             x_ext;
    ys(0) <= (others => '0') when vectoring = '0' else
             y_ext;
    zs(0) <= x_ext  when vectoring = '0' and (m = "01" or m = "10") else
             y_ext  when vectoring = '0' else
             (others => '0');
    slices: for i in 1 to xs'high generate
        -- TODO select constants with m
        sliceX: entity work.CORDICSlice
            port map (
                d      => ds(i - 1),
                m      => m,
                x_prev => xs(i - 1),
                x_shft => x_shfts(i - 1),
                y_prev => ys(i - 1),
                y_shft => y_shfts(i - 1),
                z_prev => zs(i - 1),
                const  => consts(i - 1),
                x_next => xs(i),
                y_next => ys(i),
                z_next => zs(i)
            );
    end generate slices;

end architecture synth;
