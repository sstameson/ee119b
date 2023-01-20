library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity CORDICCalc_TB is
end entity CORDICCalc_TB;

architecture testbench of CORDICCalc_TB is

    -- convert real to Q1.14 fixed point
    function real2fixed(r: real) return std_logic_vector is
        variable result: std_logic_vector(15 downto 0);
    begin
        result := std_logic_vector(to_signed(integer(round(r * real(2**14))),
                                             result'length));
        return result;
    end;

    -- convert Q1.14 fixed point to real
    function fixed2real(x: std_logic_vector(15 downto 0)) return real is
        variable result: real;
    begin
        result := real(to_integer(signed(x))) / real(2**14);
        return result;
    end;

    -- convert std_logic_vector to string for reporting
    function vec2str(vec: std_logic_vector) return string is
        variable result: string(vec'range);
    begin
        for i in vec'range loop
            case vec(i) is
                when '1'    =>  result(i) := '1';
                when '0'    =>  result(i) := '0';
                when others =>  result(i) := 'X';
            end case;
        end loop;
        return result;
    end;

    constant F_COS: std_logic_vector(4 downto 0)  := "00001";
    constant F_SIN: std_logic_vector(4 downto 0)  := "00101";
    constant F_MUL: std_logic_vector(4 downto 0)  := "00100";
    constant F_COSH: std_logic_vector(4 downto 0) := "00010";
    constant F_SINH: std_logic_vector(4 downto 0) := "00110";
    constant F_DIV: std_logic_vector(4 downto 0)  := "01100";

    -- calculate the specified function to compare with CORDIC
    function calculate(
        x, y : std_logic_vector(15 downto 0);
        f    : std_logic_vector(4 downto 0)
    ) return real is
        variable result: real;
    begin
        case f is
            when F_COS  => result := cos(fixed2real(x));
            when F_SIN  => result := sin(fixed2real(x));
            when F_MUL  => result := fixed2real(x) * fixed2real(y);
            when F_COSH => result := cosh(fixed2real(x));
            when F_SINH => result := sinh(fixed2real(x));
            when others => result := fixed2real(y) / fixed2real(x);
        end case;
        return result;
    end;

    constant EPSILON     : signed := signed(real2fixed(0.0001));
    constant EPSILON_HYP : signed := signed(real2fixed(0.3));

    function is_correct(
        sol : real;
        r   : std_logic_vector(15 downto 0);
        f   : std_logic_vector(4 downto 0)
    ) return boolean is
        variable diff: signed(15 downto 0);
        variable result: boolean;
    begin
        if not ((-2.0 < sol) and (sol < 2.0)) then
            -- solution is not representable in Q1.14 fixed point
            -- so don't report an error
            result := true;
        else
            diff := abs(signed(real2fixed(sol)) - signed(r));
            if f = F_COSH or f = F_SINH then
                result := diff < EPSILON_HYP;
            else
                result := diff < EPSILON;
            end if;
        end if;
        return result;
    end;

    type fixed_vector is array(natural range <>)
                      of std_logic_vector(15 downto 0);
    type control_vector is array(natural range<>) 
                        of std_logic_vector(4 downto 0);

    constant NUM_TESTS: integer := 30;

    constant test_xs: fixed_vector(0 to NUM_TESTS-1) := (
        real2fixed(0.0),
        real2fixed(MATH_PI / 6.0),
        real2fixed(MATH_PI / 4.0),
        real2fixed(MATH_PI / 3.0),
        real2fixed(MATH_PI / 2.0),

        real2fixed(0.0),
        real2fixed(MATH_PI / 6.0),
        real2fixed(MATH_PI / 4.0),
        real2fixed(MATH_PI / 3.0),
        real2fixed(MATH_PI / 2.0),

        real2fixed(0.00),
        real2fixed(0.25),
        real2fixed(0.50),
        real2fixed(0.75),
        real2fixed(1.00),

        real2fixed(0.00),
        real2fixed(0.25),
        real2fixed(0.50),
        real2fixed(0.75),
        real2fixed(1.00),

        real2fixed(0.00),
        real2fixed(0.25),
        real2fixed(0.50),
        real2fixed(0.75),
        real2fixed(1.00),

        real2fixed(1.00),
        real2fixed(1.00),
        real2fixed(0.75),
        real2fixed(0.50),
        real2fixed(1.75)
    );

    constant test_ys: fixed_vector(0 to NUM_TESTS-1) := (
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),

        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),

        real2fixed(1.00),
        real2fixed(0.75),
        real2fixed(0.50),
        real2fixed(0.25),
        real2fixed(0.00),

        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),

        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),
        (others => 'X'),

        real2fixed(0.00),
        real2fixed(0.25),
        real2fixed(0.50),
        real2fixed(0.75),
        real2fixed(1.00)
    );

    constant test_fs: control_vector(0 to NUM_TESTS-1) := (
        F_COS,
        F_COS,
        F_COS,
        F_COS,
        F_COS,

        F_SIN,
        F_SIN,
        F_SIN,
        F_SIN,
        F_SIN,

        F_MUL,
        F_MUL,
        F_MUL,
        F_MUL,
        F_MUL,

        F_COSH,
        F_COSH,
        F_COSH,
        F_COSH,
        F_COSH,

        F_SINH,
        F_SINH,
        F_SINH,
        F_SINH,
        F_SINH,

        F_DIV,
        F_DIV,
        F_DIV,
        F_DIV,
        F_DIV
    );

    constant NUM_CYCLES: natural := 2 + 3;

    signal clk: std_logic;
    signal x, y: std_logic_vector(15 downto 0);
    signal f: std_logic_vector(4 downto 0);
    signal r: std_logic_vector(15 downto 0);

    signal x_reg, y_reg: fixed_vector(0 to NUM_CYCLES - 1);
    signal f_reg: control_vector(0 to NUM_CYCLES - 1);

    signal x_r, y_r: std_logic_vector(15 downto 0);
    signal f_r: std_logic_vector(4 downto 0);

    signal test: std_logic_vector(15 downto 0);

    signal idx: integer := 0;
begin

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
        variable seed1, seed2: positive;
        variable rand1, rand2: real;
    begin
        if rising_edge(clk) then
            if idx < NUM_TESTS then
                x <= test_xs(idx);
                y <= test_ys(idx);
                f <= test_fs(idx);
            else
                if idx = NUM_TESTS then
                    seed1 := 1;
                    seed2 := 1;
                end if;

                uniform(seed1, seed2, rand1);
                uniform(seed1, seed2, rand2);

                x <= real2fixed(MATH_PI / 2.0 * rand1);
                y <= real2fixed(MATH_PI / 2.0 * rand2);

                case f is
                    when F_COS  => f <= F_SIN;
                    when F_SIN  => f <= F_MUL;
                    when F_MUL  => f <= F_COSH;
                    when F_COSH => f <= F_SINH;
                    when F_SINH => f <= F_DIV;
                    when others => f <= F_COS;
                end case;
            end if;

            x_reg <= x_reg(1 to x_reg'high) & x;
            y_reg <= y_reg(1 to y_reg'high) & y;
            f_reg <= f_reg(1 to f_reg'high) & f;

            idx <= idx + 1;

        end if;
    end process;

    process(clk)
        variable sol: real;
    begin
        if falling_edge(clk) then
            if idx > NUM_CYCLES then
                -- calculate correct value using
                -- the registered values x_r, y_r, f_r
                sol := calculate(x_r, y_r, f_r);
                assert is_correct(sol, r, f_r)
                    report lf &
                           "x = " & vec2str(x_r) & lf &
                           "y = " & vec2str(y_r) & lf &
                           "f = " & vec2str(f_r) & lf &
                           "incorrect value: r = " & vec2str(r) & lf &
                           "expected:        r = " & vec2str(real2fixed(sol));
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
