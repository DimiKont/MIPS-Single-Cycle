
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MIPS_tb is
end MIPS_tb;

architecture testbench of MIPS_tb is
	signal clock, reset : std_logic;
	constant clk_period : time := 100 ps;

	component MIPS is
		port (clock, reset: in std_logic);
        end component MIPS;
begin
	init: MIPS
	port map
	(
		clock => clock,
		reset => reset
	);

	clock_process: process
	begin
		clock <= '0';
		while now < 5000 ps loop
			wait for clk_period;
			clock <= not clock;
			wait for clk_period;
			clock <= not clock;
		end loop;
		wait;
	end process;

	stimulus_process : process
	begin
		reset <= '1';
		wait for 80 ps;
		reset <= '0';
		wait;
	end process;
end testbench;