
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MIPS_tb is
end MIPS_tb;

architecture testbench of MIPS_tb is
	signal clock, reset : std_logic := '0';
	signal PC_out, ALU_Result, inputA, inputB : std_logic_vector(31 downto 0);
	signal ALU_Ctl : std_logic_vector(2 downto 0);
	signal ALU_Resultb:  std_logic_vector(31 downto 0);

	component MIPS is
		port (
			clock, reset: in std_logic;
		        PC_out, ALU_Result: out std_logic_vector(31 downto 0)
	              );
        end component MIPS;
begin
	init: MIPS
	port map
	(
		clock => clock,
		reset => reset,
		PC_out => PC_out,
		ALU_Result => ALU_Result
	);

	process
	begin
		reset <= '1';
		wait for 50 ps;
		reset <= '0';
	end process;

	process
	begin
		clock <= '0';
		wait for 20 ps;
		clock <= '1';
		wait for 20 ps;
	end process;
	
end testbench;