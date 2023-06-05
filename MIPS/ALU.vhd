
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity ALU is port
	(
		inputA, inputB: in std_logic_vector(31 downto 0);
		ALU_Ctl: in std_logic_vector(2 downto 0);
		ALU_Result: out std_logic_vector(31 downto 0);
		zero: out std_logic
	);
end ALU;


architecture Behavioral of ALU is
	signal tmp: std_logic_vector(31 downto 0);
begin
	process(inputA, inputB, ALU_Ctl)
	begin
		case ALU_Ctl is
			when "000" => tmp <= inputA + inputB;
			when "001" => tmp <= inputA - inputB;
			when "010" => tmp <= inputA AND inputB;
			when "011" => tmp <= inputA OR inputB;
			when "100" => tmp <= inputA NOR inputB;
			when others => tmp <= inputA + inputB;
		end case;

	end process;
	zero <= '1' when tmp = X"00000000" else '0';
	ALU_Result <= tmp;
end behavioral;
	