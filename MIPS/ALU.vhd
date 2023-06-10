library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

-- ALU Entity

entity ALU is port
(
	inputA, inputB: in std_logic_vector(31 downto 0);
	ALU_Ctl: in std_logic_vector(3 downto 0);
	ALU_Result: out std_logic_vector(31 downto 0);
	zero: out std_logic
);
end ALU;


-- ALU Architecture

architecture alu of ALU is
	signal tmp: std_logic_vector(31 downto 0);
begin
	process(inputA, inputB, ALU_Ctl)
	begin
		case ALU_Ctl is
			when "0000" => tmp <= inputA AND inputB;
			when "0001" => tmp <= inputA OR inputB;
			when "0010" => tmp <= inputA + inputB;
			when "0110" => tmp <= inputA - inputB;
			when others => tmp <= inputA + inputB;
		end case;
	end process;
	ALU_Result <= tmp;
	zero <= '0' when tmp = X"00000000" else '1';
end alu;

