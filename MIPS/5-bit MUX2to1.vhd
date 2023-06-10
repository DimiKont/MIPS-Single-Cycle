-- 5-bit MUX 2 to 1

library ieee;
use ieee.std_logic_1164.all;

entity mux21 is port
	(
		inputA, inputB : in std_logic_vector(4 downto 0);
		regDst: in std_logic;
		result: out std_logic_vector(4 downto 0)
	);
end mux21;

architecture mux5b of mux21 is
begin
	process(regDst, inputA, inputB)
	begin
		if regDst = '0' then
			result <= inputA;
		elsif regDst = '1' then
			result <= inputB;
		end if;
	end process;
end mux5b;
