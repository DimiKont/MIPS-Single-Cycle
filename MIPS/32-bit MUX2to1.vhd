-- 32-bit MUX 2 to 1

library ieee;
use ieee.std_logic_1164.all;

entity mux21_32 is port
	(
		A, B : in std_logic_vector(31 downto 0);
		S : in std_logic;
		res: out std_logic_vector(31 downto 0)
	);
end mux21_32;

architecture mux32 of mux21_32 is
begin
	process(A, B, S)
	begin
		if S = '0' then
			res <= A;
		elsif S = '1' then
			res <= B;
		end if;
	end process;
end mux32;

