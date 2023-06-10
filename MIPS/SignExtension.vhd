-- Sign Extension

library ieee;
use ieee.std_logic_1164.all;

 entity SignExtension is port
	(
		inBits: in std_logic_vector(15 downto 0);
		outBits: out std_logic_vector(31 downto 0)
	);
end SignExtension;

architecture SignEx of SignExtension is
begin
	process(inBits)
	begin
		if inBits(15) = '0' then
			outBits <= X"0000" & inBits;
		elsif inBits(15) = '1' then
			outBits <= X"FFFF" & inBits;
		end if;
	end process;
end SignEx;
