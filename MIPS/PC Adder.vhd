
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity PCaddr is port
	(
		PC: in std_logic_vector(31 downto 0);
		reset, clock: in std_logic;
		PCnext: out std_logic_vector (31 downto 0)
	);
end PCaddr;

architecture PCnext of PCaddr is
begin
	process(clock, reset)
	begin
		if reset = '1' then
			PCnext <= X"00000000";
		elsif clock'event and clock = '1' then
			PCnext <= PC + X"00000001";
		end if;
	end process;
end PCnext;