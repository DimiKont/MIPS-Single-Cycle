
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity PCaddr is port
	(
		PC: in std_logic_vector(31 downto 0);
		PCnext: out std_logic_vector (31 downto 0)
	);
end PCaddr;

architecture PCnext of PCaddr is
begin
	PCnext <= PC + 1;
end PCnext;