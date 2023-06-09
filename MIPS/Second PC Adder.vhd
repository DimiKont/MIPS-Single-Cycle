
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity PCaddr2 is port
	(
		PCafter, signex: in std_logic_vector(31 downto 0);
		PCtomux: out std_logic_vector (31 downto 0)
	);
end PCaddr2;

architecture PCafter of PCaddr2 is
begin
	PCtomux <= PCafter + signex;
end PCafter;
