-- Program Counter Adder (Implemented in two different ways)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity PCaddr is port
	(
		in1, in2: in std_logic_vector(31 downto 0);
		addrout: out std_logic_vector (31 downto 0)
	);
end PCaddr;

architecture PCAfter of PCaddr is
begin
	addrout <= in1 + in2;
end PCAfter;