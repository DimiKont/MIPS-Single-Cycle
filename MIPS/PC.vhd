-- Program Counter

library ieee;
use ieee.std_logic_1164.all;

entity PC is port
(
	reset, clock : in std_logic;
	PC_in: in std_logic_vector(31 downto 0);
	PC_out : out std_logic_vector(31 downto 0)

);
end PC;

architecture pc of PC is
begin
	process(reset, clock)	
	begin
		if reset = '1' then
			PC_out <= X"00000000";
		elsif clock'event and clock = '1' then
			PC_out <= PC_in;
		end if;
	end process;
end pc;
