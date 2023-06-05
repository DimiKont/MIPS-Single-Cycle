
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;



entity PC is port
(
	clock, reset: in std_logic;
	PC_out : out std_logic_vector(31 downto 0)
);
end PC;


architecture pc of PC is
	signal PC: std_logic_vector(31 downto 0);
begin
	process(clock, reset)
	begin 
		if reset = '1' then
			PC <= X"00000000";
		elsif(clock'event and clock ='1') then
			PC <= PC + 1;
		end if;
	end process;
	PC_out <= PC;
end pc;