
library ieee;
use ieee.std_logic_1164.all;

entity mux21 is port
	(
		ALU_Result, readData : in std_logic_vector(31 downto 0);
		MemtoReg: in std_logic;
		writeData: out std_logic_vector(31 downto 0)
	);
end mux21;

architecture mux32 of mux21 is
begin
	process(MemtoReg, ALU_Result, readData)
	begin
		if MemtoReg = '0' then
			writeData <= ALU_Result;
		elsif MemtoReg = '1' then
			writeData <= readData;
		end if;
	end process;
end mux32;