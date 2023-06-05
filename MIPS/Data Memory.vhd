
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity DataMemory is port
(
	addr: in std_logic_vector(31 downto 0);
	writeData: in std_logic_vector(31 downto 0);
	clock, memWrite_en, memRead: in std_logic;
	readData: out std_logic_vector(31 downto 0)
);
end DataMemory;


architecture datamem of DataMemory is
	type memArray is array(0 to 15) of std_logic_vector(31 downto 0);
	signal memArr: memArray;

begin
	process(clock)
	begin
		if(clock'event and clock = '1') then
			if(memWrite_en = '1') then	
				memArr(to_integer(unsigned(addr))) <= writeData;
			end if;
		end if;
	end process;
	readData <= memArr(to_integer(unsigned(addr))) when memRead = '1' else x"00000000";

end datamem;