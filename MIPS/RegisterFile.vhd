-- Register File

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity RegisterFile is port
	(
		reset, clk, we : in std_logic;
		readReg1, readReg2, writeReg: in std_logic_vector(4 downto 0);
		writeData: in std_logic_vector(31 downto 0);
		readData1, readData2: out std_logic_vector(31 downto 0)
	);
end RegisterFile;


architecture Behavioral of RegisterFile is
	type regArray is array(0 to 15) of std_logic_vector(31 downto 0);
	signal regArr: regArray;
begin
	process(clk, reset)
	begin
		if reset = '1' then
			regArr <= (others => (others => '0'));
		elsif (clk'event and clk = '1') then
			if(we = '1') then
				regArr(to_integer(unsigned(writeReg))) <= writeData;
			end if;
		end if;
	end process;
	readData1 <= regArr(to_integer(unsigned(readReg1)));
	readData2 <= regArr(to_integer(unsigned(readReg2)));
end Behavioral;
