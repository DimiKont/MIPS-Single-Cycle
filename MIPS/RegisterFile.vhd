

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
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
			regArr(0) <= X"00000000";
			regArr(1) <= X"00000000";
			regArr(2) <= X"00000000";
			regArr(3) <= X"00000000";
			regArr(4) <= X"00000000";
			regArr(5) <= X"00000000";
			regArr(6) <= X"00000000";
			regArr(7) <= X"00000000";
			regArr(8) <= X"00000000";
			regArr(9) <= X"00000000";
			regArr(10) <= X"00000000";
			regArr(11) <= X"00000000";
			regArr(12) <= X"00000000";
			regArr(13) <= X"00000000";
			regArr(14) <= X"00000000";
			regArr(15) <= X"00000000";
		end if;

		if (clk'event and clk = '1') then
			if(we = '1') then
				regArr(to_integer(unsigned(writeReg))) <= writeData;
			end if;
		end if;
	end process;
	readData1 <= regArr(to_integer(unsigned(readReg1)));
	readData2 <= regArr(to_integer(unsigned(readReg2)));

end Behavioral;