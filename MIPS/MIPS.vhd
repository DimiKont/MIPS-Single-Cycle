
library ieee;
use ieee.std_logic_1164.all;


-- MIPS Entity

entity MIPS is port
(
	clock, reset: in std_logic;
	PC_out, ALU_Result: out std_logic_vector(31 downto 0)
);
end MIPS;




library ieee;
use ieee.std_logic_1164.all;


-- ALU Entity

entity ALU is port
(
	inputA, inputB: in std_logic_vector(31 downto 0);
	ALU_Ctl: in std_logic_vector(2 downto 0);
	ALU_Result: out std_logic_vector(31 downto 0);
	zero: out std_logic
);
end ALU;



library ieee;
use ieee.std_logic_1164.all;



-- Register File Entity

entity RegisterFile is port
	(
		reset, clock, we : in std_logic;
		readReg1, readReg2, writeReg: in std_logic_vector(4 downto 0);
		writeData: in std_logic_vector(31 downto 0);
		readData1, readData2: out std_logic_vector(31 downto 0)
	);
end RegisterFile;


library ieee;
use ieee.std_logic_1164.all;



-- Data Memory Entity

entity DataMemory is port
(
	addr: in std_logic_vector(31 downto 0);
	writeData: in std_logic_vector(31 downto 0);
	clock, memWrite_en, memRead: in std_logic;
	readData: out std_logic_vector(31 downto 0)
);
end DataMemory;

library ieee;
use ieee.std_logic_1164.all;

entity PC is port
(
	clock, reset: in std_logic;
	PC_out : out std_logic_vector(31 downto 0)
);
end PC;



library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


-- ALU Architecture

architecture alu of ALU is
	signal tmp: std_logic_vector(31 downto 0);
begin
	process(inputA, inputB, ALU_Ctl)
	begin
		case ALU_Ctl is
			when "000" => tmp <= inputA + inputB;
			when "001" => tmp <= inputA - inputB;
			when "010" => tmp <= inputA AND inputB;
			when "011" => tmp <= inputA OR inputB;
			when "100" => tmp <= inputA NOR inputB;
			when others => tmp <= inputA + inputB;
		end case;

	end process;
	zero <= '1' when tmp = X"00000000" else '0';
	ALU_Result <= tmp;
end alu;




library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


-- Register File Architecture

architecture registerfile of RegisterFile is
	type regArray is array(0 to 15) of std_logic_vector(31 downto 0);
	signal regArr: regArray;
begin
	process(clock, reset)
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

		if (clock'event and clock = '1') then
			if(we = '1') then
				regArr(to_integer(unsigned(writeReg))) <= writeData;
			end if;
		end if;
	end process;
	readData1 <= regArr(to_integer(unsigned(readReg1)));
	readData2 <= regArr(to_integer(unsigned(readReg2)));
end registerfile;




library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;



-- Data Memory Architecture

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




library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


-- -- Program Counter Architecture

architecture behavioral of PC is
	signal PC: std_logic_vector(31 downto 0);
begin
	process(clock, reset)
	begin 
		if reset = '1' then
			PC <= X"00000000";
		elsif(clock'event and clock ='1') then
			PC <= PC + X"00000001";
		end if;
	end process;
	PC_out <= PC;
end behavioral;



-- MIPS Architecture

architecture dataflow of MIPS is
	signal PC_current, PC_next : std_logic_vector(31 downto 0);

	signal inputA, inputB, ALU_Res : std_logic_vector(31 downto 0);
	signal ALU_Ctl : std_logic_vector(2 downto 0);
	signal zero : std_logic;

	signal readData1, readData2 : std_logic_vector(31 downto 0);
	signal readReg1, readReg2, writeReg : std_logic_vector(4 downto 0);
	signal writeData : std_logic_vector(31 downto 0);
	signal we : std_logic;

	signal memRead, memWrite_en : std_logic;
	signal memAddr : std_logic_vector(31 downto 0);
	signal memWriteData, memReadData : std_logic_vector(31 downto 0);

	-- ALU
	component ALU is port
	(
		inputA, inputB: in std_logic_vector(31 downto 0);
		ALU_Ctl: in std_logic_vector(2 downto 0);
		ALU_Result: out std_logic_vector(31 downto 0);
		zero : out std_logic
	);
	end component ALU;

	
	-- Register File
	component RegisterFile is port
	(
		reset, clock, we : in std_logic;
		readReg1, readReg2, writeReg : in std_logic_vector(4 downto 0);
		writeData: in std_logic_vector(31 downto 0);
		readData1, readData2 : out std_logic_vector(31 downto 0)
	);
	end component RegisterFile;

	-- Data Memory
	component DataMemory is port
	(
		clock, memRead, memWrite_en : in std_logic;
		addr, writeData : in std_logic_vector(31 downto 0);
		readData : out std_logic_vector(31 downto 0)
	);
	end component DataMemory;

	-- Program Counter
	component PC is port 
	(
		clock, reset: in std_logic;	
		PC_out: out std_logic_vector(31 downto 0)
	);
	end component PC;


begin
	ALU_init: ALU
		port map
		(
			inputA => inputA,
			inputB => inputB,
			ALU_Ctl => ALU_Ctl,
			ALU_Result => ALU_Res,
			zero => zero
		);

	RegFile_init: RegisterFile
		port map
		(
			reset => reset,
			clock => clock,
			we => we,
			readReg1 => inputA(25 downto 21),
			readReg2 => inputB(20 downto 16),
			writeReg => writeReg,
			writeData => writeData,
			readData1 => readData1,
			readData2 => readData2
		);

	DataMemory_init : DataMemory
		port map
		(
			clock => clock,
			memRead => memRead,
			memWrite_en => memWrite_en,
			addr => memAddr,
			writeData => memWriteData,
			readData => memReadData
		);

	-- Program Counter Initialization
	PC_init: PC
		port map
		(
			clock => clock,
			reset => reset,
			PC_out => PC_current
		);

	inputA <= x"00000001";
	inputB <= x"00000001";
	ALU_Ctl <= "000";

	
end dataflow;