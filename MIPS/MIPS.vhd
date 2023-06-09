
library ieee;
use ieee.std_logic_1164.all;


-- MIPS Entity

entity MIPS is port
(
	clock, reset: in std_logic
);
end MIPS;


library ieee;
use ieee.std_logic_1164.all;


-- ALU Entity

entity ALU is port
(
	inputA, inputB: in std_logic_vector(31 downto 0);
	ALU_Ctl: in std_logic_vector(3 downto 0);
	ALU_Result: out std_logic_vector(31 downto 0);
	zero: out std_logic
);
end ALU;



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
			when "0000" => tmp <= inputA + inputB;
			when "0001" => tmp <= inputA - inputB;
			when "0010" => tmp <= inputA AND inputB;
			when "0011" => tmp <= inputA OR inputB;
			when "1100" => tmp <= inputA NOR inputB;
			when others => tmp <= inputA + inputB;
		end case;

	end process;
	zero <= '1' when tmp = X"00000000" else '0';
	ALU_Result <= tmp;
end alu;




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



-- Register File Architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

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
use ieee.numeric_std.all;


-- Data Memory Entity

entity DataMemory is port
(
	addr: in std_logic_vector(31 downto 0);
	writeData: in std_logic_vector(31 downto 0);
	clock, memWrite_en, memRead: in std_logic;
	readData: out std_logic_vector(31 downto 0)
);
end DataMemory;


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

entity mux21 is port
	(
		inputA, inputB : in std_logic_vector(4 downto 0);
		regDst: in std_logic;
		result: out std_logic_vector(4 downto 0)
	);
end mux21;


architecture mux5b of mux21 is
begin
	process(regDst, inputA, inputB)
	begin
		if regDst = '0' then
			result <= inputA;
		elsif regDst = '1' then
			result <= inputB;
		end if;
	end process;
end mux5b;



library ieee;
use ieee.std_logic_1164.all;

entity mux21_32 is port
	(
		A, B : in std_logic_vector(31 downto 0);
		S : in std_logic;
		res: out std_logic_vector(31 downto 0)
	);
end mux21_32;

architecture mux32 of mux21_32 is
begin
	process(A, B, S)
	begin
		if S = '0' then
			res <= A;
		elsif S = '1' then
			res <= B;
		end if;
	end process;
end mux32;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity InstructionMemory is 
port
(
	PC: in std_logic_vector(31 downto 0);
	clock: in std_logic;
	instruction: out std_logic_vector(31 downto 0)
);
end InstructionMemory;


architecture InstructionMem of InstructionMemory is
	-- signal addr : std_logic_vector(31 downto 0);
	type iArray is array(0 to 15) of std_logic_vector(31 downto 0);
 
	signal Instructions: iArray := 
	(
		-- addi $0, $0, 0
		"00100000000000000000000000000000",
		-- addi $2, $2, 0
		"00100000010001000000000000000000",
		-- addi $2, $4, 0
		"00100000100000100000000000000000",
		-- addi $3, $0, 1
		"00100000000000110000000000000001",
		-- addi $5, $0, 3
		"00100000000001010000000000000011",
		-- add $6, $3, $0
		"00000000011000000011000000100000",
		-- sw $6, 0($4)
		"10101100100001100000000000000000",
		-- addi $3, $3, 1
		"00100000011000110000000000000001",
		-- addi $4, $4, 1
		"00100000100001000000000000000001",
		-- addi $5, $5, -1
		"00100000101001011111111111111111",
		-- bne $5, $0, L1
		"00010100000001011111111111111011",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000"
	);
begin
	process(clock)
	begin
		if clock'event and clock = '1' then
			instruction <= Instructions(to_integer(unsigned(PC)));
		end if;
	end process;
end InstructionMem;



library ieee;
use ieee.std_logic_1164.all;

entity PC is port
(
	PC_in: in std_logic_vector(31 downto 0);
	PC_out : out std_logic_vector(31 downto 0)
);
end PC;


-- Program Counter Architecture

architecture behavioral of PC is
begin
	PC_out <= PC_in;
end behavioral;




library ieee;
use ieee.std_logic_1164.all;

 entity SignExtension is port
	(
		inBits: in std_logic_vector(15 downto 0);
		outBits: out std_logic_vector(31 downto 0)
	);
end SignExtension;



library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


architecture SignEx of SignExtension is
begin
	process(inBits)
	begin
		if inBits(15) = '0' then
			outBits <= X"0000" & inBits;
		elsif inBits(15) = '1' then
			outBits <= X"FFFF" & inBits;
		end if;
	end process;
end SignEx;




library ieee;
use ieee.std_logic_1164.all;


entity ALU_Ctl is port
(
	Funct: in std_logic_vector(5 downto 0);
	ALUOp: in std_logic_vector(1 downto 0);
	ALU_control: out std_logic_vector(3 downto 0)
);
end ALU_Ctl;


architecture Control of ALU_Ctl is 
begin
	ALU_Control(3) <= '0';
	ALU_Control(2) <= ALUOp(0) OR (ALUOp(1) AND Funct(1));
	ALU_Control(1) <= NOT ALUOp(1) OR NOT Funct(2);
	ALU_Control(0) <= ALUOp(1) AND (Funct(3) OR Funct(0));
end Control;



library ieee;
use ieee.std_logic_1164.all;


entity Control_Unit is port
(
	opcode: in std_logic_vector(5 downto 0);
	RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch : out std_logic;
	ALUOp : out std_logic_vector(1 downto 0)
	
);
end Control_Unit;


architecture Behavioral of Control_Unit is
begin
	process(opcode)
	begin
		case opcode is
			-- For add and sub Instructions
			when "000000" =>
				RegDst <= '1';
				ALUSrc <= '0';
				MemtoReg <= '0';
				RegWrite <= '1';
				MemRead <= '0';
				MemWrite <= '0';
				Branch <= '0';
				ALUOp <= "10";

			-- For addi Instructions
			when "001000" =>
				RegDst <= '0';
				ALUSrc <= '1';
				MemtoReg <= '0';
				RegWrite <= '1';
				MemRead <= '0';
				MemWrite <= '0';
				Branch <= '0';
				ALUOp <= "00";
			
			-- For Load Instructions
			when "100011" =>
				RegDst <= '0';
				ALUSrc <= '1';
				MemtoReg <= '1';
				RegWrite <= '1';
				MemRead <= '1';
				MemWrite <= '0';
				Branch <= '0';
				ALUOp <= "00";

			-- For Store Instructions
			when "101011" =>
				RegDst <= '0';
				ALUSrc <= '1';
				MemtoReg <= '0';
				RegWrite <= '0';
				MemRead <= '0';
				MemWrite <= '1';
				Branch <= '0';
				ALUOp <= "00";

			-- For Branch Instructions
			when "000100" =>
				RegDst <= 'X';
				ALUSrc <= '0';
				MemtoReg <= 'X';
				RegWrite <= '0';
				MemRead <= '0';
				MemWrite <= '0';
				Branch <= '1';
				ALUOp <= "01";
			when others =>
		                RegDst    <= 'X';
		                ALUSrc    <= 'X';
		                MemtoReg  <= 'X';
		                RegWrite  <= 'X';
		                MemRead   <= 'X';
		                MemWrite  <= 'X';
		                Branch    <= 'X';
		                ALUOp     <= "XX";
		end case;
	end process;
end Behavioral;




library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity PCaddr is port
	(
		PC: in std_logic_vector(31 downto 0);
		PCnext: out std_logic_vector (31 downto 0)
	);
end PCaddr;



library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


architecture PCnext of PCaddr is
begin
	PCnext <= PC + 1;
end PCnext;



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


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;



-- MIPS Architecture

architecture dataflow of MIPS is
	signal pcn, PC_current, PC_next : std_logic_vector(31 downto 0);
	signal instr : std_logic_vector(31 downto 0);
	signal regdst, memtoreg, branch, memread, memwrite, alusrc, regwrite : std_logic;
	signal aluop : std_logic_vector(1 downto 0);
	signal writereg : std_logic_vector(4 downto 0);

	signal regwrite_en : std_logic;
	signal readdata1, readdata2 : std_logic_vector(31 downto 0);
	
	signal signExout : std_logic_vector(31 downto 0);
	signal alu_op : std_logic_vector(1 downto 0);
	signal aluctl : std_logic_vector(3 downto 0);
	
	signal aluin : std_logic_vector(31 downto 0);
	signal alures : std_logic_vector(31 downto 0);
	signal Zero : std_logic;
	
	signal readdata : std_logic_vector(31 downto 0);
	signal towritedata: std_logic_vector(31 downto 0);

	signal pctomux: std_logic_vector(31 downto 0);
	signal addr2res : std_logic_vector(31 downto 0);
	signal saddr : std_logic;

	-- signal OPCode : std_logic_vector(5 downto 0);

	-- ALU
	component ALU is port
	(
		inputA, inputB: in std_logic_vector(31 downto 0);
		ALU_Ctl: in std_logic_vector(3 downto 0);
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


	component InstructionMemory is port
	(
		PC : in std_logic_vector(31 downto 0);
		clock : in std_logic;
		instruction: out std_logic_vector(31 downto 0)
	);
	end component InstructionMemory;


	component mux21 is port
	(
		inputA, inputB : in std_logic_vector(4 downto 0);
		regDst : in std_logic;
		result : out std_logic_vector(4 downto 0)
	);
	end component mux21;

	component mux21_32 is port
	(
		A, B : in std_logic_vector(31 downto 0);
		S : in std_logic;
		res: out std_logic_vector(31 downto 0)
	);
	end component mux21_32;

	-- Program Counter
	component PC is port 
	(
		PC_in : in std_logic_vector(31 downto 0);
		PC_out: out std_logic_vector(31 downto 0)
	);
	end component PC;

	component PCaddr is port
	(
		PC : in std_logic_vector(31 downto 0);
		PCnext : out std_logic_vector(31 downto 0)
	);
	end component PCaddr;

	component SignExtension is port
	(
		inBits : in std_logic_vector(15 downto 0);
		outBits : out std_logic_vector(31 downto 0)
	);
	end component SignExtension;

	component Control_Unit is port
  	(
	    opcode : std_logic_vector(5 downto 0);
	    RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch : out std_logic;
	    ALUOp : out std_logic_vector(1 downto 0)  
  	);
        end component Control_Unit;

	component ALU_Ctl is port
	(
		Funct : std_logic_vector(5 downto 0);
		ALUOp: in std_logic_vector(1 downto 0);
		ALU_control: out std_logic_vector(3 downto 0)
	);
	end component ALU_Ctl;

	component PCafter is port
	(
		PCafter, signex: in std_logic_vector(31 downto 0);
		PCtomux: out std_logic_vector (31 downto 0)
	);
	end component PCafter;

begin
	process(clock, reset)
	begin
		if(reset = '1') then
			PC_current <= X"00000000";
		else
			PC_current <= PC_next;
		end if;
	end process;
	-- Program Counter Initialization
	PC_init: PC
		port map
		(
			PC_in => addr2res,		
			PC_out => Pcn
		);

	
	InstMemory_init: InstructionMemory
		port map
		(
			PC => PC_current,
			clock => clock,
			instruction => instr
		);
	
	CtrlUnit_init: Control_Unit
    		port map
    		(
      			opcode => instr(31 downto 26),		
			RegDst => regdst,
			MemtoReg => memtoreg,
			ALUOp => aluop,
			Branch => branch,
			MemRead => memread,			
			MemWrite => memwrite,
			ALUSrc => alusrc,
			RegWrite => regwrite
    		);

	
	reg_mux: mux21
		port map
		(
			regDst => regDst,
			inputA => instr(20 downto 16),
			inputB => instr(15 downto 11),
			result => writereg
		);


	RegFile_init: RegisterFile
		port map
		(
			reset => reset,
			clock => clock,
			we => regwrite_en,
			readReg1 => instr (25 downto 21),
			readReg2 => instr (20 downto 16),
			writeReg => writereg,
			writeData => towritedata,
			ReadData1 => readdata1,
			ReadData2 => readdata2
		);

	
	SignEx_init: SignExtension
		port map
		(
			inBits => instr(15 downto 0),
			outBits => signExout
		);
	
	RegfiletoALU_mux : mux21_32
		port map
		(
			A => readdata2,
			B => signExout,
			S => alusrc,
			res => aluin
		);

	ALU_Ctl_init : ALU_Ctl
	port map
		(
			Funct => instr(5 downto 0),
			ALUOp => aluop,
			ALU_control => aluctl
		);


	ALU_init: ALU
		port map
		(
			inputA => readdata1,
			inputB => aluin,
			ALU_Ctl => aluctl,
			ALU_Result => alures,
			zero => Zero
		);

	DataMemory_init : DataMemory
		port map
		(
			clock => clock,
			memRead => memread,
			memWrite_en => memwrite,
			addr => alures,
			writeData => readdata2,
			readData => readdata
		);
	
	datamem_mux : mux21_32
		port map
		(
			A => alures,	
			B => readdata,
			S => memtoreg,
			res => towritedata
		);

	PCaddr_init : PCaddr
		port map
		(
			PC => PC_current,
			PCnext => PC_next
		);

	PCaddr2_init: PCafter
		port map
		(
			PCafter => PC_next,
			signex => signExout,
			PCtomux => pctomux
		);
	

	saddr <= branch AND Zero;

	addrmux : mux21_32
		port map
		(
			A => PC_next,
			B => pctomux,
			S => saddr,
			res => addr2res
		);
	
end dataflow;