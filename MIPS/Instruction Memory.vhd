-- Instruction Memory

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity InstructionMemory is 
port
(
	PC: in std_logic_vector(31 downto 0);
	instruction: out std_logic_vector(31 downto 0)
);
end InstructionMemory;

architecture InstructionMem of InstructionMemory is
	type iArray is array(0 to 15) of std_logic_vector(31 downto 0);
 
	signal Instructions: iArray := 
	(
		-- addi $0, $0, 0
		--"00100000000000000000000000000000",
		-- addi $2, $2, 0
		--"00100000010000100000000000000000",
		-- addi $2, $4, 0
		--"00100000100000100000000000000000",
		-- Instruction above can be skipped because the Register File is reset at the start

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
		"00010100000001011111111111111010",
		-- Rest are zero
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000",
		"00000000000000000000000000000000"
	);
begin
	instruction <= Instructions(to_integer(unsigned(PC)));
end InstructionMem;
