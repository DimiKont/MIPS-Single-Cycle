-- General Control Unit for the Circuit

library ieee;
use ieee.std_logic_1164.all;

entity Control_Unit is port
(
	opcode: in std_logic_vector(5 downto 0);
	RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, BNE : out std_logic;
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
			-- For BNE Instructions
			when "000101" =>
				RegDst <= 'X';
				ALUSrc <= '0';
				MemtoReg <= 'X';
				RegWrite <= '0';
				MemRead <= '0';
				MemWrite <= '0';
				Branch <= '1';
				ALUOp <= "11";
			-- For others
			when others =>
		                RegDst    <= '0';
		                ALUSrc    <= '0';
		                MemtoReg  <= '0';
		                RegWrite  <= '0';
		                MemRead   <= '0';
		                MemWrite  <= '0';
		                Branch    <= '0';
		                ALUOp     <= "00";
		end case;
	end process;
end Behavioral;

