-- ALU Control

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
