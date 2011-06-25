library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity top is
-- For simulation:
--port ( done : out std_logic
--     ; leds : out std_logic_vector(7 downto 0));
	
port ( clock : in std_logic
     ; switches : in std_logic_vector (7 downto 0)
     ; leds : out std_logic_vector(7 downto 0));
end top;

architecture Behavioral of top is
component Reduceron port (
finish : out std_logic;
result1 : out std_logic;
result2 : out std_logic;
result3 : out std_logic;
result4 : out std_logic;
result5 : out std_logic;
result6 : out std_logic;
result7 : out std_logic;
result8 : out std_logic;
result9 : out std_logic;
result10 : out std_logic;
result11 : out std_logic;
result12 : out std_logic;
result13 : out std_logic;
result14 : out std_logic;
result15 : out std_logic;
result16 : out std_logic;
result17 : out std_logic;
result18 : out std_logic;
state1 : out std_logic;
state2 : out std_logic;
state3 : out std_logic;
state4 : out std_logic;
state5 : out std_logic;
state6 : out std_logic;
state7 : out std_logic;
clock : in  std_logic
);
end component Reduceron;

-- For simulation:
--signal clock : std_logic;
--constant ClockPeriod : TIME := 100 ns;

signal saved_switches : std_logic_vector(7 downto 0);
signal result : std_logic_vector(17 downto 0);
signal fin : std_logic;
signal ticks, gcticks, primticks : std_logic_vector(31 downto 0);
signal state1, state2, state3, state4, state5, state6, state7 : std_logic;

begin
--  For simulation:
--  GENERATE_CLOCK: process is
--  begin
--  wait for (ClockPeriod / 2);
--  clock <= '1';
--  wait for (ClockPeriod / 2);
--  Clock <= '0';
--  end process;

  red_instance : Reduceron
    port map (
    finish => fin,
    result1 => result(0),
	 result2 => result(1),
	 result3 => result(2),
	 result4 => result(3),
	 result5 => result(4),
	 result6 => result(5),
	 result7 => result(6),
	 result8 => result(7),
    result9 => result(8),
	 result10 => result(9),
	 result11 => result(10),
	 result12 => result(11),
	 result13 => result(12),
	 result14 => result(13),
	 result15 => result(14),
	 result16 => result(15),
	 result17 => result(16),
	 result18 => result(17),
	 state1 => state1,
	 state2 => state2,
	 state3 => state3,
	 state4 => state4,
	 state5 => state5,
	 state6 => state6,
	 state7 => state7,
	 clock   => clock
	 );

   --leds <= result(7 downto 0);

   counter : process (clock) is
   begin
     if (rising_edge(clock)) then
		 saved_switches <= switches;
       if result = "000000000000000000" then
		     ticks <= ticks + 1;
       end if;
		 if state3 = '1' or state4 = '1' then
		     primticks <= primticks + 1;
		 end if;
		 if state6 = '1' then
		     gcticks <= gcticks + 1;
		 end if;
		 
  case saved_switches is
         when "00000001" => leds <= result(7 downto 0);
         when "00000010" => leds <= result(15 downto 8);
			
			when "00000100" => leds <= ticks(7 downto 0);
         when "00000101" => leds <= ticks(15 downto 8);			         			
         when "00000110" => leds <= ticks(23 downto 16);
			when "00000111" => leds <= ticks(31 downto 24);
			
			when "00001000" => leds <= primticks(7 downto 0);
         when "00001001" => leds <= primticks(15 downto 8);			         			
         when "00001010" => leds <= primticks(23 downto 16);
			when "00001011" => leds <= primticks(31 downto 24);
			
			when "00010000" => leds <= gcticks(7 downto 0);
         when "00010001" => leds <= gcticks(15 downto 8);			         			
         when "00010010" => leds <= gcticks(23 downto 16);
			when "00010011" => leds <= gcticks(31 downto 24);			
	      when others => leds <= (others => '0');
  end case;

	  end if;
  end process;

  
end Behavioral;
