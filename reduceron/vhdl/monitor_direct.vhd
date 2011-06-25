-- Interface module for use with the Virtual Lab
-- Provides the interface to the USB monitoring board
--
-- This version is for a processor-less system. Direct access
-- to the monitored components is provided, with the exception
-- of the VGA RAM. If you need access to the VGA RAM, use
-- monitor_8_bit or monitor_32_bit.
--
-- (Written by Jack Whitham.)


library ieee;
library unisim;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use unisim.vcomponents.all;

entity Monitor_Direct is
	port (
            -- External connections
            header_g    : inout std_logic_vector ( 2 to 19 ) ;
            header_h    : inout std_logic_vector ( 2 to 19 ) ;

            -- Internal connections
            switches    : out std_logic_vector ( 15 downto 0 ) ;
            leds        : in std_logic_vector ( 15 downto 0 ) ;
            seven_seg   : in std_logic_vector ( 7 downto 0 ) ;

            reset       : out std_logic ;
            clock       : in std_logic ) ;
end entity Monitor_Direct ;



architecture basic of Monitor_Direct is

    signal stb                  : std_logic ;
    signal ack                  : std_logic ;
    signal ack_d , we , m_reset : std_logic ;
    signal adr                  : std_logic_vector ( 11 downto 0 ) ;
    signal data_out             : std_logic_vector ( 7 downto 0 ) ;
    signal data_in              : std_logic_vector ( 7 downto 0 ) ;
    signal temp                 : std_logic_vector ( 7 downto 0 ) ;
    signal state                : Natural range 0 to 255 ;
    signal leds_copy            : std_logic_vector ( 15 downto 0 ) ;
    signal seven_seg_copy       : std_logic_vector ( 7 downto 0 ) ;

begin
    m : entity Monitor_Bus_Bridge
            port map (
                header_g => header_g ,
                header_h => header_h ,
                RST_O => m_reset ,
                CLK_I => clock ,
                ADR_I => adr ,
                DAT_O => data_in ,
                DAT_I => data_out ,
                STB_I => stb ,
                ACK_O => ack_d ,
                WE_I => we ,
                INT_O => open ) ;

    reset <= m_reset ;

    process ( clock ) is
    begin
        if ( clock'event )
        and ( clock = '1' )
        then
            ack <= ack_d ;
        end if ;
    end process ;

    -- Test driver
    driver : process ( clock , m_reset ) is
        variable next_state     : Natural range 0 to 255 ;
        variable wait_ack_up    : Boolean ;
        variable wait_ack_down  : Boolean ;
    begin
        if ( m_reset = '1' )
        then
            state <= 0 ;
        elsif ( clock'event )
        and ( clock = '0' )
        then
            next_state := state + 1 ;
            wait_ack_up := false ;
            wait_ack_down := false ;
            case state is     
            when 0 =>
                    data_out <= x"00" ;
                    adr <= x"A06" ;         -- Switch settings (low)
                    stb <= '0' ;
                    we <= '0' ;
            when 1 =>
                    stb <= '1' ;
            when 2 =>
                    wait_ack_up := true ;
            when 3 =>
                    switches ( 7 downto 0 ) <= data_in ;
            when 4 =>
                    stb <= '0' ;
                    wait_ack_down := true ;
            when 5 =>
                    adr <= x"A07" ;         -- Switch settings (high)
            when 6 =>
                    stb <= '1' ;
            when 7 =>
                    wait_ack_up := true ;
            when 8 =>
                    switches ( 15 downto 8 ) <= data_in ;
            when 9 =>
                    stb <= '0' ;
                    wait_ack_down := true ;
            when 10 =>
                    -- If the LEDs have changed state, we must update them.
                    if ( leds = leds_copy )
                    then
                        next_state := 18 ;
                    end if ;
            when 11 =>
                    leds_copy <= leds ;
                    adr <= x"A04" ;         -- LED settings (low)
                    we <= '1' ;
                    data_out <= leds ( 7 downto 0 ) ;
            when 12 =>
                    stb <= '1' ;
                    wait_ack_up := true ;
            when 13 =>
                    stb <= '0' ;
                    wait_ack_down := true ;
            when 14 =>
                    adr <= x"A05" ;         -- LED settings (high)
                    data_out <= leds_copy ( 15 downto 8 ) ;
            when 15 =>
                    stb <= '1' ;
            when 16 =>
                    wait_ack_up := true ;
            when 17 =>
                    stb <= '0' ;
                    wait_ack_down := true ;
            when 18 =>
                    -- End IF.

                    -- If the 7 seg has changed state, we must update them.
                    if ( seven_seg = seven_seg_copy )
                    then
                        next_state := 23 ;
                    end if ;
            when 19 =>
                    seven_seg_copy <= seven_seg ;
                    adr <= x"A02" ;
                    we <= '1' ;
                    data_out <= seven_seg ( 7 downto 0 ) ;
            when 20 =>
                    stb <= '1' ;
            when 21 =>
                    wait_ack_up := true ;
            when 22 =>
                    stb <= '0' ;
                    wait_ack_down := true ;

            when 23 =>
                    -- End IF.

                    we <= '0' ;
                    -- Repeat!
                    next_state := 0 ;
            when others =>
                    next_state := 0 ;
            end case ;
            if ( not ((( wait_ack_up ) and ( ack = '0' ))
                or (( wait_ack_down ) and ( ack = '1' ))) )
            then
                state <= next_state ;
            end if ;
        end if ;
    end process driver ;



end architecture basic ;

