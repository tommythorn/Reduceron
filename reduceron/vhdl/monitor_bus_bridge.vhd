-- 
-- This bridge links the internal Wishbone bus to the external
-- Monitor Bus, which goes across to the Virtual Lab monitoring
-- board. 8-bit read/write access to 4096 bytes of memory is available,
-- and there are 4 interrupt lines. 
--
--
-- (Written by Jack Whitham.)

library ieee;
library unisim;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use unisim.vcomponents.all;


entity Monitor_Bus_Bridge is
	port (
            -- External connections
            header_g    : inout std_logic_vector ( 2 to 19 ) ;
            header_h    : inout std_logic_vector ( 2 to 19 ) ;

            -- Internal connections
            RST_O       : out std_logic;
            CLK_I       : in std_logic;
            ADR_I       : in std_logic_vector ( 11 downto 0 ) ;
            DAT_I       : in std_logic_vector ( 7 downto 0 ) ;
            DAT_O       : out std_logic_vector ( 7 downto 0 ) ;
            STB_I       : in std_logic;
            ACK_O       : out std_logic;
            WE_I        : in std_logic;

            INT_O       : out std_logic_vector ( 3 downto 0 ) );
end Monitor_Bus_Bridge ;



architecture basic of Monitor_Bus_Bridge is

    constant bus_lock_time          : Natural := 15 ;
    signal lock_bus                 : std_logic ;
    signal bus_unlocked             : std_logic ;
    signal bus_active               : std_logic ;
    signal bus_lock_time_remaining  : Natural range 0 to bus_lock_time ;
    signal acknowledge              : std_logic ;
begin

    process ( CLK_I ) is
    begin
        if ( CLK_I = '1' )
        and ( CLK_I'event )
        then
            -- The bus lock prevents bus accesses from starting.
            -- It does not prevent them from finishing, however.
            if (( STB_I = '0' ) or ( bus_unlocked = '1' ))
            then
                header_g ( 2 to 19 ) <= ( others => 'Z' ) ;
                header_h ( 2 to 19 ) <= ( others => 'Z' ) ;
                header_g ( 4 to 15 ) <= ADR_I ;
                header_g ( 16 ) <= STB_I ;
                header_g ( 17 ) <= WE_I and STB_I ;
                header_h ( 9 to 16 ) <= ( others => 'Z' ) ;
                bus_active <= STB_I ;
                if (( STB_I = '1' ) and ( WE_I = '1' ))
                then
                    header_h ( 9 to 16 ) <= DAT_I ;
                end if ;
            end if ;
            
            if ( lock_bus = '1' )
            then
                bus_lock_time_remaining <= bus_lock_time ;
                bus_unlocked <= '0' ;
            elsif ( bus_lock_time_remaining /= 0 )
            then
                bus_lock_time_remaining <=
                    bus_lock_time_remaining - 1 ;
            else
                bus_unlocked <= '1' ;
            end if ;
        end if ;
    end process ;
    
    process ( CLK_I ) is
        variable ACK_O_in : std_logic ;
    begin
        if ( CLK_I = '0' )
        and ( CLK_I'event )
        then
            ACK_O_in := header_g ( 18 ) ;
            -- Bus locking is active whenever the ACK line is asserted.
            -- The bus remains locked for a short time after ACK falls,
            -- in order to prevent future STBs being falsely acknowledged.
            lock_bus <= ACK_O_in ;

            -- ACK_O must fall after STB_I falls. Hence it is dependant
            -- on the bus activity as well as the ACK input.
            acknowledge <= '0' ;
            if ( bus_active = '1' ) 
            and ( bus_unlocked = '0' )
            then
                acknowledge <= '1' ;
                DAT_O <= header_h ( 9 to 16 ) ;
            end if ;
            RST_O <= header_h ( 4 ) ;
            INT_O <= header_h ( 5 to 8 ) ;
        end if ;
    end process ;

    ACK_O <= acknowledge and STB_I ;

end architecture basic ;

