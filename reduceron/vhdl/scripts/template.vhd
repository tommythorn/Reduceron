library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity Top is
            port( clock           : in std_logic;
                  burched_port_d  : inout std_logic_vector ( 2 to 19 );
                  burched_port_e  : inout std_logic_vector ( 2 to 19 ));
end entity Top;

architecture toplevel of Top is

    signal halted     : std_logic ;
    signal unwind     : std_logic ;
    signal unfold     : std_logic ;
    signal integ      : std_logic ;
    signal prim       : std_logic ;
    signal gc         : std_logic ;
    signal leds       : std_logic_vector ( 15 downto 0 ) ;
    signal switches   : std_logic_vector ( 15 downto 0 ) ;
    signal seven_seg  : std_logic_vector ( 7 downto 0 ) ;
    signal counter    : std_logic_vector ( 32 downto 0 ) ;
    signal ctr_halted : std_logic_vector ( 32 downto 0 ) ;
    signal ctr_unwind : std_logic_vector ( 32 downto 0 ) ;
    signal ctr_unfold : std_logic_vector ( 32 downto 0 ) ;
    signal ctr_integ  : std_logic_vector ( 32 downto 0 ) ;
    signal ctr_prim   : std_logic_vector ( 32 downto 0 ) ;
    signal ctr_gc     : std_logic_vector ( 32 downto 0 ) ;

begin

    mon : entity Monitor_Direct
        port map (
            header_g => burched_port_d ,
            header_h => burched_port_e ,
            switches => switches ,
            leds => leds ,
            seven_seg => seven_seg ,
            clock => clock ) ;
        
   red : entity CHANGEME -- Change this to the name of the file
                         -- produced by "reduceron".
    port map (
        clk => clock
       , result_0 => seven_seg(0)
       , result_1 => seven_seg(1)
       , result_2 => seven_seg(2)
       , result_3 => seven_seg(3)
       , result_4 => seven_seg(4)
       , result_5 => seven_seg(5)
       , result_6 => seven_seg(6)
       , result_7 => seven_seg(7)
       , status_0 => halted
       , status_1 => unwind
       , status_2 => unfold
       , status_3 => integ
       , status_4 => prim
       , status_5 => gc
    );
    
    process (clock, counter) is begin
       if ( clock = '1' ) and ( clock'event ) then
         if counter /= X"FFFFFFFF" then
           if halted = '0' then
             counter <= counter + 1;
           end if;
         end if;
         if unwind = '1' then
           ctr_unwind <= ctr_unwind + 1;
         end if;
         if unfold = '1' then
           ctr_unfold <= ctr_unfold + 1;
         end if;
         if integ = '1' then
           ctr_integ <= ctr_integ + 1;
         end if;
         if prim = '1' then
           ctr_prim <= ctr_prim + 1;
         end if;
         if gc = '1' then
           ctr_gc <= ctr_gc + 1;
         end if;
       end if;
    end process;
 
    process (switches, leds, counter) is begin
      if switches(1) = '1' then
        if switches(0) = '1' then
          leds <= counter(15 downto 0);
        else
          leds <= counter(31 downto 16);
        end if;
      end if;
      if switches(2) = '1' then
        if switches(0) = '1' then
          leds <= ctr_unwind(15 downto 0);
        else
          leds <= ctr_unwind(31 downto 16);
        end if;
      end if;
      if switches(3) = '1' then
        if switches(0) = '1' then
          leds <= ctr_unfold(15 downto 0);
        else
          leds <= ctr_unfold(31 downto 16);
        end if;
      end if;
      if switches(4) = '1' then
        if switches(0) = '1' then
          leds <= ctr_integ(15 downto 0);
        else
          leds <= ctr_integ(31 downto 16);
        end if;
      end if;
      if switches(5) = '1' then
        if switches(0) = '1' then
          leds <= ctr_prim(15 downto 0);
        else
          leds <= ctr_prim(31 downto 16);
        end if;
      end if;
      if switches(6) = '1' then
        if switches(0) = '1' then
          leds <= ctr_gc(15 downto 0);
        else
          leds <= ctr_gc(31 downto 16);
        end if;
      end if;
    end process;
    
end architecture toplevel;
