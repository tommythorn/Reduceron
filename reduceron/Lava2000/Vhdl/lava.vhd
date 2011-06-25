---------------------------------------------------------------------
-- Lava Gates
---------------------------------------------------------------------
-- Koen Claessen, koen@cs.chalmers.se, 20000323

---------------------------------------------------------------------
-- entity declarations

entity
  vdd
is
port
  ( clk   : in bit
  ; outp  : out bit
  );
end entity vdd;

entity
  gnd
is
port
  ( clk   : in bit
  ; outp  : out bit
  );
end entity gnd;

entity
  id
is
port
  ( clk   : in bit
  ; inp   : in bit
  ; outp  : out bit
  );
end entity id;

entity
  inv
is
port
  ( clk   : in bit
  ; inp   : in bit
  ; outp  : out bit
  );
end entity inv;

entity
  and2
is
port
  ( clk   : in bit
  ; inp_1 : in bit
  ; inp_2 : in bit
  ; outp  : out bit
  );
end entity and2;

entity
  or2
is
port
  ( clk   : in bit
  ; inp_1 : in bit
  ; inp_2 : in bit
  ; outp  : out bit
  );
end entity or2;

entity
  xor2
is
port
  ( clk   : in bit
  ; inp_1 : in bit
  ; inp_2 : in bit
  ; outp  : out bit
  );
end entity xor2;

entity
  delay
is
port
  ( clk   : in bit
  ; init  : in bit
  ; inp   : in bit
  ; outp  : out bit
  );
end entity delay;

---------------------------------------------------------------------
-- behavioral descriptions

architecture
  behavioral
of
  vdd
is
begin
  outp <= '1';
end;

architecture
  behavioral
of
  gnd
is
begin
  outp <= '0';
end;

architecture
  behavioral
of
  id
is
begin
  outp <= inp;
end;

architecture
  behavioral
of
  inv
is
begin
  outp <= not inp;
end;

architecture
  behavioral
of
  and2
is
begin
  outp <= inp_1 and inp_2;
end;

architecture
  behavioral
of
  or2
is
begin
  outp <= inp_1 or inp_2;
end;

architecture
  behavioral
of
  xor2
is
begin
  outp <= inp_1 xor inp_2;
end;

architecture
  behavioral
of
  delay
is
begin
  latch : process is
    variable state : bit;
  begin
    state := init;
    loop
      wait until clk = '1';
      outp <= state;
      state := inp;
    end loop;
  end process latch;
end;

---------------------------------------------------------------------
-- the end.


