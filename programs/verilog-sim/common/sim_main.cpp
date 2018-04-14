#include <iostream>
#include "Vtoplevel.h"
#include "verilated.h"

using namespace std;

Vtoplevel *top;


vluint64_t main_time = 0;       // Current simulation time
// This is a 64-bit integer to reduce wrap over issues and
// allow modulus.  You can also use a double, if you wish.

double sc_time_stamp () {       // Called by $time in Verilog
    return main_time;           // converts to double, to match
                                // what SystemC does
}

int main(int argc, char **argv, char **env) {
    Verilated::commandArgs(argc, argv);

    top = new Vtoplevel;

    while (!top->finish) {
      top->clock = 0;
      top->eval();
      top->clock = 1;
      top->eval();
      ++main_time;
    }

    //    cout << "@" << main_time << " result " << top->r/8 << endl;
    cout << top->r/8 << endl;

    delete top;

    exit(0);
}
