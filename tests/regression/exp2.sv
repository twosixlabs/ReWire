module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [99:0] __in0,
  output logic [99:0] __out0);
  logic [2099:0] zll_pure_dispatch_in;
  logic [2099:0] zll_main_dev7_in;
  logic [1099:0] main_ss$_in;
  logic [999:0] id_in;
  logic [999:0] id_inR1;
  logic [99:0] main_x2_in;
  logic [99:0] main_x2_out;
  logic [999:0] id_inR2;
  logic [999:0] id_inR3;
  logic [99:0] main_x2_inR1;
  logic [99:0] main_x2_outR1;
  logic [999:0] id_inR4;
  logic [999:0] id_inR5;
  logic [99:0] main_x2_inR2;
  logic [99:0] main_x2_outR2;
  logic [999:0] id_inR6;
  logic [999:0] id_inR7;
  logic [99:0] main_x2_inR3;
  logic [99:0] main_x2_outR3;
  logic [999:0] id_inR8;
  logic [999:0] id_inR9;
  logic [99:0] main_x2_inR4;
  logic [99:0] main_x2_outR4;
  logic [999:0] id_inR10;
  logic [999:0] id_inR11;
  logic [99:0] main_x2_inR5;
  logic [99:0] main_x2_outR5;
  logic [999:0] id_inR12;
  logic [999:0] id_inR13;
  logic [99:0] main_x2_inR6;
  logic [99:0] main_x2_outR6;
  logic [999:0] id_inR14;
  logic [999:0] id_inR15;
  logic [99:0] main_x2_inR7;
  logic [99:0] main_x2_outR7;
  logic [999:0] id_inR16;
  logic [999:0] id_inR17;
  logic [99:0] main_x2_inR8;
  logic [99:0] main_x2_outR8;
  logic [99:0] main_x2_inR9;
  logic [99:0] main_x2_outR9;
  logic [999:0] zll_main_dev3_in;
  logic [2100:0] zll_main_dev2_in;
  logic [2100:0] zll_main_dev4_in;
  logic [999:0] main_dev_in;
  logic [1999:0] zll_main_dev12_in;
  logic [1999:0] zll_main_dev5_in;
  logic [2100:0] zll_main_dev11_in;
  logic [2100:0] zll_main_dev10_in;
  logic [1999:0] zll_main_dev9_in;
  logic [999:0] id_inR18;
  logic [0:0] __continue;
  logic [999:0] __resumption_tag;
  logic [999:0] __st0;
  logic [999:0] __resumption_tag_next;
  logic [999:0] __st0_next;
  assign zll_pure_dispatch_in = {__in0, {__resumption_tag, __st0}};
  assign zll_main_dev7_in = {zll_pure_dispatch_in[1999:1000], zll_pure_dispatch_in[2099:2000], zll_pure_dispatch_in[999:0]};
  assign main_ss$_in = {zll_main_dev7_in[2099:1100], zll_main_dev7_in[1099:1000]};
  assign id_in = main_ss$_in[1099:100];
  assign id_inR1 = {id_in[899:0], main_ss$_in[99:0]};
  assign main_x2_in = id_inR1[999:900];
  Main_x2  inst (main_x2_in[99:0], main_x2_out);
  assign id_inR2 = main_ss$_in[1099:100];
  assign id_inR3 = {id_inR2[899:0], main_ss$_in[99:0]};
  assign main_x2_inR1 = id_inR3[899:800];
  Main_x2  instR1 (main_x2_inR1[99:0], main_x2_outR1);
  assign id_inR4 = main_ss$_in[1099:100];
  assign id_inR5 = {id_inR4[899:0], main_ss$_in[99:0]};
  assign main_x2_inR2 = id_inR5[799:700];
  Main_x2  instR2 (main_x2_inR2[99:0], main_x2_outR2);
  assign id_inR6 = main_ss$_in[1099:100];
  assign id_inR7 = {id_inR6[899:0], main_ss$_in[99:0]};
  assign main_x2_inR3 = id_inR7[699:600];
  Main_x2  instR3 (main_x2_inR3[99:0], main_x2_outR3);
  assign id_inR8 = main_ss$_in[1099:100];
  assign id_inR9 = {id_inR8[899:0], main_ss$_in[99:0]};
  assign main_x2_inR4 = id_inR9[599:500];
  Main_x2  instR4 (main_x2_inR4[99:0], main_x2_outR4);
  assign id_inR10 = main_ss$_in[1099:100];
  assign id_inR11 = {id_inR10[899:0], main_ss$_in[99:0]};
  assign main_x2_inR5 = id_inR11[499:400];
  Main_x2  instR5 (main_x2_inR5[99:0], main_x2_outR5);
  assign id_inR12 = main_ss$_in[1099:100];
  assign id_inR13 = {id_inR12[899:0], main_ss$_in[99:0]};
  assign main_x2_inR6 = id_inR13[399:300];
  Main_x2  instR6 (main_x2_inR6[99:0], main_x2_outR6);
  assign id_inR14 = main_ss$_in[1099:100];
  assign id_inR15 = {id_inR14[899:0], main_ss$_in[99:0]};
  assign main_x2_inR7 = id_inR15[299:200];
  Main_x2  instR7 (main_x2_inR7[99:0], main_x2_outR7);
  assign id_inR16 = main_ss$_in[1099:100];
  assign id_inR17 = {id_inR16[899:0], main_ss$_in[99:0]};
  assign main_x2_inR8 = id_inR17[199:100];
  Main_x2  instR8 (main_x2_inR8[99:0], main_x2_outR8);
  assign main_x2_inR9 = main_ss$_in[99:0];
  Main_x2  instR9 (main_x2_inR9[99:0], main_x2_outR9);
  assign zll_main_dev3_in = {main_x2_out, main_x2_outR1, main_x2_outR2, main_x2_outR3, main_x2_outR4, main_x2_outR5, main_x2_outR6, main_x2_outR7, main_x2_outR8, main_x2_outR9};
  assign zll_main_dev2_in = {{101'h00000000000000000000000001, {10'h3e8{1'h0}}}, zll_main_dev3_in[999:0]};
  assign zll_main_dev4_in = zll_main_dev2_in[2100:0];
  assign main_dev_in = zll_main_dev4_in[999:0];
  assign zll_main_dev12_in = {main_dev_in[999:0], main_dev_in[999:0]};
  assign zll_main_dev5_in = zll_main_dev12_in[1999:0];
  assign zll_main_dev11_in = {{7'h65{1'h0}}, zll_main_dev5_in[1999:1000], zll_main_dev5_in[999:0]};
  assign zll_main_dev10_in = zll_main_dev11_in[2100:0];
  assign zll_main_dev9_in = {zll_main_dev10_in[1999:1000], zll_main_dev10_in[999:0]};
  assign id_inR18 = zll_main_dev9_in[1999:1000];
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = {1'h1, id_inR18[999:900], zll_main_dev9_in[1999:1000], zll_main_dev9_in[999:0]};
  initial {__resumption_tag, __st0} <= {11'h7d0{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {11'h7d0{1'h0}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module Main_x2 (input logic [99:0] arg0,
  output logic [99:0] res);
  logic [199:0] binop_in;
  assign binop_in = {arg0, 100'h0000000000000000000000002};
  assign res = binop_in[199:100] * binop_in[99:0];
endmodule