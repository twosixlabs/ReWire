module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [17:0] zll_pure_dispatch_in;
  logic [18:0] zll_pure_dispatch_out;
  logic [17:0] zll_pure_dispatch1_in;
  logic [15:0] zll_main_reset1_in;
  logic [7:0] zll_main_reset6_in;
  logic [18:0] zll_main_loop8_in;
  logic [18:0] zll_main_loop8_out;
  logic [17:0] zll_pure_dispatch_inR1;
  logic [18:0] zll_pure_dispatch_outR1;
  logic [0:0] __continue;
  logic [1:0] __resumption_tag;
  logic [7:0] __st0;
  logic [1:0] __resumption_tag_next;
  logic [7:0] __st0_next;
  assign zll_pure_dispatch_in = {__in0, {__resumption_tag, __st0}};
  ZLL_Pure_dispatch  inst (zll_pure_dispatch_in[17:10], zll_pure_dispatch_in[7:0], zll_pure_dispatch_out);
  assign zll_pure_dispatch1_in = {__in0, {__resumption_tag, __st0}};
  assign zll_main_reset1_in = {zll_pure_dispatch1_in[17:10], zll_pure_dispatch1_in[7:0]};
  assign zll_main_reset6_in = zll_main_reset1_in[15:8];
  assign zll_main_loop8_in = {11'h200, zll_main_reset6_in[7:0]};
  ZLL_Main_loop8  instR1 (zll_main_loop8_in[18:0], zll_main_loop8_out);
  assign zll_pure_dispatch_inR1 = {__in0, {__resumption_tag, __st0}};
  ZLL_Pure_dispatch  instR2 (zll_pure_dispatch_inR1[17:10], zll_pure_dispatch_inR1[7:0], zll_pure_dispatch_outR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = (zll_pure_dispatch_inR1[9:8] == 2'h1) ? zll_pure_dispatch_outR1 : ((zll_pure_dispatch1_in[9:8] == 2'h2) ? zll_main_loop8_out : zll_pure_dispatch_out);
  initial {__resumption_tag, __st0} <= 10'h200;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 10'h200;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_Main_loop8 (input logic [18:0] arg0,
  output logic [18:0] res);
  logic [18:0] zll_main_loop_in;
  logic [7:0] main_loop_in;
  logic [15:0] zll_main_loop17_in;
  logic [15:0] zll_main_loop15_in;
  logic [18:0] zll_main_loop14_in;
  logic [18:0] zll_main_loop13_in;
  logic [15:0] zll_main_loop4_in;
  logic [7:0] resize_in;
  logic [0:0] msbit_in;
  logic [0:0] rewire_prelude_not_in;
  logic [0:0] rewire_prelude_not_out;
  logic [7:0] resize_inR1;
  logic [0:0] msbit_inR1;
  logic [0:0] rewire_prelude_not_inR1;
  logic [0:0] rewire_prelude_not_outR1;
  logic [8:0] zll_main_loop12_in;
  logic [7:0] zll_main_loop6_in;
  logic [8:0] zll_main_loop11_in;
  logic [7:0] zll_main_loop5_in;
  assign zll_main_loop_in = arg0;
  assign main_loop_in = zll_main_loop_in[7:0];
  assign zll_main_loop17_in = {main_loop_in[7:0], main_loop_in[7:0]};
  assign zll_main_loop15_in = zll_main_loop17_in[15:0];
  assign zll_main_loop14_in = {3'h0, zll_main_loop15_in[15:8], zll_main_loop15_in[7:0]};
  assign zll_main_loop13_in = zll_main_loop14_in[18:0];
  assign zll_main_loop4_in = {zll_main_loop13_in[15:8], zll_main_loop13_in[7:0]};
  assign resize_in = zll_main_loop4_in[15:8];
  assign msbit_in = resize_in[0];
  assign rewire_prelude_not_in = msbit_in[0];
  ReWire_Prelude_not  inst (rewire_prelude_not_in[0], rewire_prelude_not_out);
  assign resize_inR1 = zll_main_loop4_in[15:8];
  assign msbit_inR1 = resize_inR1[0];
  assign rewire_prelude_not_inR1 = msbit_inR1[0];
  ReWire_Prelude_not  instR1 (rewire_prelude_not_inR1[0], rewire_prelude_not_outR1);
  assign zll_main_loop12_in = {zll_main_loop4_in[7:0], rewire_prelude_not_outR1};
  assign zll_main_loop6_in = zll_main_loop12_in[8:1];
  assign zll_main_loop11_in = {zll_main_loop4_in[7:0], rewire_prelude_not_out};
  assign zll_main_loop5_in = zll_main_loop11_in[8:1];
  assign res = (zll_main_loop11_in[0] == 1'h1) ? {11'h408, zll_main_loop5_in[7:0]} : {11'h409, zll_main_loop6_in[7:0]};
endmodule

module ZLL_Pure_dispatch (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [18:0] res);
  logic [15:0] zll_main_loop1_in;
  logic [18:0] zll_main_loop8_in;
  logic [18:0] zll_main_loop8_out;
  assign zll_main_loop1_in = {arg0, arg1};
  assign zll_main_loop8_in = {11'h200, zll_main_loop1_in[7:0]};
  ZLL_Main_loop8  inst (zll_main_loop8_in[18:0], zll_main_loop8_out);
  assign res = zll_main_loop8_out;
endmodule

module ReWire_Prelude_not (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule