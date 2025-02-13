module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [5:0] zll_pure_dispatch10_in;
  logic [3:0] zll_pure_dispatch9_in;
  logic [3:0] zll_main_go1_in;
  logic [1:0] zll_main_go19_in;
  logic [1:0] zll_main_go16_in;
  logic [6:0] zll_main_go16_out;
  logic [6:0] zll_main_go15_in;
  logic [6:0] zll_main_go_in;
  logic [6:0] zll_main_go_out;
  logic [5:0] zll_pure_dispatch6_in;
  logic [2:0] zll_main_go10_in;
  logic [3:0] zll_main_go30_in;
  logic [2:0] zll_main_go29_in;
  logic [2:0] zll_main_go12_in;
  logic [2:0] zll_main_go38_in;
  logic [2:0] zll_main_go35_in;
  logic [7:0] zll_main_go34_in;
  logic [7:0] zll_main_go31_in;
  logic [3:0] zll_main_go7_in;
  logic [1:0] zll_main_go4_in;
  logic [0:0] zll_main_go4_out;
  logic [1:0] id_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  logic [3:0] zll_main_go28_in;
  logic [2:0] zll_main_go27_in;
  logic [2:0] zll_main_go11_in;
  logic [5:0] zll_pure_dispatch5_in;
  logic [3:0] zll_pure_dispatch4_in;
  logic [3:0] zll_main_go6_in;
  logic [1:0] zll_main_go26_in;
  logic [1:0] zll_main_go16_inR1;
  logic [6:0] zll_main_go16_outR1;
  logic [6:0] zll_main_go22_in;
  logic [6:0] zll_main_go_inR1;
  logic [6:0] zll_main_go_outR1;
  logic [5:0] zll_pure_dispatch2_in;
  logic [3:0] zll_pure_dispatch1_in;
  logic [3:0] zll_main_go2_in;
  logic [1:0] zll_main_go4_inR1;
  logic [0:0] zll_main_go4_outR1;
  logic [1:0] id_inR1;
  logic [1:0] rewirezupreludezuzazazuinR1;
  logic [0:0] rewirezupreludezuzaza_outR1;
  logic [0:0] __continue;
  logic [2:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign zll_pure_dispatch10_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_pure_dispatch9_in = {zll_pure_dispatch10_in[5], zll_pure_dispatch10_in[2], zll_pure_dispatch10_in[1], zll_pure_dispatch10_in[0]};
  assign zll_main_go1_in = {zll_pure_dispatch9_in[2], zll_pure_dispatch9_in[3], zll_pure_dispatch9_in[1], zll_pure_dispatch9_in[0]};
  assign zll_main_go19_in = {zll_main_go1_in[1], zll_main_go1_in[3]};
  assign zll_main_go16_in = zll_main_go19_in[1:0];
  ZLL_Main_go16  inst (zll_main_go16_in[1], zll_main_go16_in[0], zll_main_go16_out);
  assign zll_main_go15_in = zll_main_go16_out;
  assign zll_main_go_in = zll_main_go15_in[6:0];
  ZLL_Main_go  instR1 (zll_main_go_in[1], zll_main_go_in[0], zll_main_go_out);
  assign zll_pure_dispatch6_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_main_go10_in = {zll_pure_dispatch6_in[5], zll_pure_dispatch6_in[1], zll_pure_dispatch6_in[0]};
  assign zll_main_go30_in = {zll_main_go10_in[0], zll_main_go10_in[1], zll_main_go10_in[2], zll_main_go10_in[2]};
  assign zll_main_go29_in = {zll_main_go30_in[3], zll_main_go30_in[2], zll_main_go30_in[1]};
  assign zll_main_go12_in = {zll_main_go29_in[0], zll_main_go29_in[1], zll_main_go29_in[2]};
  assign zll_main_go38_in = {zll_main_go12_in[1], zll_main_go12_in[1], zll_main_go12_in[0]};
  assign zll_main_go35_in = zll_main_go38_in[2:0];
  assign zll_main_go34_in = {zll_main_go12_in[2], {4'h1, zll_main_go35_in[2], zll_main_go35_in[1], zll_main_go35_in[0]}};
  assign zll_main_go31_in = {zll_main_go34_in[7], zll_main_go34_in[6:0]};
  assign zll_main_go7_in = {zll_main_go31_in[7], zll_main_go31_in[2], zll_main_go31_in[1], zll_main_go31_in[0]};
  assign zll_main_go4_in = {zll_main_go7_in[3], zll_main_go7_in[2]};
  ZLL_Main_go4  instR2 (zll_main_go4_in[1], zll_main_go4_out);
  assign id_in = {zll_main_go7_in[3], zll_main_go7_in[2]};
  assign rewirezupreludezuzazazuin = {(id_in[0] == 1'h1) ? id_in[1] : zll_main_go4_out, zll_main_go7_in[2]};
  ReWirezuPreludezuzaza  instR3 (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign zll_main_go28_in = {zll_main_go10_in[0], zll_main_go10_in[1], zll_main_go10_in[2], zll_main_go10_in[2]};
  assign zll_main_go27_in = {zll_main_go28_in[3], zll_main_go28_in[2], zll_main_go28_in[1]};
  assign zll_main_go11_in = {zll_main_go27_in[0], zll_main_go27_in[1], zll_main_go27_in[2]};
  assign zll_pure_dispatch5_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_pure_dispatch4_in = {zll_pure_dispatch5_in[5], zll_pure_dispatch5_in[2], zll_pure_dispatch5_in[1], zll_pure_dispatch5_in[0]};
  assign zll_main_go6_in = {zll_pure_dispatch4_in[2], zll_pure_dispatch4_in[3], zll_pure_dispatch4_in[1], zll_pure_dispatch4_in[0]};
  assign zll_main_go26_in = {zll_main_go6_in[1], zll_main_go6_in[3]};
  assign zll_main_go16_inR1 = zll_main_go26_in[1:0];
  ZLL_Main_go16  instR4 (zll_main_go16_inR1[1], zll_main_go16_inR1[0], zll_main_go16_outR1);
  assign zll_main_go22_in = zll_main_go16_outR1;
  assign zll_main_go_inR1 = zll_main_go22_in[6:0];
  ZLL_Main_go  instR5 (zll_main_go_inR1[1], zll_main_go_inR1[0], zll_main_go_outR1);
  assign zll_pure_dispatch2_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_pure_dispatch1_in = {zll_pure_dispatch2_in[5], zll_pure_dispatch2_in[2], zll_pure_dispatch2_in[1], zll_pure_dispatch2_in[0]};
  assign zll_main_go2_in = {zll_pure_dispatch1_in[2], zll_pure_dispatch1_in[3], zll_pure_dispatch1_in[1], zll_pure_dispatch1_in[0]};
  assign zll_main_go4_inR1 = {zll_main_go2_in[3], zll_main_go2_in[2]};
  ZLL_Main_go4  instR6 (zll_main_go4_inR1[1], zll_main_go4_outR1);
  assign id_inR1 = {zll_main_go2_in[3], zll_main_go2_in[2]};
  assign rewirezupreludezuzazazuinR1 = {(id_inR1[0] == 1'h1) ? id_inR1[1] : zll_main_go4_outR1, zll_main_go2_in[2]};
  ReWirezuPreludezuzaza  instR7 (rewirezupreludezuzazazuinR1[1], rewirezupreludezuzazazuinR1[0], rewirezupreludezuzaza_outR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next, __st1_next} = (zll_pure_dispatch2_in[4:3] == 2'h1) ? {1'h1, rewirezupreludezuzaza_outR1, 2'h0, zll_main_go2_in[2], zll_main_go2_in[1], zll_main_go2_in[0]} : ((zll_pure_dispatch5_in[4:3] == 2'h2) ? zll_main_go_outR1 : ((zll_pure_dispatch6_in[4:3] == 2'h3) ? ((zll_main_go28_in[0] == 1'h1) ? {4'h9, zll_main_go11_in[2], zll_main_go11_in[1], zll_main_go11_in[0]} : {1'h1, rewirezupreludezuzaza_out, 2'h2, zll_main_go7_in[2], zll_main_go7_in[1], zll_main_go7_in[0]}) : zll_main_go_out));
  initial {__resumption_tag, __st0, __st1} <= 5'h19;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h19;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_go (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [6:0] res);
  logic [1:0] main_go_in;
  assign main_go_in = {arg0, arg1};
  assign res = {5'h16, main_go_in[1], main_go_in[0]};
endmodule

module ZLL_Main_go4 (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  assign rewirezupreludezuzazazuin = {arg0, arg0};
  ReWirezuPreludezuzaza  inst (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign res = rewirezupreludezuzaza_out;
endmodule

module ZLL_Main_go16 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [6:0] res);
  assign res = {5'h00, arg0, arg1};
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [1:0] lit_in;
  logic [1:0] id_in;
  assign lit_in = {arg0, arg1};
  assign id_in = {arg0, arg1};
  assign res = (id_in[1] == 1'h1) ? id_in[0] : 1'h0;
endmodule