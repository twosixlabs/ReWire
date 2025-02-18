module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [5:0] zll_pure_dispatch10_in;
  logic [6:0] zll_pure_dispatch10_out;
  logic [5:0] zll_pure_dispatch1_in;
  logic [3:0] zll_pure_dispatch9_in;
  logic [3:0] zll_main_go7_in;
  logic [1:0] zll_main_go11_in;
  logic [0:0] zll_main_go11_out;
  logic [1:0] id_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  logic [5:0] zll_pure_dispatch10_inR1;
  logic [6:0] zll_pure_dispatch10_outR1;
  logic [5:0] zll_pure_dispatch4_in;
  logic [2:0] zll_main_go20_in;
  logic [3:0] zll_main_go19_in;
  logic [2:0] zll_main_go29_in;
  logic [2:0] zll_main_go26_in;
  logic [2:0] zll_main_go16_in;
  logic [2:0] zll_main_go22_in;
  logic [2:0] zll_main_go36_in;
  logic [7:0] zll_main_go25_in;
  logic [7:0] zll_main_go32_in;
  logic [3:0] zll_main_go34_in;
  logic [1:0] zll_main_go11_inR1;
  logic [0:0] zll_main_go11_outR1;
  logic [1:0] id_inR1;
  logic [1:0] rewirezupreludezuzazazuinR1;
  logic [0:0] rewirezupreludezuzaza_outR1;
  logic [3:0] zll_main_go33_in;
  logic [2:0] zll_main_go31_in;
  logic [2:0] zll_main_go30_in;
  logic [0:0] __continue;
  logic [2:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign zll_pure_dispatch10_in = {__in0, {__resumption_tag, __st0, __st1}};
  ZLL_Pure_dispatch10  inst (zll_pure_dispatch10_in[5], zll_pure_dispatch10_in[2], zll_pure_dispatch10_in[1], zll_pure_dispatch10_in[0], zll_pure_dispatch10_out);
  assign zll_pure_dispatch1_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_pure_dispatch9_in = {zll_pure_dispatch1_in[5], zll_pure_dispatch1_in[2], zll_pure_dispatch1_in[1], zll_pure_dispatch1_in[0]};
  assign zll_main_go7_in = {zll_pure_dispatch9_in[2], zll_pure_dispatch9_in[3], zll_pure_dispatch9_in[1], zll_pure_dispatch9_in[0]};
  assign zll_main_go11_in = {zll_main_go7_in[3], zll_main_go7_in[2]};
  ZLL_Main_go11  instR1 (zll_main_go11_in[1], zll_main_go11_out);
  assign id_in = {zll_main_go7_in[3], zll_main_go7_in[2]};
  assign rewirezupreludezuzazazuin = {(id_in[0] == 1'h1) ? id_in[1] : zll_main_go11_out, zll_main_go7_in[2]};
  ReWirezuPreludezuzaza  instR2 (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign zll_pure_dispatch10_inR1 = {__in0, {__resumption_tag, __st0, __st1}};
  ZLL_Pure_dispatch10  instR3 (zll_pure_dispatch10_inR1[5], zll_pure_dispatch10_inR1[2], zll_pure_dispatch10_inR1[1], zll_pure_dispatch10_inR1[0], zll_pure_dispatch10_outR1);
  assign zll_pure_dispatch4_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_main_go20_in = {zll_pure_dispatch4_in[5], zll_pure_dispatch4_in[1], zll_pure_dispatch4_in[0]};
  assign zll_main_go19_in = {zll_main_go20_in[0], zll_main_go20_in[1], zll_main_go20_in[2], zll_main_go20_in[2]};
  assign zll_main_go29_in = {zll_main_go19_in[3], zll_main_go19_in[2], zll_main_go19_in[1]};
  assign zll_main_go26_in = {zll_main_go29_in[0], zll_main_go29_in[1], zll_main_go29_in[2]};
  assign zll_main_go16_in = {zll_main_go26_in[1], zll_main_go26_in[1], zll_main_go26_in[0]};
  assign zll_main_go22_in = zll_main_go16_in[2:0];
  assign zll_main_go36_in = {zll_main_go22_in[1], zll_main_go22_in[2], zll_main_go22_in[0]};
  assign zll_main_go25_in = {zll_main_go26_in[2], {4'h0, zll_main_go36_in[1], zll_main_go36_in[2], zll_main_go36_in[0]}};
  assign zll_main_go32_in = {zll_main_go25_in[7], zll_main_go25_in[6:0]};
  assign zll_main_go34_in = {zll_main_go32_in[7], zll_main_go32_in[2], zll_main_go32_in[1], zll_main_go32_in[0]};
  assign zll_main_go11_inR1 = {zll_main_go34_in[3], zll_main_go34_in[2]};
  ZLL_Main_go11  instR4 (zll_main_go11_inR1[1], zll_main_go11_outR1);
  assign id_inR1 = {zll_main_go34_in[3], zll_main_go34_in[2]};
  assign rewirezupreludezuzazazuinR1 = {(id_inR1[0] == 1'h1) ? id_inR1[1] : zll_main_go11_outR1, zll_main_go34_in[2]};
  ReWirezuPreludezuzaza  instR5 (rewirezupreludezuzazazuinR1[1], rewirezupreludezuzazazuinR1[0], rewirezupreludezuzaza_outR1);
  assign zll_main_go33_in = {zll_main_go20_in[0], zll_main_go20_in[1], zll_main_go20_in[2], zll_main_go20_in[2]};
  assign zll_main_go31_in = {zll_main_go33_in[3], zll_main_go33_in[2], zll_main_go33_in[1]};
  assign zll_main_go30_in = {zll_main_go31_in[0], zll_main_go31_in[1], zll_main_go31_in[2]};
  assign {__continue, __out0, __resumption_tag_next, __st0_next, __st1_next} = (zll_pure_dispatch4_in[4:3] == 2'h1) ? ((zll_main_go33_in[0] == 1'h1) ? {4'hb, zll_main_go30_in[2], zll_main_go30_in[1], zll_main_go30_in[0]} : {1'h1, rewirezupreludezuzaza_outR1, 2'h2, zll_main_go34_in[2], zll_main_go34_in[1], zll_main_go34_in[0]}) : ((zll_pure_dispatch10_inR1[4:3] == 2'h2) ? zll_pure_dispatch10_outR1 : ((zll_pure_dispatch1_in[4:3] == 2'h3) ? {1'h1, rewirezupreludezuzaza_out, 2'h0, zll_main_go7_in[2], zll_main_go7_in[1], zll_main_go7_in[0]} : zll_pure_dispatch10_out));
  initial {__resumption_tag, __st0, __st1} <= 5'h9;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h9;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Pure_dispatch10 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [6:0] res);
  logic [3:0] zll_pure_dispatch8_in;
  logic [3:0] zll_main_go21_in;
  logic [1:0] zll_main_go35_in;
  logic [1:0] zll_main_go18_in;
  logic [6:0] zll_main_go12_in;
  logic [6:0] zll_main_go37_in;
  logic [1:0] main_go_in;
  assign zll_pure_dispatch8_in = {arg0, arg1, arg2, arg3};
  assign zll_main_go21_in = {zll_pure_dispatch8_in[2], zll_pure_dispatch8_in[3], zll_pure_dispatch8_in[1], zll_pure_dispatch8_in[0]};
  assign zll_main_go35_in = {zll_main_go21_in[1], zll_main_go21_in[3]};
  assign zll_main_go18_in = zll_main_go35_in[1:0];
  assign zll_main_go12_in = {5'h2, zll_main_go18_in[1], zll_main_go18_in[0]};
  assign zll_main_go37_in = zll_main_go12_in[6:0];
  assign main_go_in = {zll_main_go37_in[1], zll_main_go37_in[0]};
  assign res = {5'h12, main_go_in[1], main_go_in[0]};
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

module ZLL_Main_go11 (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  assign rewirezupreludezuzazazuin = {arg0, arg0};
  ReWirezuPreludezuzaza  inst (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign res = rewirezupreludezuzaza_out;
endmodule