module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [20:0] zll_pure_dispatch15_in;
  logic [27:0] zll_pure_dispatch15_out;
  logic [20:0] zll_pure_dispatch14_in;
  logic [27:0] zll_pure_dispatch14_out;
  logic [20:0] zll_pure_dispatch14_inR1;
  logic [27:0] zll_pure_dispatch14_outR1;
  logic [20:0] zll_pure_dispatch8_in;
  logic [0:0] zll_main_loop4_in;
  logic [0:0] lit_in;
  logic [20:0] zll_pure_dispatch4_in;
  logic [27:0] zll_pure_dispatch4_out;
  logic [20:0] zll_pure_dispatch13_in;
  logic [27:0] zll_pure_dispatch13_out;
  logic [20:0] zll_pure_dispatch15_inR1;
  logic [27:0] zll_pure_dispatch15_outR1;
  logic [20:0] zll_pure_dispatch11_in;
  logic [16:0] zll_pure_dispatch10_in;
  logic [16:0] zll_pure_dispatch7_in;
  logic [16:0] zll_main_loop8_in;
  logic [16:0] zll_main_loop18_in;
  logic [15:0] zll_main_loop12_in;
  logic [15:0] binop_in;
  logic [15:0] main_loop10_in;
  logic [27:0] main_loop10_out;
  logic [16:0] zll_main_loop1_in;
  logic [15:0] zll_main_loop3_in;
  logic [15:0] main_loop10_inR1;
  logic [27:0] main_loop10_outR1;
  logic [20:0] zll_pure_dispatch12_in;
  logic [0:0] zll_main_loop9_in;
  logic [0:0] lit_inR1;
  logic [20:0] zll_pure_dispatch4_inR1;
  logic [27:0] zll_pure_dispatch4_outR1;
  logic [20:0] zll_pure_dispatch9_in;
  logic [27:0] zll_pure_dispatch9_out;
  logic [20:0] zll_pure_dispatch13_inR1;
  logic [27:0] zll_pure_dispatch13_outR1;
  logic [20:0] zll_pure_dispatch3_in;
  logic [0:0] zll_main_loop17_in;
  logic [0:0] lit_inR2;
  logic [20:0] zll_pure_dispatch9_inR1;
  logic [27:0] zll_pure_dispatch9_outR1;
  logic [0:0] __continue;
  logic [19:0] __resumption_tag;
  logic [19:0] __resumption_tag_next;
  assign zll_pure_dispatch15_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch15  inst (zll_pure_dispatch15_in[20], zll_pure_dispatch15_out);
  assign zll_pure_dispatch14_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch14  instR1 (zll_pure_dispatch14_in[20], zll_pure_dispatch14_out);
  assign zll_pure_dispatch14_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch14  instR2 (zll_pure_dispatch14_inR1[20], zll_pure_dispatch14_outR1);
  assign zll_pure_dispatch8_in = {__in0, __resumption_tag};
  assign zll_main_loop4_in = zll_pure_dispatch8_in[20];
  assign lit_in = zll_main_loop4_in[0];
  assign zll_pure_dispatch4_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch4  instR3 (zll_pure_dispatch4_in[20], zll_pure_dispatch4_out);
  assign zll_pure_dispatch13_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch13  instR4 (zll_pure_dispatch13_in[20], zll_pure_dispatch13_out);
  assign zll_pure_dispatch15_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch15  instR5 (zll_pure_dispatch15_inR1[20], zll_pure_dispatch15_outR1);
  assign zll_pure_dispatch11_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch10_in = {zll_pure_dispatch11_in[20], zll_pure_dispatch11_in[15:8], zll_pure_dispatch11_in[7:0]};
  assign zll_pure_dispatch7_in = {zll_pure_dispatch10_in[15:8], zll_pure_dispatch10_in[16], zll_pure_dispatch10_in[7:0]};
  assign zll_main_loop8_in = {zll_pure_dispatch7_in[16:9], zll_pure_dispatch7_in[7:0], zll_pure_dispatch7_in[8]};
  assign zll_main_loop18_in = {zll_main_loop8_in[16:9], zll_main_loop8_in[8:1], zll_main_loop8_in[0]};
  assign zll_main_loop12_in = {zll_main_loop18_in[16:9], zll_main_loop18_in[8:1]};
  assign binop_in = {zll_main_loop12_in[7:0], zll_main_loop12_in[15:8]};
  assign main_loop10_in = {zll_main_loop12_in[15:8], binop_in[15:8] + binop_in[7:0]};
  Main_loop10  instR6 (main_loop10_in[15:8], main_loop10_in[7:0], main_loop10_out);
  assign zll_main_loop1_in = {zll_main_loop8_in[16:9], zll_main_loop8_in[8:1], zll_main_loop8_in[0]};
  assign zll_main_loop3_in = {zll_main_loop1_in[16:9], zll_main_loop1_in[8:1]};
  assign main_loop10_inR1 = {zll_main_loop3_in[7:0], zll_main_loop3_in[15:8]};
  Main_loop10  instR7 (main_loop10_inR1[15:8], main_loop10_inR1[7:0], main_loop10_outR1);
  assign zll_pure_dispatch12_in = {__in0, __resumption_tag};
  assign zll_main_loop9_in = zll_pure_dispatch12_in[20];
  assign lit_inR1 = zll_main_loop9_in[0];
  assign zll_pure_dispatch4_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch4  instR8 (zll_pure_dispatch4_inR1[20], zll_pure_dispatch4_outR1);
  assign zll_pure_dispatch9_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch9  instR9 (zll_pure_dispatch9_in[20], zll_pure_dispatch9_out);
  assign zll_pure_dispatch13_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch13  instR10 (zll_pure_dispatch13_inR1[20], zll_pure_dispatch13_outR1);
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  assign zll_main_loop17_in = zll_pure_dispatch3_in[20];
  assign lit_inR2 = zll_main_loop17_in[0];
  assign zll_pure_dispatch9_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch9  instR11 (zll_pure_dispatch9_inR1[20], zll_pure_dispatch9_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_pure_dispatch9_inR1[19:16] == 4'h1) ? zll_pure_dispatch9_outR1 : ((zll_pure_dispatch3_in[19:16] == 4'h2) ? ((lit_inR2[0] == 1'h1) ? 28'h05b0000 : 28'h0860000) : ((zll_pure_dispatch13_inR1[19:16] == 4'h3) ? zll_pure_dispatch13_outR1 : ((zll_pure_dispatch9_in[19:16] == 4'h4) ? zll_pure_dispatch9_out : ((zll_pure_dispatch4_inR1[19:16] == 4'h5) ? zll_pure_dispatch4_outR1 : ((zll_pure_dispatch12_in[19:16] == 4'h6) ? ((lit_inR1[0] == 1'h1) ? 28'h0870d08 : 28'h0d7150d) : ((zll_pure_dispatch11_in[19:16] == 4'h7) ? ((zll_main_loop1_in[0] == 1'h1) ? main_loop10_outR1 : main_loop10_out) : ((zll_pure_dispatch15_inR1[19:16] == 4'h8) ? zll_pure_dispatch15_outR1 : ((zll_pure_dispatch13_in[19:16] == 4'h9) ? zll_pure_dispatch13_out : ((zll_pure_dispatch4_in[19:16] == 4'ha) ? zll_pure_dispatch4_out : ((zll_pure_dispatch8_in[19:16] == 4'hb) ? ((lit_in[0] == 1'h1) ? 28'h0570805 : 28'h0870d08) : ((zll_pure_dispatch14_inR1[19:16] == 4'hc) ? zll_pure_dispatch14_outR1 : ((zll_pure_dispatch14_in[19:16] == 4'hd) ? zll_pure_dispatch14_out : zll_pure_dispatch15_out))))))))))));
  initial __resumption_tag <= 20'h90000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h90000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Pure_dispatch15 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop20_in;
  logic [0:0] lit_in;
  assign zll_main_loop20_in = arg0;
  assign lit_in = zll_main_loop20_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0180000 : 28'h0240000;
endmodule

module ZLL_Pure_dispatch14 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop2_in;
  logic [0:0] lit_in;
  assign zll_main_loop2_in = arg0;
  assign lit_in = zll_main_loop2_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h01d0000 : 28'h0100000;
endmodule

module ZLL_Pure_dispatch13 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop16_in;
  logic [0:0] lit_in;
  assign zll_main_loop16_in = arg0;
  assign lit_in = zll_main_loop16_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0030000 : 28'h01c0000;
endmodule

module ZLL_Pure_dispatch9 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop19_in;
  logic [0:0] lit_in;
  assign zll_main_loop19_in = arg0;
  assign lit_in = zll_main_loop19_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0210000 : 28'h0350000;
endmodule

module Main_loop10 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [27:0] res);
  assign res = {arg0, 4'h7, arg1, arg0};
endmodule

module ZLL_Pure_dispatch4 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop13_in;
  logic [0:0] lit_in;
  assign zll_main_loop13_in = arg0;
  assign lit_in = zll_main_loop13_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h03a0000 : 28'h0520000;
endmodule