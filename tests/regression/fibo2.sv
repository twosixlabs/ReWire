module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [20:0] zll_pure_dispatch14_in;
  logic [16:0] zll_pure_dispatch10_in;
  logic [16:0] zll_pure_dispatch_in;
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
  logic [20:0] zll_pure_dispatch15_in;
  logic [0:0] zll_main_loop4_in;
  logic [0:0] lit_in;
  logic [20:0] zll_pure_dispatch4_in;
  logic [0:0] zll_main_loop9_in;
  logic [0:0] lit_inR1;
  logic [20:0] zll_pure_dispatch5_in;
  logic [0:0] zll_main_loop17_in;
  logic [0:0] lit_inR2;
  logic [20:0] zll_pure_dispatch11_in;
  logic [27:0] zll_pure_dispatch11_out;
  logic [20:0] zll_pure_dispatch11_inR1;
  logic [27:0] zll_pure_dispatch11_outR1;
  logic [20:0] zll_pure_dispatch12_in;
  logic [27:0] zll_pure_dispatch12_out;
  logic [20:0] zll_pure_dispatch3_in;
  logic [27:0] zll_pure_dispatch3_out;
  logic [20:0] zll_pure_dispatch12_inR1;
  logic [27:0] zll_pure_dispatch12_outR1;
  logic [20:0] zll_pure_dispatch3_inR1;
  logic [27:0] zll_pure_dispatch3_outR1;
  logic [20:0] zll_pure_dispatch13_in;
  logic [27:0] zll_pure_dispatch13_out;
  logic [20:0] zll_pure_dispatch13_inR1;
  logic [27:0] zll_pure_dispatch13_outR1;
  logic [20:0] zll_pure_dispatch8_in;
  logic [27:0] zll_pure_dispatch8_out;
  logic [20:0] zll_pure_dispatch8_inR1;
  logic [27:0] zll_pure_dispatch8_outR1;
  logic [0:0] __continue;
  logic [19:0] __resumption_tag;
  logic [19:0] __resumption_tag_next;
  assign zll_pure_dispatch14_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch10_in = {zll_pure_dispatch14_in[20], zll_pure_dispatch14_in[15:8], zll_pure_dispatch14_in[7:0]};
  assign zll_pure_dispatch_in = {zll_pure_dispatch10_in[15:8], zll_pure_dispatch10_in[16], zll_pure_dispatch10_in[7:0]};
  assign zll_main_loop8_in = {zll_pure_dispatch_in[16:9], zll_pure_dispatch_in[7:0], zll_pure_dispatch_in[8]};
  assign zll_main_loop18_in = {zll_main_loop8_in[16:9], zll_main_loop8_in[8:1], zll_main_loop8_in[0]};
  assign zll_main_loop12_in = {zll_main_loop18_in[16:9], zll_main_loop18_in[8:1]};
  assign binop_in = {zll_main_loop12_in[7:0], zll_main_loop12_in[15:8]};
  assign main_loop10_in = {zll_main_loop12_in[15:8], binop_in[15:8] + binop_in[7:0]};
  Main_loop10  inst (main_loop10_in[15:8], main_loop10_in[7:0], main_loop10_out);
  assign zll_main_loop1_in = {zll_main_loop8_in[16:9], zll_main_loop8_in[8:1], zll_main_loop8_in[0]};
  assign zll_main_loop3_in = {zll_main_loop1_in[16:9], zll_main_loop1_in[8:1]};
  assign main_loop10_inR1 = {zll_main_loop3_in[7:0], zll_main_loop3_in[15:8]};
  Main_loop10  instR1 (main_loop10_inR1[15:8], main_loop10_inR1[7:0], main_loop10_outR1);
  assign zll_pure_dispatch15_in = {__in0, __resumption_tag};
  assign zll_main_loop4_in = zll_pure_dispatch15_in[20];
  assign lit_in = zll_main_loop4_in[0];
  assign zll_pure_dispatch4_in = {__in0, __resumption_tag};
  assign zll_main_loop9_in = zll_pure_dispatch4_in[20];
  assign lit_inR1 = zll_main_loop9_in[0];
  assign zll_pure_dispatch5_in = {__in0, __resumption_tag};
  assign zll_main_loop17_in = zll_pure_dispatch5_in[20];
  assign lit_inR2 = zll_main_loop17_in[0];
  assign zll_pure_dispatch11_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch11  instR2 (zll_pure_dispatch11_in[20], zll_pure_dispatch11_out);
  assign zll_pure_dispatch11_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch11  instR3 (zll_pure_dispatch11_inR1[20], zll_pure_dispatch11_outR1);
  assign zll_pure_dispatch12_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch12  instR4 (zll_pure_dispatch12_in[20], zll_pure_dispatch12_out);
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch3  instR5 (zll_pure_dispatch3_in[20], zll_pure_dispatch3_out);
  assign zll_pure_dispatch12_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch12  instR6 (zll_pure_dispatch12_inR1[20], zll_pure_dispatch12_outR1);
  assign zll_pure_dispatch3_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch3  instR7 (zll_pure_dispatch3_inR1[20], zll_pure_dispatch3_outR1);
  assign zll_pure_dispatch13_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch13  instR8 (zll_pure_dispatch13_in[20], zll_pure_dispatch13_out);
  assign zll_pure_dispatch13_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch13  instR9 (zll_pure_dispatch13_inR1[20], zll_pure_dispatch13_outR1);
  assign zll_pure_dispatch8_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch8  instR10 (zll_pure_dispatch8_in[20], zll_pure_dispatch8_out);
  assign zll_pure_dispatch8_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch8  instR11 (zll_pure_dispatch8_inR1[20], zll_pure_dispatch8_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_pure_dispatch8_inR1[19:16] == 4'h1) ? zll_pure_dispatch8_outR1 : ((zll_pure_dispatch8_in[19:16] == 4'h2) ? zll_pure_dispatch8_out : ((zll_pure_dispatch13_inR1[19:16] == 4'h3) ? zll_pure_dispatch13_outR1 : ((zll_pure_dispatch13_in[19:16] == 4'h4) ? zll_pure_dispatch13_out : ((zll_pure_dispatch3_inR1[19:16] == 4'h5) ? zll_pure_dispatch3_outR1 : ((zll_pure_dispatch12_inR1[19:16] == 4'h6) ? zll_pure_dispatch12_outR1 : ((zll_pure_dispatch3_in[19:16] == 4'h7) ? zll_pure_dispatch3_out : ((zll_pure_dispatch12_in[19:16] == 4'h8) ? zll_pure_dispatch12_out : ((zll_pure_dispatch11_inR1[19:16] == 4'h9) ? zll_pure_dispatch11_outR1 : ((zll_pure_dispatch11_in[19:16] == 4'ha) ? zll_pure_dispatch11_out : ((zll_pure_dispatch5_in[19:16] == 4'hb) ? ((lit_inR2[0] == 1'h1) ? 28'h05d0000 : 28'h08c0000) : ((zll_pure_dispatch4_in[19:16] == 4'hc) ? ((lit_inR1[0] == 1'h1) ? 28'h0800d08 : 28'h0d0150d) : ((zll_pure_dispatch15_in[19:16] == 4'hd) ? ((lit_in[0] == 1'h1) ? 28'h0500805 : 28'h0800d08) : ((zll_main_loop1_in[0] == 1'h1) ? main_loop10_outR1 : main_loop10_out)))))))))))));
  initial __resumption_tag <= 20'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h10000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Pure_dispatch13 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop2_in;
  logic [0:0] lit_in;
  assign zll_main_loop2_in = arg0;
  assign lit_in = zll_main_loop2_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0140000 : 28'h0150000;
endmodule

module ZLL_Pure_dispatch12 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop19_in;
  logic [0:0] lit_in;
  assign zll_main_loop19_in = arg0;
  assign lit_in = zll_main_loop19_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0280000 : 28'h0390000;
endmodule

module ZLL_Pure_dispatch11 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop13_in;
  logic [0:0] lit_in;
  assign zll_main_loop13_in = arg0;
  assign lit_in = zll_main_loop13_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h03a0000 : 28'h05b0000;
endmodule

module ZLL_Pure_dispatch8 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop16_in;
  logic [0:0] lit_in;
  assign zll_main_loop16_in = arg0;
  assign lit_in = zll_main_loop16_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0020000 : 28'h0130000;
endmodule

module Main_loop10 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [27:0] res);
  assign res = {arg0, 4'h0, arg1, arg0};
endmodule

module ZLL_Pure_dispatch3 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop20_in;
  logic [0:0] lit_in;
  assign zll_main_loop20_in = arg0;
  assign lit_in = zll_main_loop20_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0170000 : 28'h0260000;
endmodule