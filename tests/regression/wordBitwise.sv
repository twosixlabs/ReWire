module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [23:0] zll_main_loop_in;
  logic [23:0] zll_main_compute1_in;
  logic [23:0] zll_main_compute3_in;
  logic [23:0] zll_main_compute_in;
  logic [15:0] binop_in;
  logic [15:0] binop_inR1;
  logic [7:0] unop_in;
  logic [7:0] unop_inR1;
  logic [15:0] binop_inR2;
  logic [15:0] binop_inR3;
  logic [15:0] binop_inR4;
  logic [8:0] zll_main_loop3_in;
  logic [8:0] zll_main_loop1_in;
  logic [0:0] __padding;
  assign zll_main_loop_in = {__in0, __in1, __in2};
  assign zll_main_compute1_in = zll_main_loop_in[23:0];
  assign zll_main_compute3_in = zll_main_compute1_in[23:0];
  assign zll_main_compute_in = {zll_main_compute3_in[15:8], zll_main_compute3_in[23:16], zll_main_compute3_in[7:0]};
  assign binop_in = {zll_main_compute_in[15:8], zll_main_compute_in[23:16]};
  assign binop_inR1 = {binop_in[15:8] & binop_in[7:0], zll_main_compute_in[7:0]};
  assign unop_in = zll_main_compute_in[15:8];
  assign unop_inR1 = zll_main_compute_in[23:16];
  assign binop_inR2 = {~unop_in[7:0], ~unop_inR1[7:0]};
  assign binop_inR3 = {binop_inR2[15:8] & binop_inR2[7:0], zll_main_compute_in[7:0]};
  assign binop_inR4 = {binop_inR1[15:8] ^ binop_inR1[7:0], binop_inR3[15:8] ~^ binop_inR3[7:0]};
  assign zll_main_loop3_in = {1'h0, binop_inR4[15:8] | binop_inR4[7:0]};
  assign zll_main_loop1_in = zll_main_loop3_in[8:0];
  assign {__padding, __out0} = {1'h1, zll_main_loop1_in[7:0]};
endmodule