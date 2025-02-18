module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [23:0] main_loop1_in;
  logic [23:0] main_compute1_in;
  logic [23:0] zll_main_compute_in;
  logic [23:0] zll_main_compute2_in;
  logic [15:0] binop_in;
  logic [15:0] binop_inR1;
  logic [7:0] unop_in;
  logic [7:0] unop_inR1;
  logic [15:0] binop_inR2;
  logic [15:0] binop_inR3;
  logic [15:0] binop_inR4;
  logic [8:0] zll_main_loop2_in;
  logic [8:0] zll_main_loop1_in;
  logic [0:0] __continue;
  logic [23:0] __resumption_tag;
  logic [23:0] __resumption_tag_next;
  assign main_loop1_in = __resumption_tag;
  assign main_compute1_in = main_loop1_in[23:0];
  assign zll_main_compute_in = main_compute1_in[23:0];
  assign zll_main_compute2_in = {zll_main_compute_in[15:8], zll_main_compute_in[23:16], zll_main_compute_in[7:0]};
  assign binop_in = {zll_main_compute2_in[15:8], zll_main_compute2_in[23:16]};
  assign binop_inR1 = {binop_in[15:8] & binop_in[7:0], zll_main_compute2_in[7:0]};
  assign unop_in = zll_main_compute2_in[15:8];
  assign unop_inR1 = zll_main_compute2_in[23:16];
  assign binop_inR2 = {~unop_in[7:0], ~unop_inR1[7:0]};
  assign binop_inR3 = {binop_inR2[15:8] & binop_inR2[7:0], zll_main_compute2_in[7:0]};
  assign binop_inR4 = {binop_inR1[15:8] ^ binop_inR1[7:0], binop_inR3[15:8] ~^ binop_inR3[7:0]};
  assign zll_main_loop2_in = {1'h0, binop_inR4[15:8] | binop_inR4[7:0]};
  assign zll_main_loop1_in = zll_main_loop2_in[8:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop1_in[7:0]};
  initial __resumption_tag <= 24'h103;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 24'h103;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule