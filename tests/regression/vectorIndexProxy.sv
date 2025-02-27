module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [1023:0] __in0,
  input logic [31:0] __in1,
  output logic [63:0] __out0);
  logic [1055:0] main_loop_in;
  logic [1055:0] zll_main_loop2_in;
  logic [1055:0] main_compute_in;
  logic [1023:0] id_in;
  logic [511:0] id_inR1;
  logic [95:0] zll_main_compute_in;
  logic [7:0] zll_main_compute_out;
  logic [1023:0] id_inR2;
  logic [511:0] id_inR3;
  logic [95:0] zll_main_compute_inR1;
  logic [7:0] zll_main_compute_outR1;
  logic [1023:0] id_inR4;
  logic [511:0] id_inR5;
  logic [95:0] zll_main_compute_inR2;
  logic [7:0] zll_main_compute_outR2;
  logic [1023:0] id_inR6;
  logic [511:0] id_inR7;
  logic [95:0] zll_main_compute_inR3;
  logic [7:0] zll_main_compute_outR3;
  logic [1023:0] id_inR8;
  logic [511:0] id_inR9;
  logic [95:0] zll_main_compute_inR4;
  logic [7:0] zll_main_compute_outR4;
  logic [1023:0] id_inR10;
  logic [511:0] id_inR11;
  logic [95:0] zll_main_compute_inR5;
  logic [7:0] zll_main_compute_outR5;
  logic [1023:0] id_inR12;
  logic [511:0] id_inR13;
  logic [95:0] zll_main_compute_inR6;
  logic [7:0] zll_main_compute_outR6;
  logic [1023:0] id_inR14;
  logic [511:0] id_inR15;
  logic [95:0] zll_main_compute_inR7;
  logic [7:0] zll_main_compute_outR7;
  logic [64:0] zll_main_loop4_in;
  logic [64:0] zll_main_loop_in;
  logic [0:0] __continue;
  logic [1055:0] __resumption_tag;
  logic [1055:0] __resumption_tag_next;
  assign main_loop_in = __resumption_tag;
  assign zll_main_loop2_in = main_loop_in[1055:0];
  assign main_compute_in = {zll_main_loop2_in[1055:32], zll_main_loop2_in[31:0]};
  assign id_in = main_compute_in[1055:32];
  assign id_inR1 = id_in[1023:512];
  assign zll_main_compute_in = {main_compute_in[31:0], id_inR1[511:448]};
  ZLL_Main_compute  inst (zll_main_compute_in[95:64], zll_main_compute_in[63:0], zll_main_compute_out);
  assign id_inR2 = main_compute_in[1055:32];
  assign id_inR3 = id_inR2[1023:512];
  assign zll_main_compute_inR1 = {main_compute_in[31:0], id_inR3[447:384]};
  ZLL_Main_compute  instR1 (zll_main_compute_inR1[95:64], zll_main_compute_inR1[63:0], zll_main_compute_outR1);
  assign id_inR4 = main_compute_in[1055:32];
  assign id_inR5 = id_inR4[1023:512];
  assign zll_main_compute_inR2 = {main_compute_in[31:0], id_inR5[383:320]};
  ZLL_Main_compute  instR2 (zll_main_compute_inR2[95:64], zll_main_compute_inR2[63:0], zll_main_compute_outR2);
  assign id_inR6 = main_compute_in[1055:32];
  assign id_inR7 = id_inR6[1023:512];
  assign zll_main_compute_inR3 = {main_compute_in[31:0], id_inR7[319:256]};
  ZLL_Main_compute  instR3 (zll_main_compute_inR3[95:64], zll_main_compute_inR3[63:0], zll_main_compute_outR3);
  assign id_inR8 = main_compute_in[1055:32];
  assign id_inR9 = id_inR8[1023:512];
  assign zll_main_compute_inR4 = {main_compute_in[31:0], id_inR9[255:192]};
  ZLL_Main_compute  instR4 (zll_main_compute_inR4[95:64], zll_main_compute_inR4[63:0], zll_main_compute_outR4);
  assign id_inR10 = main_compute_in[1055:32];
  assign id_inR11 = id_inR10[1023:512];
  assign zll_main_compute_inR5 = {main_compute_in[31:0], id_inR11[191:128]};
  ZLL_Main_compute  instR5 (zll_main_compute_inR5[95:64], zll_main_compute_inR5[63:0], zll_main_compute_outR5);
  assign id_inR12 = main_compute_in[1055:32];
  assign id_inR13 = id_inR12[1023:512];
  assign zll_main_compute_inR6 = {main_compute_in[31:0], id_inR13[127:64]};
  ZLL_Main_compute  instR6 (zll_main_compute_inR6[95:64], zll_main_compute_inR6[63:0], zll_main_compute_outR6);
  assign id_inR14 = main_compute_in[1055:32];
  assign id_inR15 = id_inR14[1023:512];
  assign zll_main_compute_inR7 = {main_compute_in[31:0], id_inR15[63:0]};
  ZLL_Main_compute  instR7 (zll_main_compute_inR7[95:64], zll_main_compute_inR7[63:0], zll_main_compute_outR7);
  assign zll_main_loop4_in = {1'h0, {zll_main_compute_out, zll_main_compute_outR1, zll_main_compute_outR2, zll_main_compute_outR3, zll_main_compute_outR4, zll_main_compute_outR5, zll_main_compute_outR6, zll_main_compute_outR7}};
  assign zll_main_loop_in = zll_main_loop4_in[64:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop_in[63:0]};
  initial __resumption_tag <= {992'h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001, {7'h40{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {992'h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001, {7'h40{1'h0}}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_compute (input logic [31:0] arg0,
  input logic [63:0] arg1,
  output logic [7:0] res);
  logic [31:0] id_in;
  logic [63:0] id_inR1;
  logic [15:0] binop_in;
  assign id_in = arg0;
  assign id_inR1 = arg1;
  assign binop_in = {id_in[7:0], id_inR1[39:32]};
  assign res = binop_in[15:8] + binop_in[7:0];
endmodule