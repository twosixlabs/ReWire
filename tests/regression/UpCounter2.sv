module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] zll_main_go4_in;
  logic [8:0] zll_main_go19_in;
  logic [7:0] zll_main_go6_in;
  logic [15:0] zll_main_go24_in;
  logic [17:0] zll_main_go24_out;
  logic [17:0] zll_main_go26_in;
  logic [17:0] zll_main_go25_in;
  logic [15:0] zll_main_go3_in;
  logic [7:0] main_incw8_in;
  logic [15:0] binop_in;
  logic [7:0] zll_main_go10_in;
  logic [17:0] zll_main_go10_out;
  logic [17:0] zll_main_go9_in;
  logic [17:0] zll_main_go9_out;
  logic [8:0] zll_main_go18_in;
  logic [7:0] zll_main_go5_in;
  logic [15:0] zll_main_go24_inR1;
  logic [17:0] zll_main_go24_outR1;
  logic [17:0] zll_main_go21_in;
  logic [17:0] zll_main_go20_in;
  logic [15:0] zll_main_go1_in;
  logic [7:0] main_rolw8_in;
  logic [15:0] binop_inR1;
  logic [7:0] main_msbitw8_in;
  logic [7:0] msbit_in;
  logic [0:0] resize_in;
  logic [15:0] binop_inR2;
  logic [7:0] zll_main_go10_inR1;
  logic [17:0] zll_main_go10_outR1;
  logic [17:0] zll_main_go9_inR1;
  logic [17:0] zll_main_go9_outR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign zll_main_go4_in = {__in0, __st0};
  assign zll_main_go19_in = {zll_main_go4_in[7:0], zll_main_go4_in[8]};
  assign zll_main_go6_in = zll_main_go19_in[8:1];
  assign zll_main_go24_in = {zll_main_go6_in[7:0], zll_main_go6_in[7:0]};
  ZLL_Main_go24  inst (zll_main_go24_in[15:0], zll_main_go24_out);
  assign zll_main_go26_in = zll_main_go24_out;
  assign zll_main_go25_in = zll_main_go26_in[17:0];
  assign zll_main_go3_in = {zll_main_go25_in[15:8], zll_main_go25_in[7:0]};
  assign main_incw8_in = zll_main_go3_in[15:8];
  assign binop_in = {main_incw8_in[7:0], 8'h01};
  assign zll_main_go10_in = binop_in[15:8] + binop_in[7:0];
  ZLL_Main_go10  instR1 (zll_main_go10_in[7:0], zll_main_go10_out);
  assign zll_main_go9_in = zll_main_go10_out;
  ZLL_Main_go9  instR2 (zll_main_go9_in[17:0], zll_main_go9_out);
  assign zll_main_go18_in = {zll_main_go4_in[7:0], zll_main_go4_in[8]};
  assign zll_main_go5_in = zll_main_go18_in[8:1];
  assign zll_main_go24_inR1 = {zll_main_go5_in[7:0], zll_main_go5_in[7:0]};
  ZLL_Main_go24  instR3 (zll_main_go24_inR1[15:0], zll_main_go24_outR1);
  assign zll_main_go21_in = zll_main_go24_outR1;
  assign zll_main_go20_in = zll_main_go21_in[17:0];
  assign zll_main_go1_in = {zll_main_go20_in[15:8], zll_main_go20_in[7:0]};
  assign main_rolw8_in = zll_main_go1_in[15:8];
  assign binop_inR1 = {main_rolw8_in[7:0], 8'h01};
  assign main_msbitw8_in = main_rolw8_in[7:0];
  assign msbit_in = main_msbitw8_in[7:0];
  assign resize_in = msbit_in[7];
  assign binop_inR2 = {binop_inR1[15:8] << binop_inR1[7:0], 8'(resize_in[0])};
  assign zll_main_go10_inR1 = binop_inR2[15:8] | binop_inR2[7:0];
  ZLL_Main_go10  instR4 (zll_main_go10_inR1[7:0], zll_main_go10_outR1);
  assign zll_main_go9_inR1 = zll_main_go10_outR1;
  ZLL_Main_go9  instR5 (zll_main_go9_inR1[17:0], zll_main_go9_outR1);
  assign {__continue, __padding, __out0, __st0_next} = (zll_main_go18_in[0] == 1'h1) ? zll_main_go9_outR1 : zll_main_go9_out;
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_go9 (input logic [17:0] arg0,
  output logic [17:0] res);
  logic [17:0] zll_main_go_in;
  logic [7:0] main_go_in;
  logic [15:0] zll_main_go24_in;
  logic [17:0] zll_main_go24_out;
  logic [17:0] zll_main_go31_in;
  logic [17:0] zll_main_go30_in;
  logic [15:0] zll_main_go7_in;
  assign zll_main_go_in = arg0;
  assign main_go_in = zll_main_go_in[7:0];
  assign zll_main_go24_in = {main_go_in[7:0], main_go_in[7:0]};
  ZLL_Main_go24  inst (zll_main_go24_in[15:0], zll_main_go24_out);
  assign zll_main_go31_in = zll_main_go24_out;
  assign zll_main_go30_in = zll_main_go31_in[17:0];
  assign zll_main_go7_in = {zll_main_go30_in[15:8], zll_main_go30_in[7:0]};
  assign res = {2'h2, zll_main_go7_in[15:8], zll_main_go7_in[7:0]};
endmodule

module ZLL_Main_go10 (input logic [7:0] arg0,
  output logic [17:0] res);
  assign res = {10'h100, arg0};
endmodule

module ZLL_Main_go24 (input logic [15:0] arg0,
  output logic [17:0] res);
  logic [15:0] zll_main_go22_in;
  assign zll_main_go22_in = arg0;
  assign res = {2'h0, zll_main_go22_in[15:8], zll_main_go22_in[7:0]};
endmodule