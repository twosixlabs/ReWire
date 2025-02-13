module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [6:0] __in0,
  output logic [6:0] __out0);
  logic [7:0] zll_pure_dispatch1_in;
  logic [6:0] zll_main_dev_in;
  logic [8:0] zll_main_dev_out;
  logic [7:0] zll_pure_dispatch_in;
  logic [6:0] zll_main_dev_inR1;
  logic [8:0] zll_main_dev_outR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign zll_pure_dispatch1_in = {__in0, __resumption_tag};
  assign zll_main_dev_in = zll_pure_dispatch1_in[7:1];
  ZLL_Main_dev  inst (zll_main_dev_in[6:0], zll_main_dev_out);
  assign zll_pure_dispatch_in = {__in0, __resumption_tag};
  assign zll_main_dev_inR1 = zll_pure_dispatch_in[7:1];
  ZLL_Main_dev  instR1 (zll_main_dev_inR1[6:0], zll_main_dev_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_pure_dispatch_in[0] == 1'h1) ? zll_main_dev_outR1 : zll_main_dev_out;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_dev (input logic [6:0] arg0,
  output logic [8:0] res);
  logic [6:0] resize_in;
  logic [13:0] binop_in;
  logic [6:0] resize_inR1;
  logic [255:0] binop_inR1;
  logic [127:0] resize_inR2;
  logic [6:0] main_dev_in;
  assign resize_in = arg0;
  assign binop_in = {resize_in[6:0], 7'h01};
  assign resize_inR1 = binop_in[13:7] + binop_in[6:0];
  assign binop_inR1 = {128'(resize_inR1[6:0]), 128'h00000000000000000000000000000064};
  assign resize_inR2 = binop_inR1[255:128] % binop_inR1[127:0];
  assign main_dev_in = resize_inR2[6:0];
  assign res = {1'h1, main_dev_in[6:0], 1'h0};
endmodule