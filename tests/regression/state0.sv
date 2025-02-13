module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [8:0] zll_pure_dispatch6_in;
  logic [5:0] zll_pure_dispatch5_in;
  logic [5:0] zll_main_dev_in;
  logic [8:0] zll_pure_dispatch4_in;
  logic [2:0] main_dev_in;
  logic [8:0] lit_in;
  logic [8:0] zll_pure_dispatch2_in;
  logic [2:0] lit_inR1;
  logic [8:0] zll_pure_dispatch1_in;
  logic [5:0] zll_pure_dispatch_in;
  logic [5:0] zll_main_dev1_in;
  logic [0:0] __continue;
  logic [5:0] __resumption_tag;
  logic [5:0] __resumption_tag_next;
  assign zll_pure_dispatch6_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch5_in = {zll_pure_dispatch6_in[8:6], zll_pure_dispatch6_in[2:0]};
  assign zll_main_dev_in = {zll_pure_dispatch5_in[2:0], zll_pure_dispatch5_in[5:3]};
  assign zll_pure_dispatch4_in = {__in0, __resumption_tag};
  assign main_dev_in = zll_pure_dispatch4_in[8:6];
  assign lit_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch2_in = {__in0, __resumption_tag};
  assign lit_inR1 = zll_pure_dispatch2_in[8:6];
  assign zll_pure_dispatch1_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch_in = {zll_pure_dispatch1_in[8:6], zll_pure_dispatch1_in[2:0]};
  assign zll_main_dev1_in = {zll_pure_dispatch_in[2:0], zll_pure_dispatch_in[5:3]};
  assign {__continue, __out0, __out1, __resumption_tag_next} = (zll_pure_dispatch1_in[5:3] == 3'h1) ? {8'h80, zll_main_dev1_in[5:3]} : ((zll_pure_dispatch2_in[5:3] == 3'h2) ? 11'h620 : ((lit_in[5:3] == 3'h3) ? 11'h410 : ((zll_pure_dispatch4_in[5:3] == 3'h4) ? {8'h81, main_dev_in[2:0]} : {2'h3, zll_main_dev_in[5:3], 6'h20})));
  initial __resumption_tag <= 6'h18;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 6'h18;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule