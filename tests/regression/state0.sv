module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [7:0] main_dev1_in;
  logic [2:0] zll_main_dev3_in;
  logic [7:0] zll_pure_dispatch_in;
  logic [5:0] zll_pure_dispatch1_in;
  logic [5:0] zll_main_dev_in;
  logic [7:0] zll_pure_dispatch3_in;
  logic [5:0] zll_pure_dispatch4_in;
  logic [5:0] zll_main_dev2_in;
  logic [0:0] __continue;
  logic [4:0] __resumption_tag;
  logic [4:0] __resumption_tag_next;
  assign main_dev1_in = {__in0, __resumption_tag};
  assign zll_main_dev3_in = main_dev1_in[7:5];
  assign zll_pure_dispatch_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch1_in = {zll_pure_dispatch_in[7:5], zll_pure_dispatch_in[2:0]};
  assign zll_main_dev_in = {zll_pure_dispatch1_in[2:0], zll_pure_dispatch1_in[5:3]};
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch4_in = {zll_pure_dispatch3_in[7:5], zll_pure_dispatch3_in[2:0]};
  assign zll_main_dev2_in = {zll_pure_dispatch4_in[2:0], zll_pure_dispatch4_in[5:3]};
  assign {__continue, __out0, __out1, __resumption_tag_next} = (zll_pure_dispatch3_in[4:3] == 2'h1) ? {1'h1, zll_main_dev2_in[5:3], 5'h0} : ((zll_pure_dispatch_in[4:3] == 2'h2) ? {6'h1, zll_main_dev_in[5:3]} : {6'h2, zll_main_dev3_in[2:0]});
  initial __resumption_tag <= 5'h10;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 5'h10;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule