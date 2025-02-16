module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [8:0] zll_pure_dispatch3_in;
  logic [5:0] zll_pure_dispatch6_in;
  logic [5:0] zll_main_dev3_in;
  logic [8:0] zll_pure_dispatch4_in;
  logic [5:0] zll_pure_dispatch5_in;
  logic [5:0] zll_main_dev_in;
  logic [8:0] lit_in;
  logic [8:0] zll_pure_dispatch1_in;
  logic [2:0] main_dev1_in;
  logic [8:0] lit_inR1;
  logic [0:0] __continue;
  logic [5:0] __resumption_tag;
  logic [5:0] __resumption_tag_next;
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch6_in = {zll_pure_dispatch3_in[8:6], zll_pure_dispatch3_in[2:0]};
  assign zll_main_dev3_in = {zll_pure_dispatch6_in[2:0], zll_pure_dispatch6_in[5:3]};
  assign zll_pure_dispatch4_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch5_in = {zll_pure_dispatch4_in[8:6], zll_pure_dispatch4_in[2:0]};
  assign zll_main_dev_in = {zll_pure_dispatch5_in[2:0], zll_pure_dispatch5_in[5:3]};
  assign lit_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch1_in = {__in0, __resumption_tag};
  assign main_dev1_in = zll_pure_dispatch1_in[8:6];
  assign lit_inR1 = {__in0, __resumption_tag};
  assign {__continue, __out0, __out1, __resumption_tag_next} = (lit_inR1[5:3] == 3'h1) ? 10'h018 : ((zll_pure_dispatch1_in[5:3] == 3'h2) ? {7'h00, main_dev1_in[2:0]} : ((lit_in[5:3] == 3'h3) ? 10'h210 : ((zll_pure_dispatch4_in[5:3] == 3'h4) ? {1'h1, zll_main_dev_in[5:3], 6'h10} : {7'h04, zll_main_dev3_in[5:3]})));
  initial __resumption_tag <= 6'h08;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 6'h08;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule