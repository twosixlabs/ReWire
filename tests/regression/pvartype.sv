module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] zll_main_go_in;
  logic [0:0] main_go_in;
  logic [1:0] zll_main_go13_in;
  logic [1:0] zll_main_go2_in;
  logic [1:0] id_in;
  logic [1:0] id_inR1;
  logic [0:0] zll_main_go9_in;
  logic [2:0] zll_main_go8_in;
  logic [2:0] zll_main_go7_in;
  logic [0:0] zll_main_go1_in;
  logic [0:0] __continue;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign zll_main_go_in = {__in0, __st0};
  assign main_go_in = zll_main_go_in[0];
  assign zll_main_go13_in = {main_go_in[0], main_go_in[0]};
  assign zll_main_go2_in = zll_main_go13_in[1:0];
  assign id_in = {zll_main_go2_in[0], zll_main_go2_in[1]};
  assign id_inR1 = {zll_main_go2_in[0], zll_main_go2_in[1]};
  assign zll_main_go9_in = (id_inR1[0] == 1'h1) ? id_inR1[1] : id_in[1];
  assign zll_main_go8_in = {2'h0, zll_main_go9_in[0]};
  assign zll_main_go7_in = zll_main_go8_in[2:0];
  assign zll_main_go1_in = zll_main_go7_in[0];
  assign {__continue, __out0, __st0_next} = {2'h2, zll_main_go1_in[0]};
  initial __st0 <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule