module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] main_loop_in;
  logic [1:0] main_stateaction_in;
  logic [2:0] zll_main_stateaction12_in;
  logic [2:0] zll_main_stateaction1_in;
  logic [1:0] binop_in;
  logic [0:0] msbit_in;
  logic [1:0] zll_main_loop6_in;
  logic [1:0] zll_main_loop4_in;
  logic [2:0] zll_main_loop3_in;
  logic [2:0] zll_main_loop2_in;
  logic [1:0] zll_main_loop_in;
  logic [0:0] __continue;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign main_loop_in = {__in0, __st0};
  assign main_stateaction_in = {main_loop_in[1], main_loop_in[0]};
  assign zll_main_stateaction12_in = {main_stateaction_in[1], main_stateaction_in[0], main_stateaction_in[0]};
  assign zll_main_stateaction1_in = {zll_main_stateaction12_in[2], zll_main_stateaction12_in[1:0]};
  assign binop_in = {zll_main_stateaction1_in[2], zll_main_stateaction1_in[1]};
  assign msbit_in = binop_in[1] ^ binop_in[0];
  assign zll_main_loop6_in = {zll_main_stateaction1_in[1], msbit_in[0]};
  assign zll_main_loop4_in = zll_main_loop6_in[1:0];
  assign zll_main_loop3_in = {1'h0, zll_main_loop4_in[1], zll_main_loop4_in[0]};
  assign zll_main_loop2_in = zll_main_loop3_in[2:0];
  assign zll_main_loop_in = {zll_main_loop2_in[1], zll_main_loop2_in[0]};
  assign {__continue, __out0, __st0_next} = {1'h1, zll_main_loop_in[1], zll_main_loop_in[0]};
  initial __st0 <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule