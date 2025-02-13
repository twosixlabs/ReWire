module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] main_loop_in;
  logic [0:0] rewire_prelude_not_in;
  logic [0:0] lit_in;
  logic [1:0] zll_main_loop2_in;
  logic [1:0] zll_main_loop_in;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign main_loop_in = __resumption_tag;
  assign rewire_prelude_not_in = main_loop_in[0];
  assign lit_in = rewire_prelude_not_in[0];
  assign zll_main_loop2_in = {1'h0, (lit_in[0] == 1'h1) ? 1'h0 : 1'h1};
  assign zll_main_loop_in = zll_main_loop2_in[1:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop_in[0]};
  initial __resumption_tag <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule