module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] rewire_prelude_not1_in;
  logic [0:0] lit_in;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign rewire_prelude_not1_in = __resumption_tag;
  assign lit_in = rewire_prelude_not1_in[0];
  assign {__continue, __out0, __resumption_tag_next} = (lit_in[0] == 1'h1) ? 1'h0 : 1'h1;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule