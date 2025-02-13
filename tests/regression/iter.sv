module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] gReWireziMonadziiter1;
  logic [0:0] gReWireziPreludezinot;
  logic [0:0] lit;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gReWireziMonadziiter1 = __resumption_tag;
  assign gReWireziPreludezinot = gReWireziMonadziiter1[0];
  assign lit = gReWireziPreludezinot[0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, (lit[0] == 1'h1) ? 1'h0 : 1'h1};
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule