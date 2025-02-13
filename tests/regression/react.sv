module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] gMainziloop;
  logic [0:0] gReWireziPreludezinot;
  logic [0:0] lit;
  logic [1:0] gzdLLziMainziloop2;
  logic [1:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gMainziloop = __resumption_tag;
  assign gReWireziPreludezinot = gMainziloop[0];
  assign lit = gReWireziPreludezinot[0];
  assign gzdLLziMainziloop2 = {1'h0, (lit[0] == 1'h1) ? 1'h0 : 1'h1};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[1:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, gzdLLziMainziloop[0]};
  initial __resumption_tag <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule