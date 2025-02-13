module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [7:0] gMainziloop;
  logic [7:0] resizze;
  logic [31:0] binOp;
  logic [16:0] gzdLLziMainziloop2;
  logic [16:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [7:0] __resumption_tag;
  logic [7:0] __resumption_tag_next;
  assign gMainziloop = __resumption_tag;
  assign resizze = gMainziloop[7:0];
  assign binOp = {16'(resizze[7:0]), 16'h0001};
  assign gzdLLziMainziloop2 = {1'h0, binOp[31:16] ^ binOp[15:0]};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[16:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, gzdLLziMainziloop[15:0]};
  initial __resumption_tag <= 8'hfe;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 8'hfe;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule