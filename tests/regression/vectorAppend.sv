module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0);
  logic [127:0] gMainziloop;
  logic [127:0] gMainzicompute;
  logic [127:0] gzdLLziMainzicompute;
  logic [63:0] id;
  logic [31:0] reverse;
  logic [63:0] idR1;
  logic [64:0] gzdLLziMainziloop2;
  logic [64:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [127:0] __resumption_tag;
  logic [127:0] __resumption_tag_next;
  assign gMainziloop = __resumption_tag;
  assign gMainzicompute = gMainziloop[127:0];
  assign gzdLLziMainzicompute = gMainzicompute[127:0];
  assign id = gzdLLziMainzicompute[127:64];
  assign reverse = id[63:32];
  assign idR1 = gzdLLziMainzicompute[63:0];
  assign gzdLLziMainziloop2 = {1'h0, {{reverse[7:0], reverse[15:8], reverse[23:16], reverse[31:24]}, 8'h00, idR1[23:0]}};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[64:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, gzdLLziMainziloop[63:0]};
  initial __resumption_tag <= {64'h0000000000000001, {7'h40{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {64'h0000000000000001, {7'h40{1'h0}}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule