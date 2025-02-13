module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [127:0] gMainziloop;
  logic [127:0] gMainzicompute;
  logic [127:0] gzdLLziMainzicompute;
  logic [63:0] id;
  logic [63:0] resizze;
  logic [255:0] binOp;
  logic [127:0] resizzeR1;
  logic [63:0] idR1;
  logic [63:0] idR2;
  logic [128:0] gzdLLziMainziloop2;
  logic [128:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [127:0] __resumption_tag;
  logic [127:0] __resumption_tag_next;
  assign gMainziloop = __resumption_tag;
  assign gMainzicompute = gMainziloop[127:0];
  assign gzdLLziMainzicompute = gMainzicompute[127:0];
  assign id = gzdLLziMainzicompute[127:64];
  assign resizze = gzdLLziMainzicompute[63:0];
  assign binOp = {128'(resizze[63:0]), {8'h80{1'h0}}};
  assign resizzeR1 = binOp[255:128] >> binOp[127:0];
  assign idR1 = gzdLLziMainzicompute[127:64];
  assign idR2 = gzdLLziMainzicompute[63:0];
  assign gzdLLziMainziloop2 = {1'h0, {id[55:0], resizzeR1[7:0], idR1[63:56], idR2[63:8]}};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[128:0];
  assign {__continue, __out0, __out1, __resumption_tag_next} = {1'h1, gzdLLziMainziloop[127:0]};
  initial __resumption_tag <= {8'h80{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {8'h80{1'h0}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule