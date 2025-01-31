module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [128:0] gzdLLzicase8591;
  logic [129:0] callRes;
  logic [128:0] gzdLLzicase8591R1;
  logic [129:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase8591 = {__resumption_tag, {__in0, __in1}};
  zdLLzicase8591  zdLLzicase8591 (gzdLLzicase8591[127:0], callRes);
  assign gzdLLzicase8591R1 = {__resumption_tag, {__in0, __in1}};
  zdLLzicase8591  zdLLzicase8591R1 (gzdLLzicase8591R1[127:0], callResR1);
  assign {__continue, __out0, __out1, __resumption_tag_next} = (gzdLLzicase8591R1[128] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase8591 (input logic [127:0] arg0,
  output logic [129:0] res);
  logic [127:0] gMainziloop;
  logic [127:0] gMainzicompute;
  logic [127:0] gzdLLzicase8567;
  logic [63:0] id;
  logic [63:0] resizze;
  logic [255:0] binOp;
  logic [127:0] resizzeR1;
  logic [63:0] idR1;
  logic [63:0] idR2;
  logic [129:0] gzdLLzilambda8597;
  logic [129:0] gzdLLzicase8595;
  logic [127:0] gzdLLzilambda8571;
  assign gMainziloop = arg0;
  assign gMainzicompute = gMainziloop[127:0];
  assign gzdLLzicase8567 = gMainzicompute[127:0];
  assign id = gzdLLzicase8567[127:64];
  assign resizze = gzdLLzicase8567[63:0];
  assign binOp = {128'(resizze[63:0]), {8'h80{1'h0}}};
  assign resizzeR1 = binOp[255:128] >> binOp[127:0];
  assign idR1 = gzdLLzicase8567[127:64];
  assign idR2 = gzdLLzicase8567[63:0];
  assign gzdLLzilambda8597 = {2'h0, {id[55:0], resizzeR1[7:0], idR1[63:56], idR2[63:8]}};
  assign gzdLLzicase8595 = gzdLLzilambda8597[129:0];
  assign gzdLLzilambda8571 = gzdLLzicase8595[127:0];
  assign res = {1'h1, gzdLLzilambda8571[127:0], 1'h0};
endmodule