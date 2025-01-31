module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0);
  logic [128:0] gzdLLzicase8428;
  logic [65:0] callRes;
  logic [128:0] gzdLLzicase8428R1;
  logic [65:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase8428 = {__resumption_tag, {__in0, __in1}};
  zdLLzicase8428  zdLLzicase8428 (gzdLLzicase8428[127:0], callRes);
  assign gzdLLzicase8428R1 = {__resumption_tag, {__in0, __in1}};
  zdLLzicase8428  zdLLzicase8428R1 (gzdLLzicase8428R1[127:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase8428R1[128] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase8428 (input logic [127:0] arg0,
  output logic [65:0] res);
  logic [127:0] gMainziloop;
  logic [127:0] gMainzicompute;
  logic [127:0] gzdLLzicase8404;
  logic [63:0] id;
  logic [31:0] reverse;
  logic [63:0] idR1;
  logic [65:0] gzdLLzilambda8434;
  logic [65:0] gzdLLzicase8432;
  logic [63:0] gzdLLzilambda8408;
  assign gMainziloop = arg0;
  assign gMainzicompute = gMainziloop[127:0];
  assign gzdLLzicase8404 = gMainzicompute[127:0];
  assign id = gzdLLzicase8404[127:64];
  assign reverse = id[63:32];
  assign idR1 = gzdLLzicase8404[63:0];
  assign gzdLLzilambda8434 = {2'h0, {{reverse[7:0], reverse[15:8], reverse[23:16], reverse[31:24]}, 8'h00, idR1[23:0]}};
  assign gzdLLzicase8432 = gzdLLzilambda8434[65:0];
  assign gzdLLzilambda8408 = gzdLLzicase8432[63:0];
  assign res = {1'h1, gzdLLzilambda8408[63:0], 1'h0};
endmodule