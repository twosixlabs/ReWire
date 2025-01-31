module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [24:0] gzdLLzicase5001;
  logic [9:0] callRes;
  logic [24:0] gzdLLzicase5001R1;
  logic [9:0] callResR1;
  logic [127:0] resizzeS0;
  logic [7:0] unOpS0;
  logic [7:0] unOpS0R1;
  logic [15:0] binOpS0;
  logic [15:0] binOpS0R1;
  logic [15:0] binOpS0R2;
  logic [9:0] gzdLLzilambda5011S0;
  logic [9:0] gzdLLzicase5009S0;
  logic [7:0] gzdLLzilambda4985S0;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign resizzeS0 = {8'h80{1'h0}};
  assign unOpS0 = resizzeS0[7:0];
  assign unOpS0R1 = 8'(128'h00000000000000000000000000000001);
  assign binOpS0 = {~unOpS0[7:0], ~unOpS0R1[7:0]};
  assign binOpS0R1 = {binOpS0[15:8] & binOpS0[7:0], 8'(128'h00000000000000000000000000000002)};
  assign binOpS0R2 = {8'h02, binOpS0R1[15:8] ~^ binOpS0R1[7:0]};
  assign gzdLLzilambda5011S0 = {2'h0, binOpS0R2[15:8] | binOpS0R2[7:0]};
  assign gzdLLzicase5009S0 = gzdLLzilambda5011S0[9:0];
  assign gzdLLzilambda4985S0 = gzdLLzicase5009S0[7:0];
  assign gzdLLzicase5001 = {__resumption_tag, {__in0, __in1, __in2}};
  zdLLzicase5001  zdLLzicase5001 (gzdLLzicase5001[23:0], callRes);
  assign gzdLLzicase5001R1 = {__resumption_tag, {__in0, __in1, __in2}};
  zdLLzicase5001  zdLLzicase5001R1 (gzdLLzicase5001R1[23:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase5001R1[24] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase5001 (input logic [23:0] arg0,
  output logic [9:0] res);
  logic [23:0] gMainziloop;
  logic [23:0] gMainzicompute;
  logic [23:0] gzdLLzicase4977;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [7:0] unOp;
  logic [7:0] unOpR1;
  logic [15:0] binOpR2;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [9:0] gzdLLzilambda5007;
  logic [9:0] gzdLLzicase5005;
  logic [7:0] gzdLLzilambda4981;
  assign gMainziloop = arg0;
  assign gMainzicompute = gMainziloop[23:0];
  assign gzdLLzicase4977 = gMainzicompute[23:0];
  assign binOp = {gzdLLzicase4977[23:16], gzdLLzicase4977[15:8]};
  assign binOpR1 = {binOp[15:8] & binOp[7:0], gzdLLzicase4977[7:0]};
  assign unOp = gzdLLzicase4977[23:16];
  assign unOpR1 = gzdLLzicase4977[15:8];
  assign binOpR2 = {~unOp[7:0], ~unOpR1[7:0]};
  assign binOpR3 = {binOpR2[15:8] & binOpR2[7:0], gzdLLzicase4977[7:0]};
  assign binOpR4 = {binOpR1[15:8] ^ binOpR1[7:0], binOpR3[15:8] ~^ binOpR3[7:0]};
  assign gzdLLzilambda5007 = {2'h0, binOpR4[15:8] | binOpR4[7:0]};
  assign gzdLLzicase5005 = gzdLLzilambda5007[9:0];
  assign gzdLLzilambda4981 = gzdLLzicase5005[7:0];
  assign res = {1'h1, gzdLLzilambda4981[7:0], 1'h0};
endmodule