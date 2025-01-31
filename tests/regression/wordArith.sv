module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLzicase4930;
  logic [9:0] callRes;
  logic [8:0] gzdLLzicase4930R1;
  logic [9:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase4930 = {__resumption_tag, __in0};
  zdLLzicase4930  zdLLzicase4930 (gzdLLzicase4930[7:0], callRes);
  assign gzdLLzicase4930R1 = {__resumption_tag, __in0};
  zdLLzicase4930  zdLLzicase4930R1 (gzdLLzicase4930R1[7:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase4930R1[8] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase4930 (input logic [7:0] arg0,
  output logic [9:0] res);
  logic [7:0] gMainziloop;
  logic [7:0] gMainzicompute;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [15:0] binOpR2;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [15:0] binOpR5;
  logic [15:0] binOpR6;
  logic [9:0] gzdLLzilambda4936;
  logic [9:0] gzdLLzicase4934;
  logic [7:0] gzdLLzilambda4910;
  assign gMainziloop = arg0;
  assign gMainzicompute = gMainziloop[7:0];
  assign binOp = {gMainzicompute[7:0], 8'h01};
  assign binOpR1 = {binOp[15:8] + binOp[7:0], 8'h02};
  assign binOpR2 = {gMainzicompute[7:0], 8'h02};
  assign binOpR3 = {binOpR1[15:8] ** binOpR1[7:0], binOpR2[15:8] - binOpR2[7:0]};
  assign binOpR4 = {binOpR3[15:8] * binOpR3[7:0], 8'h03};
  assign binOpR5 = {gMainzicompute[7:0], 8'h01};
  assign binOpR6 = {binOpR4[15:8] / binOpR4[7:0], binOpR5[15:8] + binOpR5[7:0]};
  assign gzdLLzilambda4936 = {2'h0, binOpR6[15:8] % binOpR6[7:0]};
  assign gzdLLzicase4934 = gzdLLzilambda4936[9:0];
  assign gzdLLzilambda4910 = gzdLLzicase4934[7:0];
  assign res = {1'h1, gzdLLzilambda4910[7:0], 1'h0};
endmodule