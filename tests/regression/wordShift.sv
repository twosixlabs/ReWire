module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLzicase5584;
  logic [9:0] callRes;
  logic [8:0] gzdLLzicase5584R1;
  logic [9:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase5584 = {__resumption_tag, __in0};
  zdLLzicase5584  zdLLzicase5584 (gzdLLzicase5584[7:0], callRes);
  assign gzdLLzicase5584R1 = {__resumption_tag, __in0};
  zdLLzicase5584  zdLLzicase5584R1 (gzdLLzicase5584R1[7:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase5584R1[8] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase5584 (input logic [7:0] arg0,
  output logic [9:0] res);
  logic [7:0] gMainziloop;
  logic [7:0] gMainzimyRotr4804;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [15:0] binOpR2;
  logic [7:0] gMainzimyArithRotr4905;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [15:0] binOpR5;
  logic [9:0] gzdLLzilambda5590;
  logic [9:0] gzdLLzicase5588;
  logic [7:0] gzdLLzilambda5564;
  assign gMainziloop = arg0;
  assign gMainzimyRotr4804 = gMainziloop[7:0];
  assign binOp = {gMainzimyRotr4804[7:0], 8'h05};
  assign binOpR1 = {gMainzimyRotr4804[7:0], 8'h03};
  assign binOpR2 = {binOp[15:8] << binOp[7:0], binOpR1[15:8] >> binOpR1[7:0]};
  assign gMainzimyArithRotr4905 = binOpR2[15:8] | binOpR2[7:0];
  assign binOpR3 = {gMainzimyArithRotr4905[7:0], 8'h03};
  assign binOpR4 = {gMainzimyArithRotr4905[7:0], 8'h05};
  assign binOpR5 = {binOpR3[15:8] << binOpR3[7:0], binOpR4[15:8] >>> binOpR4[7:0]};
  assign gzdLLzilambda5590 = {2'h0, binOpR5[15:8] | binOpR5[7:0]};
  assign gzdLLzicase5588 = gzdLLzilambda5590[9:0];
  assign gzdLLzilambda5564 = gzdLLzicase5588[7:0];
  assign res = {1'h1, gzdLLzilambda5564[7:0], 1'h0};
endmodule