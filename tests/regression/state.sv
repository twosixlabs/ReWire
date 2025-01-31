module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] gzdLLzicase5004;
  logic [3:0] callRes;
  logic [2:0] gzdLLzicase5004R1;
  logic [3:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  assign gzdLLzicase5004 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase5004  zdLLzicase5004 (gzdLLzicase5004[1], gzdLLzicase5004[0], callRes);
  assign gzdLLzicase5004R1 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase5004  zdLLzicase5004R1 (gzdLLzicase5004R1[1], gzdLLzicase5004R1[0], callResR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = (gzdLLzicase5004R1[2] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0} <= 2'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h2;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module zdLLzicase5004 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [1:0] gMainziloop;
  logic [1:0] gMainzistateAction;
  logic [2:0] gzdLLzilambda5028;
  logic [2:0] gzdLLzicase5025;
  logic [2:0] gzdLLzilambda4935;
  logic [1:0] binOp;
  logic [0:0] msbit;
  logic [1:0] gzdLLzilambda5014;
  logic [1:0] gzdLLzicase5011;
  logic [1:0] gzdLLzilambda5043;
  logic [1:0] gzdLLzicase5041;
  logic [3:0] gzdLLzilambda5038;
  logic [3:0] gzdLLzicase5036;
  logic [1:0] gzdLLzilambda4929;
  assign gMainziloop = {arg1, arg0};
  assign gMainzistateAction = {gMainziloop[1], gMainziloop[0]};
  assign gzdLLzilambda5028 = {gMainzistateAction[1], gMainzistateAction[0], gMainzistateAction[0]};
  assign gzdLLzicase5025 = {gzdLLzilambda5028[1:0], gzdLLzilambda5028[2]};
  assign gzdLLzilambda4935 = {gzdLLzicase5025[0], gzdLLzicase5025[2], gzdLLzicase5025[1]};
  assign binOp = {gzdLLzilambda4935[2], gzdLLzilambda4935[1]};
  assign msbit = binOp[1] ^ binOp[0];
  assign gzdLLzilambda5014 = {gzdLLzilambda4935[1], msbit[0]};
  assign gzdLLzicase5011 = {gzdLLzilambda5014[0], gzdLLzilambda5014[1]};
  assign gzdLLzilambda5043 = {gzdLLzicase5011[0], gzdLLzicase5011[1]};
  assign gzdLLzicase5041 = gzdLLzilambda5043[1:0];
  assign gzdLLzilambda5038 = {2'h0, gzdLLzicase5041[1], gzdLLzicase5041[0]};
  assign gzdLLzicase5036 = gzdLLzilambda5038[3:0];
  assign gzdLLzilambda4929 = {gzdLLzicase5036[1], gzdLLzicase5036[0]};
  assign res = {1'h1, gzdLLzilambda4929[1], 1'h0, gzdLLzilambda4929[0]};
endmodule