module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [3:0] gzdLLzicase4158;
  logic [4:0] callRes;
  logic [3:0] gzdLLzicase4158R1;
  logic [4:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign gzdLLzicase4158 = {{__resumption_tag, __st0, __st1}, __in0};
  zdLLzicase4158  zdLLzicase4158 (gzdLLzicase4158[2:1], gzdLLzicase4158[0], callRes);
  assign gzdLLzicase4158R1 = {{__resumption_tag, __st0, __st1}, __in0};
  zdLLzicase4158  zdLLzicase4158R1 (gzdLLzicase4158R1[2:1], gzdLLzicase4158R1[0], callResR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next, __st1_next} = (gzdLLzicase4158R1[3] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0, __st1} <= 3'h4;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 3'h4;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module zdLLzicase4158 (input logic [1:0] arg0,
  input logic [0:0] arg1,
  output logic [4:0] res);
  logic [2:0] gMainzirepl;
  assign gMainzirepl = {arg1, arg0};
  assign res = {3'h4, gMainzirepl[1:0]};
endmodule