module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase4609;
  logic [17:0] callRes;
  logic [16:0] gzdLLzicase4609R1;
  logic [17:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [7:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [7:0] __st0_next;
  assign gzdLLzicase4609 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase4609  zdLLzicase4609 (gzdLLzicase4609[15:8], gzdLLzicase4609[7:0], callRes);
  assign gzdLLzicase4609R1 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase4609  zdLLzicase4609R1 (gzdLLzicase4609R1[15:8], gzdLLzicase4609R1[7:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = (gzdLLzicase4609R1[16] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0} <= 9'h163;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 9'h163;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module zdLLzicase4609 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [17:0] res);
  logic [15:0] gMainziinc;
  logic [7:0] gMainziincr;
  logic [15:0] binOp;
  assign gMainziinc = {arg1, arg0};
  assign gMainziincr = gMainziinc[15:8];
  assign binOp = {gMainziincr[7:0], 8'h01};
  assign res = {1'h1, binOp[15:8] + binOp[7:0], 1'h0, gMainziinc[7:0]};
endmodule
