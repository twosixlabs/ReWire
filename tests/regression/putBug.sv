module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] gzdLLzicase4411;
  logic [3:0] callRes;
  logic [2:0] gzdLLzicase4411R1;
  logic [3:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  assign gzdLLzicase4411 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase4411  zdLLzicase4411 (gzdLLzicase4411[1], gzdLLzicase4411[0], callRes);
  assign gzdLLzicase4411R1 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase4411  zdLLzicase4411R1 (gzdLLzicase4411R1[1], gzdLLzicase4411R1[0], callResR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = (gzdLLzicase4411R1[2] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0} <= 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module zdLLzicase4411 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [1:0] lit;
  assign lit = {arg1, arg0};
  assign res = 4'ha;
endmodule