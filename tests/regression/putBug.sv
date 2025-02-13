module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] gzdLLziPurezidispatch;
  logic [3:0] callRes;
  logic [2:0] gzdLLziPurezidispatchR1;
  logic [3:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  assign gzdLLziPurezidispatch = {__in0, {__resumption_tag, __st0}};
  zdLLziPurezidispatch  zdLLziPurezidispatch (gzdLLziPurezidispatch[2], gzdLLziPurezidispatch[0], callRes);
  assign gzdLLziPurezidispatchR1 = {__in0, {__resumption_tag, __st0}};
  zdLLziPurezidispatch  zdLLziPurezidispatchR1 (gzdLLziPurezidispatchR1[2], gzdLLziPurezidispatchR1[0], callResR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = (gzdLLziPurezidispatchR1[1] == 1'h1) ? callResR1 : callRes;
  initial {__resumption_tag, __st0} <= 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module zdLLziPurezidispatch (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [1:0] lit;
  assign lit = {arg0, arg1};
  assign res = 4'ha;
endmodule