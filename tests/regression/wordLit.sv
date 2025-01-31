module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [8:0] gzdLLzicase4701;
  logic [17:0] callRes;
  logic [8:0] gzdLLzicase4701R1;
  logic [17:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase4701 = {__resumption_tag, __in0};
  zdLLzicase4701  zdLLzicase4701 (gzdLLzicase4701[7:0], callRes);
  assign gzdLLzicase4701R1 = {__resumption_tag, __in0};
  zdLLzicase4701  zdLLzicase4701R1 (gzdLLzicase4701R1[7:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase4701R1[8] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase4701 (input logic [7:0] arg0,
  output logic [17:0] res);
  logic [7:0] gMainziloop;
  logic [7:0] resizze;
  logic [31:0] binOp;
  logic [17:0] gzdLLzilambda4707;
  logic [17:0] gzdLLzicase4705;
  logic [15:0] gzdLLzilambda4681;
  assign gMainziloop = arg0;
  assign resizze = gMainziloop[7:0];
  assign binOp = {16'(resizze[7:0]), 16'h0001};
  assign gzdLLzilambda4707 = {2'h0, binOp[31:16] ^ binOp[15:0]};
  assign gzdLLzicase4705 = gzdLLzilambda4707[17:0];
  assign gzdLLzilambda4681 = gzdLLzicase4705[15:0];
  assign res = {1'h1, gzdLLzilambda4681[15:0], 1'h0};
endmodule