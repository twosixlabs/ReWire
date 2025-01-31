module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] gzdLLzicase3008;
  logic [2:0] callRes;
  logic [1:0] gzdLLzicase3008R1;
  logic [2:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase3008 = {__resumption_tag, __in0};
  zdLLzicase3008  zdLLzicase3008 (gzdLLzicase3008[0], callRes);
  assign gzdLLzicase3008R1 = {__resumption_tag, __in0};
  zdLLzicase3008  zdLLzicase3008R1 (gzdLLzicase3008R1[0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase3008R1[1] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase3008 (input logic [0:0] arg0,
  output logic [2:0] res);
  logic [0:0] gReWireziMonadziiter2881;
  logic [0:0] gReWireziPreludezinot;
  logic [0:0] lit;
  assign gReWireziMonadziiter2881 = arg0;
  assign gReWireziPreludezinot = gReWireziMonadziiter2881[0];
  assign lit = gReWireziPreludezinot[0];
  assign res = {1'h1, (lit[0] == 1'h1) ? 1'h0 : 1'h1, 1'h1};
endmodule