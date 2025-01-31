module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] gzdLLzicase2946;
  logic [2:0] callRes;
  logic [1:0] gzdLLzicase2946R1;
  logic [2:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase2946 = {__resumption_tag, __in0};
  zdLLzicase2946  zdLLzicase2946 (gzdLLzicase2946[0], callRes);
  assign gzdLLzicase2946R1 = {__resumption_tag, __in0};
  zdLLzicase2946  zdLLzicase2946R1 (gzdLLzicase2946R1[0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase2946R1[1] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase2946 (input logic [0:0] arg0,
  output logic [2:0] res);
  logic [0:0] gMainziloop;
  logic [0:0] gReWireziPreludezinot;
  logic [0:0] lit;
  logic [2:0] gzdLLzilambda2952;
  logic [2:0] gzdLLzicase2950;
  logic [0:0] gzdLLzilambda2924;
  assign gMainziloop = arg0;
  assign gReWireziPreludezinot = gMainziloop[0];
  assign lit = gReWireziPreludezinot[0];
  assign gzdLLzilambda2952 = {2'h0, (lit[0] == 1'h1) ? 1'h0 : 1'h1};
  assign gzdLLzicase2950 = gzdLLzilambda2952[2:0];
  assign gzdLLzilambda2924 = gzdLLzicase2950[0];
  assign res = {1'h1, gzdLLzilambda2924[0], 1'h0};
endmodule