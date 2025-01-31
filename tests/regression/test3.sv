module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLzicase4245;
  logic [8:0] gzdLLzilambda4221;
  logic [8:0] gzdLLzicase4247;
  logic [16:0] callRes;
  logic [8:0] gzdLLzicase4247R1;
  logic [16:0] callResR1;
  logic [0:0] __continue;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLzicase4245 = {__st0, __in0};
  assign gzdLLzilambda4221 = {gzdLLzicase4245[0], gzdLLzicase4245[8:1]};
  assign gzdLLzicase4247 = {gzdLLzilambda4221[8], gzdLLzilambda4221[7:0]};
  zdLLzicase4247  zdLLzicase4247 (gzdLLzicase4247[7:0], callRes);
  assign gzdLLzicase4247R1 = {gzdLLzilambda4221[8], gzdLLzilambda4221[7:0]};
  zdLLzicase4247  zdLLzicase4247R1 (gzdLLzicase4247R1[7:0], callResR1);
  assign {__continue, __out0, __st0_next} = (gzdLLzicase4247R1[8] == 1'h1) ? callResR1 : callRes;
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module zdLLzicase4247 (input logic [7:0] arg0,
  output logic [16:0] res);
  logic [7:0] gMainzigo;
  logic [15:0] gzdLLzilambda4259;
  logic [15:0] gzdLLzicase4257;
  logic [16:0] gzdLLzilambda4254;
  logic [16:0] gzdLLzicase4252;
  logic [15:0] gzdLLzilambda4223;
  assign gMainzigo = arg0;
  assign gzdLLzilambda4259 = {gMainzigo[7:0], gMainzigo[7:0]};
  assign gzdLLzicase4257 = gzdLLzilambda4259[15:0];
  assign gzdLLzilambda4254 = {1'h0, gzdLLzicase4257[15:8], gzdLLzicase4257[7:0]};
  assign gzdLLzicase4252 = gzdLLzilambda4254[16:0];
  assign gzdLLzilambda4223 = {gzdLLzicase4252[15:8], gzdLLzicase4252[7:0]};
  assign res = {1'h1, gzdLLzilambda4223[15:8], gzdLLzilambda4223[7:0]};
endmodule