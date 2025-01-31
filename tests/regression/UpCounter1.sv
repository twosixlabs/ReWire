module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [15:0] gzdLLzilambda4249;
  logic [15:0] binOp;
  logic [7:0] gzdLLzicase4291;
  logic [24:0] gzdLLzilambda4288;
  logic [24:0] gzdLLzicase4286;
  logic [7:0] gMainzigo;
  logic [15:0] gzdLLzilambda4303;
  logic [15:0] gzdLLzicase4301;
  logic [24:0] gzdLLzilambda4298;
  logic [24:0] gzdLLzicase4296;
  logic [15:0] gzdLLzilambda4251;
  logic [0:0] __continue;
  logic [7:0] __resumption_tag;
  logic [7:0] __st0;
  logic [7:0] __resumption_tag_next;
  logic [7:0] __st0_next;
  assign gzdLLzilambda4249 = {__resumption_tag, __st0};
  assign binOp = {gzdLLzilambda4249[15:8], 8'h01};
  assign gzdLLzicase4291 = binOp[15:8] + binOp[7:0];
  assign gzdLLzilambda4288 = {17'h00100, gzdLLzicase4291[7:0]};
  assign gzdLLzicase4286 = gzdLLzilambda4288[24:0];
  assign gMainzigo = gzdLLzicase4286[7:0];
  assign gzdLLzilambda4303 = {gMainzigo[7:0], gMainzigo[7:0]};
  assign gzdLLzicase4301 = gzdLLzilambda4303[15:0];
  assign gzdLLzilambda4298 = {9'h000, gzdLLzicase4301[15:8], gzdLLzicase4301[7:0]};
  assign gzdLLzicase4296 = gzdLLzilambda4298[24:0];
  assign gzdLLzilambda4251 = {gzdLLzicase4296[15:8], gzdLLzicase4296[7:0]};
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = {1'h1, gzdLLzilambda4251[15:8], gzdLLzilambda4251[15:8], gzdLLzilambda4251[7:0]};
  initial {__resumption_tag, __st0} <= 16'h0000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 16'h0000;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule