module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLzicase4283;
  logic [8:0] gzdLLzilambda4248;
  logic [7:0] gMainziincr;
  logic [15:0] gzdLLzilambda4305;
  logic [15:0] gzdLLzicase4303;
  logic [17:0] gzdLLzilambda4300;
  logic [17:0] gzdLLzicase4298;
  logic [15:0] gzdLLzilambda4253;
  logic [7:0] gzdLLzicase4293;
  logic [25:0] gzdLLzilambda4290;
  logic [25:0] gzdLLzicase4287;
  logic [15:0] gzdLLzilambda4251;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLzicase4283 = {__st0, __in0};
  assign gzdLLzilambda4248 = {gzdLLzicase4283[0], gzdLLzicase4283[8:1]};
  assign gMainziincr = gzdLLzilambda4248[7:0];
  assign gzdLLzilambda4305 = {gMainziincr[7:0], gMainziincr[7:0]};
  assign gzdLLzicase4303 = gzdLLzilambda4305[15:0];
  assign gzdLLzilambda4300 = {2'h0, gzdLLzicase4303[15:8], gzdLLzicase4303[7:0]};
  assign gzdLLzicase4298 = gzdLLzilambda4300[17:0];
  assign gzdLLzilambda4253 = {gzdLLzicase4298[15:8], gzdLLzicase4298[7:0]};
  assign gzdLLzicase4293 = gzdLLzilambda4253[15:8];
  assign gzdLLzilambda4290 = {gzdLLzilambda4253[15:8], {10'h100, gzdLLzicase4293[7:0]}};
  assign gzdLLzicase4287 = {gzdLLzilambda4290[17:0], gzdLLzilambda4290[25:18]};
  assign gzdLLzilambda4251 = {gzdLLzicase4287[7:0], gzdLLzicase4287[15:8]};
  assign {__continue, __padding, __out0, __st0_next} = {2'h2, gzdLLzilambda4251[15:8], gzdLLzilambda4251[7:0]};
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule