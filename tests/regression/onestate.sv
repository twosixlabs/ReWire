module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLzicase4331;
  logic [8:0] gzdLLzilambda4292;
  logic [7:0] gMainziincr;
  logic [7:0] gMainzigrunt;
  logic [15:0] gzdLLzilambda4336;
  logic [15:0] gzdLLzilambda4290;
  logic [7:0] gzdLLzicase4354;
  logic [17:0] gzdLLzilambda4351;
  logic [17:0] gzdLLzicase4349;
  logic [7:0] gzdLLzilambda4296;
  logic [15:0] gzdLLzilambda4346;
  logic [15:0] gzdLLzicase4344;
  logic [17:0] gzdLLzilambda4341;
  logic [17:0] gzdLLzicase4339;
  logic [15:0] gzdLLzilambda4294;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLzicase4331 = {__st0, __in0};
  assign gzdLLzilambda4292 = {gzdLLzicase4331[0], gzdLLzicase4331[8:1]};
  assign gMainziincr = gzdLLzilambda4292[7:0];
  assign gMainzigrunt = gMainziincr[7:0];
  assign gzdLLzilambda4336 = {gMainzigrunt[7:0], gMainzigrunt[7:0]};
  assign gzdLLzilambda4290 = gzdLLzilambda4336[15:0];
  assign gzdLLzicase4354 = gzdLLzilambda4290[15:8];
  assign gzdLLzilambda4351 = {10'h100, gzdLLzicase4354[7:0]};
  assign gzdLLzicase4349 = gzdLLzilambda4351[17:0];
  assign gzdLLzilambda4296 = gzdLLzicase4349[7:0];
  assign gzdLLzilambda4346 = {gzdLLzilambda4296[7:0], gzdLLzilambda4296[7:0]};
  assign gzdLLzicase4344 = gzdLLzilambda4346[15:0];
  assign gzdLLzilambda4341 = {2'h0, gzdLLzicase4344[15:8], gzdLLzicase4344[7:0]};
  assign gzdLLzicase4339 = gzdLLzilambda4341[17:0];
  assign gzdLLzilambda4294 = {gzdLLzicase4339[15:8], gzdLLzicase4339[7:0]};
  assign {__continue, __padding, __out0, __st0_next} = {2'h2, gzdLLzilambda4294[15:8], gzdLLzilambda4294[7:0]};
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule