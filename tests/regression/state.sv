module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] gMainziloop;
  logic [1:0] gMainzistateAction;
  logic [2:0] gzdLLziMainzistateAction12;
  logic [2:0] gzdLLziMainzistateAction1;
  logic [1:0] binOp;
  logic [0:0] msbit;
  logic [1:0] gzdLLziMainziloop6;
  logic [1:0] gzdLLziMainziloop4;
  logic [2:0] gzdLLziMainziloop3;
  logic [2:0] gzdLLziMainziloop2;
  logic [1:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign gMainziloop = {__in0, __st0};
  assign gMainzistateAction = {gMainziloop[1], gMainziloop[0]};
  assign gzdLLziMainzistateAction12 = {gMainzistateAction[1], gMainzistateAction[0], gMainzistateAction[0]};
  assign gzdLLziMainzistateAction1 = {gzdLLziMainzistateAction12[2], gzdLLziMainzistateAction12[1:0]};
  assign binOp = {gzdLLziMainzistateAction1[2], gzdLLziMainzistateAction1[1]};
  assign msbit = binOp[1] ^ binOp[0];
  assign gzdLLziMainziloop6 = {gzdLLziMainzistateAction1[1], msbit[0]};
  assign gzdLLziMainziloop4 = gzdLLziMainziloop6[1:0];
  assign gzdLLziMainziloop3 = {1'h0, gzdLLziMainziloop4[1], gzdLLziMainziloop4[0]};
  assign gzdLLziMainziloop2 = gzdLLziMainziloop3[2:0];
  assign gzdLLziMainziloop = {gzdLLziMainziloop2[1], gzdLLziMainziloop2[0]};
  assign {__continue, __out0, __st0_next} = {1'h1, gzdLLziMainziloop[1], gzdLLziMainziloop[0]};
  initial __st0 <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule