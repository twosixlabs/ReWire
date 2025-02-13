module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] gReWireziMonadziiterSt1;
  logic [2:0] gzdLLziReWireziMonadziiterSt42;
  logic [2:0] gzdLLziReWireziMonadziiterSt9;
  logic [1:0] gMainzif;
  logic [1:0] binOp;
  logic [0:0] msbit;
  logic [2:0] gzdLLziReWireziMonadziiterSt39;
  logic [2:0] gzdLLziReWireziMonadziiterSt30;
  logic [4:0] gzdLLziReWireziMonadziiterSt36;
  logic [4:0] gzdLLziReWireziMonadziiterSt35;
  logic [2:0] gzdLLziReWireziMonadziiterSt6;
  logic [2:0] gzdLLziReWireziMonadziiterSt20;
  logic [2:0] gzdLLziReWireziMonadziiterSt19;
  logic [2:0] gzdLLziReWireziMonadziiterSt7;
  logic [0:0] gzdLLziReWireziMonadziiterSt16;
  logic [5:0] gzdLLziReWireziMonadziiterSt24;
  logic [5:0] gzdLLziReWireziMonadziiterSt22;
  logic [1:0] gzdLLziReWireziMonadziiterSt;
  logic [0:0] __continue;
  logic [1:0] __padding;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign gReWireziMonadziiterSt1 = {__in0, __st0};
  assign gzdLLziReWireziMonadziiterSt42 = {gReWireziMonadziiterSt1[1], gReWireziMonadziiterSt1[0], gReWireziMonadziiterSt1[0]};
  assign gzdLLziReWireziMonadziiterSt9 = {gzdLLziReWireziMonadziiterSt42[2], gzdLLziReWireziMonadziiterSt42[1:0]};
  assign gMainzif = {gzdLLziReWireziMonadziiterSt9[2], gzdLLziReWireziMonadziiterSt9[1]};
  assign binOp = {gMainzif[0], gMainzif[1]};
  assign msbit = binOp[1] ^ binOp[0];
  assign gzdLLziReWireziMonadziiterSt39 = {{msbit[0], gMainzif[0]}, gzdLLziReWireziMonadziiterSt9[0]};
  assign gzdLLziReWireziMonadziiterSt30 = gzdLLziReWireziMonadziiterSt39[2:0];
  assign gzdLLziReWireziMonadziiterSt36 = {2'h0, gzdLLziReWireziMonadziiterSt30[2:1], gzdLLziReWireziMonadziiterSt30[0]};
  assign gzdLLziReWireziMonadziiterSt35 = gzdLLziReWireziMonadziiterSt36[4:0];
  assign gzdLLziReWireziMonadziiterSt6 = {gzdLLziReWireziMonadziiterSt35[2:1], gzdLLziReWireziMonadziiterSt35[0]};
  assign gzdLLziReWireziMonadziiterSt20 = {gzdLLziReWireziMonadziiterSt6[0], gzdLLziReWireziMonadziiterSt6[2:1]};
  assign gzdLLziReWireziMonadziiterSt19 = {gzdLLziReWireziMonadziiterSt20[1], gzdLLziReWireziMonadziiterSt20[2], gzdLLziReWireziMonadziiterSt20[0]};
  assign gzdLLziReWireziMonadziiterSt7 = {gzdLLziReWireziMonadziiterSt19[2], gzdLLziReWireziMonadziiterSt19[0], gzdLLziReWireziMonadziiterSt19[1]};
  assign gzdLLziReWireziMonadziiterSt16 = gzdLLziReWireziMonadziiterSt7[1];
  assign gzdLLziReWireziMonadziiterSt24 = {gzdLLziReWireziMonadziiterSt7[2], {4'h4, gzdLLziReWireziMonadziiterSt16[0]}};
  assign gzdLLziReWireziMonadziiterSt22 = {gzdLLziReWireziMonadziiterSt24[5], gzdLLziReWireziMonadziiterSt24[4:0]};
  assign gzdLLziReWireziMonadziiterSt = {gzdLLziReWireziMonadziiterSt22[5], gzdLLziReWireziMonadziiterSt22[0]};
  assign {__continue, __padding, __out0, __st0_next} = {3'h4, gzdLLziReWireziMonadziiterSt[1], gzdLLziReWireziMonadziiterSt[0]};
  initial __st0 <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule