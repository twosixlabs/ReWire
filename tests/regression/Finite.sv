module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [6:0] __in0,
  output logic [6:0] __out0);
  logic [7:0] gzdLLziPurezidispatch1;
  logic [6:0] gzdLLziMainzidev;
  logic [8:0] callRes;
  logic [7:0] gzdLLziPurezidispatch;
  logic [6:0] gzdLLziMainzidevR1;
  logic [8:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLziPurezidispatch1 = {__in0, __resumption_tag};
  assign gzdLLziMainzidev = gzdLLziPurezidispatch1[7:1];
  zdLLziMainzidev  zdLLziMainzidev (gzdLLziMainzidev[6:0], callRes);
  assign gzdLLziPurezidispatch = {__in0, __resumption_tag};
  assign gzdLLziMainzidevR1 = gzdLLziPurezidispatch[7:1];
  zdLLziMainzidev  zdLLziMainzidevR1 (gzdLLziMainzidevR1[6:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLziPurezidispatch[0] == 1'h1) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLziMainzidev (input logic [6:0] arg0,
  output logic [8:0] res);
  logic [6:0] resizze;
  logic [13:0] binOp;
  logic [6:0] resizzeR1;
  logic [255:0] binOpR1;
  logic [127:0] resizzeR2;
  logic [6:0] gMainzidev;
  assign resizze = arg0;
  assign binOp = {resizze[6:0], 7'h01};
  assign resizzeR1 = binOp[13:7] + binOp[6:0];
  assign binOpR1 = {128'(resizzeR1[6:0]), 128'h00000000000000000000000000000064};
  assign resizzeR2 = binOpR1[255:128] % binOpR1[127:0];
  assign gMainzidev = resizzeR2[6:0];
  assign res = {1'h1, gMainzidev[6:0], 1'h0};
endmodule