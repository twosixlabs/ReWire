module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [6:0] __in0,
  output logic [4:0] __out0);
  logic [7:0] gzdLLzicase5386;
  logic [6:0] callRes;
  logic [7:0] gzdLLzicase5386R1;
  logic [6:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase5386 = {__resumption_tag, __in0};
  zdLLzicase5386  zdLLzicase5386 (gzdLLzicase5386[6:0], callRes);
  assign gzdLLzicase5386R1 = {__resumption_tag, __in0};
  zdLLzicase5386  zdLLzicase5386R1 (gzdLLzicase5386R1[6:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase5386R1[7] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase5386 (input logic [6:0] arg0,
  output logic [6:0] res);
  logic [6:0] gMainziloop;
  logic [6:0] gMainzicompute;
  logic [6:0] resizze;
  logic [6:0] resizzeR1;
  logic [255:0] binOp;
  logic [127:0] resizzeR2;
  logic [4:0] resizzeR3;
  logic [255:0] binOpR1;
  logic [255:0] binOpR2;
  logic [127:0] resizzeR4;
  logic [6:0] gzdLLzilambda5392;
  logic [6:0] gzdLLzicase5390;
  logic [4:0] gzdLLzilambda5366;
  assign gMainziloop = arg0;
  assign gMainzicompute = gMainziloop[6:0];
  assign resizze = gMainzicompute[6:0];
  assign resizzeR1 = resizze[6:0];
  assign binOp = {128'(resizzeR1[6:0]), 128'h00000000000000000000000000000014};
  assign resizzeR2 = binOp[255:128] % binOp[127:0];
  assign resizzeR3 = resizzeR2[4:0];
  assign binOpR1 = {128'(resizzeR3[4:0]), 128'h00000000000000000000000000000006};
  assign binOpR2 = {binOpR1[255:128] + binOpR1[127:0], 128'h00000000000000000000000000000014};
  assign resizzeR4 = binOpR2[255:128] % binOpR2[127:0];
  assign gzdLLzilambda5392 = {2'h0, resizzeR4[4:0]};
  assign gzdLLzicase5390 = gzdLLzilambda5392[6:0];
  assign gzdLLzilambda5366 = gzdLLzicase5390[4:0];
  assign res = {1'h1, gzdLLzilambda5366[4:0], 1'h0};
endmodule