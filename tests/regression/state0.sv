module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [8:0] gzdLLziPurezidispatch6;
  logic [5:0] gzdLLziPurezidispatch5;
  logic [5:0] gzdLLziMainzidev;
  logic [8:0] gzdLLziPurezidispatch4;
  logic [2:0] gMainzidev;
  logic [8:0] lit;
  logic [8:0] gzdLLziPurezidispatch2;
  logic [2:0] litR1;
  logic [8:0] gzdLLziPurezidispatch1;
  logic [5:0] gzdLLziPurezidispatch;
  logic [5:0] gzdLLziMainzidev1;
  logic [0:0] __continue;
  logic [5:0] __resumption_tag;
  logic [5:0] __resumption_tag_next;
  assign gzdLLziPurezidispatch6 = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch5 = {gzdLLziPurezidispatch6[8:6], gzdLLziPurezidispatch6[2:0]};
  assign gzdLLziMainzidev = {gzdLLziPurezidispatch5[2:0], gzdLLziPurezidispatch5[5:3]};
  assign gzdLLziPurezidispatch4 = {__in0, __resumption_tag};
  assign gMainzidev = gzdLLziPurezidispatch4[8:6];
  assign lit = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch2 = {__in0, __resumption_tag};
  assign litR1 = gzdLLziPurezidispatch2[8:6];
  assign gzdLLziPurezidispatch1 = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch = {gzdLLziPurezidispatch1[8:6], gzdLLziPurezidispatch1[2:0]};
  assign gzdLLziMainzidev1 = {gzdLLziPurezidispatch[2:0], gzdLLziPurezidispatch[5:3]};
  assign {__continue, __out0, __out1, __resumption_tag_next} = (gzdLLziPurezidispatch1[5:3] == 3'h1) ? {8'h80, gzdLLziMainzidev1[5:3]} : ((gzdLLziPurezidispatch2[5:3] == 3'h2) ? 11'h620 : ((lit[5:3] == 3'h3) ? 11'h410 : ((gzdLLziPurezidispatch4[5:3] == 3'h4) ? {8'h81, gMainzidev[2:0]} : {2'h3, gzdLLziMainzidev[5:3], 6'h20})));
  initial __resumption_tag <= 6'h18;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 6'h18;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule