module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [4:0] gzdLLziPurezidispatch5;
  logic [1:0] gzdLLziPurezidispatch4;
  logic [1:0] gzdLLziMainziloop;
  logic [0:0] gReWireziPreludezizaza2;
  logic [1:0] lit;
  logic [1:0] id;
  logic [0:0] gMainziloop;
  logic [4:0] gzdLLziPurezidispatch3;
  logic [0:0] litR1;
  logic [4:0] gzdLLziPurezidispatch2;
  logic [0:0] litR2;
  logic [4:0] gzdLLziPurezidispatch1;
  logic [0:0] litR3;
  logic [4:0] litR4;
  logic [0:0] __continue;
  logic [3:0] __resumption_tag;
  logic [3:0] __resumption_tag_next;
  assign gzdLLziPurezidispatch5 = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch4 = {gzdLLziPurezidispatch5[4], gzdLLziPurezidispatch5[0]};
  assign gzdLLziMainziloop = {gzdLLziPurezidispatch4[0], gzdLLziPurezidispatch4[1]};
  assign gReWireziPreludezizaza2 = gzdLLziMainziloop[1];
  assign lit = {gReWireziPreludezizaza2[0], 1'h1};
  assign id = {gReWireziPreludezizaza2[0], 1'h1};
  assign gMainziloop = (id[1] == 1'h1) ? id[0] : 1'h0;
  assign gzdLLziPurezidispatch3 = {__in0, __resumption_tag};
  assign litR1 = gzdLLziPurezidispatch3[4];
  assign gzdLLziPurezidispatch2 = {__in0, __resumption_tag};
  assign litR2 = gzdLLziPurezidispatch2[4];
  assign gzdLLziPurezidispatch1 = {__in0, __resumption_tag};
  assign litR3 = gzdLLziPurezidispatch1[4];
  assign litR4 = {__in0, __resumption_tag};
  assign {__continue, __out0, __resumption_tag_next} = (litR4[3:1] == 3'h1) ? 6'h24 : ((gzdLLziPurezidispatch1[3:1] == 3'h2) ? 6'h26 : ((gzdLLziPurezidispatch2[3:1] == 3'h3) ? 6'h28 : ((gzdLLziPurezidispatch3[3:1] == 3'h4) ? 6'h20 : {1'h1, gMainziloop[0], 3'h0, gMainziloop[0]})));
  initial __resumption_tag <= 4'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 4'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule