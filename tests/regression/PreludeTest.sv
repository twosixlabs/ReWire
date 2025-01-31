module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [4:0] gzdLLzicase3366;
  logic [0:0] lit;
  logic [4:0] gzdLLzicase3368;
  logic [0:0] litR1;
  logic [4:0] gzdLLzicase3370;
  logic [0:0] litR2;
  logic [4:0] litR3;
  logic [4:0] gzdLLzicase3375;
  logic [1:0] gzdLLzilambda3313;
  logic [0:0] gReWireziPreludezizaza2719;
  logic [1:0] litR4;
  logic [1:0] id;
  logic [0:0] gMainziloop;
  logic [0:0] __continue;
  logic [3:0] __resumption_tag;
  logic [3:0] __resumption_tag_next;
  assign gzdLLzicase3366 = {__resumption_tag, __in0};
  assign lit = gzdLLzicase3366[0];
  assign gzdLLzicase3368 = {__resumption_tag, __in0};
  assign litR1 = gzdLLzicase3368[0];
  assign gzdLLzicase3370 = {__resumption_tag, __in0};
  assign litR2 = gzdLLzicase3370[0];
  assign litR3 = {__resumption_tag, __in0};
  assign gzdLLzicase3375 = {__resumption_tag, __in0};
  assign gzdLLzilambda3313 = {gzdLLzicase3375[1], gzdLLzicase3375[0]};
  assign gReWireziPreludezizaza2719 = gzdLLzilambda3313[1];
  assign litR4 = {gReWireziPreludezizaza2719[0], 1'h1};
  assign id = {gReWireziPreludezizaza2719[0], 1'h1};
  assign gMainziloop = (id[1] == 1'h1) ? id[0] : 1'h0;
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase3375[4:2] == 3'h0) ? {1'h1, gMainziloop[0], 3'h0, gMainziloop[0]} : ((litR3[4:2] == 3'h1) ? 6'h24 : ((gzdLLzicase3370[4:2] == 3'h2) ? 6'h26 : ((gzdLLzicase3368[4:2] == 3'h3) ? 6'h28 : 6'h20)));
  initial __resumption_tag <= 4'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 4'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule