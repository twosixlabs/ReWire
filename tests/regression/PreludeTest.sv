module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] zll_pure_dispatch_in;
  logic [1:0] zll_main_loop_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [3:0] zzllzurewirezupreludezuzaza2zuin;
  logic [1:0] zzllzurewirezupreludezuzazazuin;
  logic [1:0] lit_in;
  logic [1:0] id_in;
  logic [0:0] zll_main_loop1_in;
  logic [0:0] zll_main_zookus2_in;
  logic [0:0] zll_main_zookus2_out;
  logic [0:0] zll_main_zookus2_inR1;
  logic [0:0] zll_main_zookus2_outR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign zll_pure_dispatch_in = {__in0, __resumption_tag};
  assign zll_main_loop_in = {zll_pure_dispatch_in[0], zll_pure_dispatch_in[1]};
  assign rewirezupreludezuzazazuin = {zll_main_loop_in[1], 1'h1};
  assign zzllzurewirezupreludezuzaza2zuin = {rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0]};
  assign zzllzurewirezupreludezuzazazuin = {zzllzurewirezupreludezuzaza2zuin[3], zzllzurewirezupreludezuzaza2zuin[2]};
  assign lit_in = zzllzurewirezupreludezuzazazuin[1:0];
  assign id_in = zzllzurewirezupreludezuzaza2zuin[1:0];
  assign zll_main_loop1_in = (id_in[1] == 1'h1) ? id_in[0] : 1'h0;
  assign zll_main_zookus2_in = zll_main_loop1_in[0];
  ZLL_Main_zookus2  inst (zll_main_zookus2_in[0], zll_main_zookus2_out);
  assign zll_main_zookus2_inR1 = zll_main_zookus2_out;
  ZLL_Main_zookus2  instR1 (zll_main_zookus2_inR1[0], zll_main_zookus2_outR1);
  assign {__continue, __out0, __resumption_tag_next} = {zll_main_zookus2_outR1, zll_main_loop1_in[0]};
  initial __resumption_tag <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_zookus2 (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] zll_main_zookus4_in;
  logic [1:0] zll_main_zookus1_in;
  assign zll_main_zookus4_in = {arg0, arg0};
  assign zll_main_zookus1_in = zll_main_zookus4_in[1:0];
  assign res = zll_main_zookus1_in[1];
endmodule