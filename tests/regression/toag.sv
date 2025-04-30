module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [3:0] zll_pure_dispatch3_in;
  logic [3:0] zll_pure_dispatch3_out;
  logic [3:0] zll_pure_dispatch3_inR1;
  logic [3:0] zll_pure_dispatch3_outR1;
  logic [3:0] lit_in;
  logic [3:0] zll_pure_dispatch4_in;
  logic [0:0] zll_main_go10_in;
  logic [2:0] zll_main_go4_in;
  logic [1:0] zll_main_go7_in;
  logic [1:0] zll_main_go12_in;
  logic [0:0] __continue;
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch3  inst (zll_pure_dispatch3_in[3], zll_pure_dispatch3_in[0], zll_pure_dispatch3_out);
  assign zll_pure_dispatch3_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch3  instR1 (zll_pure_dispatch3_inR1[3], zll_pure_dispatch3_inR1[0], zll_pure_dispatch3_outR1);
  assign lit_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch4_in = {__in0, __resumption_tag};
  assign zll_main_go10_in = zll_pure_dispatch4_in[3];
  assign zll_main_go4_in = {zll_main_go10_in[0], zll_main_go10_in[0], zll_main_go10_in[0]};
  assign zll_main_go7_in = {zll_main_go4_in[2], zll_main_go4_in[1]};
  assign zll_main_go12_in = {zll_main_go10_in[0], zll_main_go10_in[0]};
  assign {__continue, __out0, __resumption_tag_next} = (zll_pure_dispatch4_in[2:1] == 2'h1) ? ((zll_main_go12_in[0] == 1'h1) ? {3'h0, zll_main_go12_in[1]} : {zll_main_go7_in[0], 2'h3, zll_main_go7_in[1]}) : ((lit_in[2:1] == 2'h2) ? 4'h2 : ((zll_pure_dispatch3_inR1[2:1] == 2'h3) ? zll_pure_dispatch3_outR1 : zll_pure_dispatch3_out));
  initial __resumption_tag <= 3'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Pure_dispatch3 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [1:0] zll_pure_dispatch5_in;
  logic [1:0] zll_main_go13_in;
  logic [2:0] zll_main_go5_in;
  logic [1:0] zll_main_go6_in;
  logic [1:0] zll_main_go_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  logic [1:0] id_in;
  logic [1:0] rewirezupreludezuzazazuinR1;
  logic [0:0] rewirezupreludezuzaza_outR1;
  assign zll_pure_dispatch5_in = {arg0, arg1};
  assign zll_main_go13_in = {zll_pure_dispatch5_in[0], zll_pure_dispatch5_in[1]};
  assign zll_main_go5_in = {zll_main_go13_in[0], zll_main_go13_in[1], zll_main_go13_in[0]};
  assign zll_main_go6_in = {zll_main_go5_in[1], zll_main_go5_in[2]};
  assign zll_main_go_in = {zll_main_go6_in[1], zll_main_go6_in[0]};
  assign rewirezupreludezuzazazuin = {zll_main_go_in[1], zll_main_go_in[1]};
  ReWirezuPreludezuzaza  inst (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign id_in = {zll_main_go5_in[1], zll_main_go5_in[0]};
  assign rewirezupreludezuzazazuinR1 = {(id_in[0] == 1'h1) ? id_in[1] : rewirezupreludezuzaza_out, zll_main_go13_in[0]};
  ReWirezuPreludezuzaza  instR1 (rewirezupreludezuzazazuinR1[1], rewirezupreludezuzazazuinR1[0], rewirezupreludezuzaza_outR1);
  assign res = {rewirezupreludezuzaza_outR1, 3'h4};
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [3:0] zzllzurewirezupreludezuzaza1zuin;
  logic [1:0] zzllzurewirezupreludezuzaza3zuin;
  logic [1:0] lit_in;
  logic [1:0] id_in;
  assign zzllzurewirezupreludezuzaza1zuin = {arg1, arg0, arg0, arg1};
  assign zzllzurewirezupreludezuzaza3zuin = {zzllzurewirezupreludezuzaza1zuin[2], zzllzurewirezupreludezuzaza1zuin[3]};
  assign lit_in = zzllzurewirezupreludezuzaza3zuin[1:0];
  assign id_in = zzllzurewirezupreludezuzaza1zuin[1:0];
  assign res = (id_in[1] == 1'h1) ? id_in[0] : 1'h0;
endmodule