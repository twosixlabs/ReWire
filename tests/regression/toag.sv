module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [4:0] lit_in;
  logic [4:0] zll_pure_dispatch_in;
  logic [0:0] zll_main_go4_in;
  logic [1:0] zll_main_go10_in;
  logic [1:0] zll_main_go8_in;
  logic [4:0] zll_pure_dispatch3_in;
  logic [1:0] zll_pure_dispatch5_in;
  logic [1:0] zll_main_go9_in;
  logic [1:0] zll_main_go6_in;
  logic [0:0] zll_main_go6_out;
  logic [1:0] id_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  logic [4:0] lit_inR1;
  logic [4:0] zll_pure_dispatch6_in;
  logic [1:0] zll_pure_dispatch1_in;
  logic [1:0] zll_main_go3_in;
  logic [1:0] zll_main_go6_inR1;
  logic [0:0] zll_main_go6_outR1;
  logic [1:0] id_inR1;
  logic [1:0] rewirezupreludezuzazazuinR1;
  logic [0:0] rewirezupreludezuzaza_outR1;
  logic [0:0] __continue;
  logic [3:0] __resumption_tag;
  logic [3:0] __resumption_tag_next;
  assign lit_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch_in = {__in0, __resumption_tag};
  assign zll_main_go4_in = zll_pure_dispatch_in[4];
  assign zll_main_go10_in = {zll_main_go4_in[0], zll_main_go4_in[0]};
  assign zll_main_go8_in = {zll_main_go4_in[0], zll_main_go4_in[0]};
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch5_in = {zll_pure_dispatch3_in[4], zll_pure_dispatch3_in[0]};
  assign zll_main_go9_in = {zll_pure_dispatch5_in[0], zll_pure_dispatch5_in[1]};
  assign zll_main_go6_in = {zll_main_go9_in[1], zll_main_go9_in[0]};
  ZLL_Main_go6  inst (zll_main_go6_in[1], zll_main_go6_out);
  assign id_in = {zll_main_go9_in[1], zll_main_go9_in[0]};
  assign rewirezupreludezuzazazuin = {(id_in[0] == 1'h1) ? id_in[1] : zll_main_go6_out, zll_main_go9_in[0]};
  ReWirezuPreludezuzaza  instR1 (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign lit_inR1 = {__in0, __resumption_tag};
  assign zll_pure_dispatch6_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch1_in = {zll_pure_dispatch6_in[4], zll_pure_dispatch6_in[0]};
  assign zll_main_go3_in = {zll_pure_dispatch1_in[0], zll_pure_dispatch1_in[1]};
  assign zll_main_go6_inR1 = {zll_main_go3_in[1], zll_main_go3_in[0]};
  ZLL_Main_go6  instR2 (zll_main_go6_inR1[1], zll_main_go6_outR1);
  assign id_inR1 = {zll_main_go3_in[1], zll_main_go3_in[0]};
  assign rewirezupreludezuzazazuinR1 = {(id_inR1[0] == 1'h1) ? id_inR1[1] : zll_main_go6_outR1, zll_main_go3_in[0]};
  ReWirezuPreludezuzaza  instR3 (rewirezupreludezuzazazuinR1[1], rewirezupreludezuzazazuinR1[0], rewirezupreludezuzaza_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_pure_dispatch6_in[3:1] == 3'h1) ? {rewirezupreludezuzaza_outR1, 4'h0} : ((lit_inR1[3:1] == 3'h2) ? 5'h08 : ((zll_pure_dispatch3_in[3:1] == 3'h3) ? {rewirezupreludezuzaza_out, 4'h4} : ((zll_pure_dispatch_in[3:1] == 3'h4) ? ((zll_main_go8_in[0] == 1'h1) ? {4'h1, zll_main_go8_in[1]} : {zll_main_go10_in[1], 3'h3, zll_main_go10_in[1]}) : 5'h08)));
  initial __resumption_tag <= 4'h8;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 4'h8;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_go6 (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  assign rewirezupreludezuzazazuin = {arg0, arg0};
  ReWirezuPreludezuzaza  inst (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign res = rewirezupreludezuzaza_out;
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [1:0] lit_in;
  logic [1:0] id_in;
  assign lit_in = {arg0, arg1};
  assign id_in = {arg0, arg1};
  assign res = (id_in[1] == 1'h1) ? id_in[0] : 1'h0;
endmodule