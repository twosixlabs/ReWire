module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [4:0] zll_pure_dispatch5_in;
  logic [1:0] zll_pure_dispatch4_in;
  logic [1:0] zll_main_loop_in;
  logic [0:0] rewirezupreludezuzaza2zuin;
  logic [1:0] lit_in;
  logic [1:0] id_in;
  logic [0:0] main_loop_in;
  logic [4:0] zll_pure_dispatch3_in;
  logic [0:0] lit_inR1;
  logic [4:0] zll_pure_dispatch2_in;
  logic [0:0] lit_inR2;
  logic [4:0] zll_pure_dispatch1_in;
  logic [0:0] lit_inR3;
  logic [4:0] lit_inR4;
  logic [0:0] __continue;
  logic [3:0] __resumption_tag;
  logic [3:0] __resumption_tag_next;
  assign zll_pure_dispatch5_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch4_in = {zll_pure_dispatch5_in[4], zll_pure_dispatch5_in[0]};
  assign zll_main_loop_in = {zll_pure_dispatch4_in[0], zll_pure_dispatch4_in[1]};
  assign rewirezupreludezuzaza2zuin = zll_main_loop_in[1];
  assign lit_in = {rewirezupreludezuzaza2zuin[0], 1'h1};
  assign id_in = {rewirezupreludezuzaza2zuin[0], 1'h1};
  assign main_loop_in = (id_in[1] == 1'h1) ? id_in[0] : 1'h0;
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  assign lit_inR1 = zll_pure_dispatch3_in[4];
  assign zll_pure_dispatch2_in = {__in0, __resumption_tag};
  assign lit_inR2 = zll_pure_dispatch2_in[4];
  assign zll_pure_dispatch1_in = {__in0, __resumption_tag};
  assign lit_inR3 = zll_pure_dispatch1_in[4];
  assign lit_inR4 = {__in0, __resumption_tag};
  assign {__continue, __out0, __resumption_tag_next} = (lit_inR4[3:1] == 3'h1) ? 6'h24 : ((zll_pure_dispatch1_in[3:1] == 3'h2) ? 6'h26 : ((zll_pure_dispatch2_in[3:1] == 3'h3) ? 6'h28 : ((zll_pure_dispatch3_in[3:1] == 3'h4) ? 6'h20 : {1'h1, main_loop_in[0], 3'h0, main_loop_in[0]})));
  initial __resumption_tag <= 4'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 4'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule