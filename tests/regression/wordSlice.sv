module top_level (input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] zll_main_loop1_in;
  logic [15:0] zll_main_compute3_in;
  logic [15:0] msbit_in;
  logic [15:0] id_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  logic [16:0] zll_main_compute4_in;
  logic [15:0] msbit_inR1;
  logic [15:0] id_inR1;
  logic [1:0] rewirezupreludezuzazazuinR1;
  logic [0:0] rewirezupreludezuzaza_outR1;
  logic [16:0] zll_main_compute2_in;
  logic [16:0] zll_main_compute5_in;
  logic [15:0] id_inR2;
  logic [16:0] zll_main_compute_in;
  logic [15:0] id_inR3;
  logic [8:0] zll_main_loop_in;
  logic [8:0] zll_main_loop2_in;
  logic [0:0] __continue;
  assign zll_main_loop1_in = __in0;
  assign zll_main_compute3_in = zll_main_loop1_in[15:0];
  assign msbit_in = zll_main_compute3_in[15:0];
  assign id_in = zll_main_compute3_in[15:0];
  assign rewirezupreludezuzazazuin = {msbit_in[15], id_in[8]};
  ReWirezuPreludezuzaza  inst (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign zll_main_compute4_in = {zll_main_compute3_in[15:0], rewirezupreludezuzaza_out};
  assign msbit_inR1 = zll_main_compute4_in[16:1];
  assign id_inR1 = zll_main_compute4_in[16:1];
  assign rewirezupreludezuzazazuinR1 = {msbit_inR1[15], id_inR1[8]};
  ReWirezuPreludezuzaza  instR1 (rewirezupreludezuzazazuinR1[1], rewirezupreludezuzazazuinR1[0], rewirezupreludezuzaza_outR1);
  assign zll_main_compute2_in = {zll_main_compute4_in[16:1], rewirezupreludezuzaza_outR1};
  assign zll_main_compute5_in = {zll_main_compute2_in[16:1], zll_main_compute2_in[0]};
  assign id_inR2 = zll_main_compute5_in[16:1];
  assign zll_main_compute_in = {zll_main_compute4_in[16:1], zll_main_compute4_in[0]};
  assign id_inR3 = zll_main_compute_in[16:1];
  assign zll_main_loop_in = {1'h0, (zll_main_compute_in[0] == 1'h1) ? id_inR3[7:0] : id_inR2[15:8]};
  assign zll_main_loop2_in = zll_main_loop_in[8:0];
  assign {__continue, __out0} = {1'h1, zll_main_loop2_in[7:0]};
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [3:0] zzllzurewirezupreludezuzaza1zuin;
  logic [1:0] zzllzurewirezupreludezuzazazuin;
  logic [1:0] lit_in;
  logic [1:0] id_in;
  assign zzllzurewirezupreludezuzaza1zuin = {arg0, arg1, arg0, arg1};
  assign zzllzurewirezupreludezuzazazuin = {zzllzurewirezupreludezuzaza1zuin[3], zzllzurewirezupreludezuzaza1zuin[2]};
  assign lit_in = zzllzurewirezupreludezuzazazuin[1:0];
  assign id_in = zzllzurewirezupreludezuzaza1zuin[1:0];
  assign res = (id_in[1] == 1'h1) ? id_in[0] : 1'h0;
endmodule