module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] main_loop_in;
  logic [15:0] main_compute_in;
  logic [15:0] msbit_in;
  logic [15:0] id_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  logic [15:0] msbit_inR1;
  logic [15:0] id_inR1;
  logic [1:0] rewirezupreludezuzazazuinR1;
  logic [0:0] rewirezupreludezuzaza_outR1;
  logic [16:0] zll_main_compute1_in;
  logic [15:0] id_inR2;
  logic [16:0] zll_main_compute_in;
  logic [15:0] id_inR3;
  logic [8:0] zll_main_loop2_in;
  logic [8:0] zll_main_loop_in;
  logic [0:0] __continue;
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  assign main_loop_in = __resumption_tag;
  assign main_compute_in = main_loop_in[15:0];
  assign msbit_in = main_compute_in[15:0];
  assign id_in = main_compute_in[15:0];
  assign rewirezupreludezuzazazuin = {msbit_in[15], id_in[8]};
  ReWirezuPreludezuzaza  inst (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign msbit_inR1 = main_compute_in[15:0];
  assign id_inR1 = main_compute_in[15:0];
  assign rewirezupreludezuzazazuinR1 = {msbit_inR1[15], id_inR1[8]};
  ReWirezuPreludezuzaza  instR1 (rewirezupreludezuzazazuinR1[1], rewirezupreludezuzazazuinR1[0], rewirezupreludezuzaza_outR1);
  assign zll_main_compute1_in = {main_compute_in[15:0], rewirezupreludezuzaza_outR1};
  assign id_inR2 = zll_main_compute1_in[16:1];
  assign zll_main_compute_in = {main_compute_in[15:0], rewirezupreludezuzaza_out};
  assign id_inR3 = zll_main_compute_in[16:1];
  assign zll_main_loop2_in = {1'h0, (zll_main_compute_in[0] == 1'h1) ? id_inR3[7:0] : id_inR2[15:8]};
  assign zll_main_loop_in = zll_main_loop2_in[8:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop_in[7:0]};
  initial __resumption_tag <= 16'h0100;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h0100;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
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