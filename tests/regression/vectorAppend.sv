module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0);
  logic [127:0] main_loop1_in;
  logic [127:0] main_compute1_in;
  logic [127:0] zll_main_compute1_in;
  logic [63:0] id_in;
  logic [31:0] reverse_in;
  logic [63:0] id_inR1;
  logic [64:0] zll_main_loop3_in;
  logic [64:0] zll_main_loop2_in;
  logic [0:0] __continue;
  logic [127:0] __resumption_tag;
  logic [127:0] __resumption_tag_next;
  assign main_loop1_in = __resumption_tag;
  assign main_compute1_in = main_loop1_in[127:0];
  assign zll_main_compute1_in = main_compute1_in[127:0];
  assign id_in = zll_main_compute1_in[127:64];
  assign reverse_in = id_in[63:32];
  assign id_inR1 = zll_main_compute1_in[63:0];
  assign zll_main_loop3_in = {1'h0, {{reverse_in[7:0], reverse_in[15:8], reverse_in[23:16], reverse_in[31:24]}, 8'h0, id_inR1[23:0]}};
  assign zll_main_loop2_in = zll_main_loop3_in[64:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop2_in[63:0]};
  initial __resumption_tag <= {64'h1, {7'h40{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {64'h1, {7'h40{1'h0}}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule