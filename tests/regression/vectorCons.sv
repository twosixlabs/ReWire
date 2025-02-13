module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [127:0] main_loop_in;
  logic [127:0] main_compute_in;
  logic [127:0] zll_main_compute_in;
  logic [63:0] id_in;
  logic [63:0] resize_in;
  logic [255:0] binop_in;
  logic [127:0] resize_inR1;
  logic [63:0] id_inR1;
  logic [63:0] id_inR2;
  logic [128:0] zll_main_loop2_in;
  logic [128:0] zll_main_loop_in;
  logic [0:0] __continue;
  logic [127:0] __resumption_tag;
  logic [127:0] __resumption_tag_next;
  assign main_loop_in = __resumption_tag;
  assign main_compute_in = main_loop_in[127:0];
  assign zll_main_compute_in = main_compute_in[127:0];
  assign id_in = zll_main_compute_in[127:64];
  assign resize_in = zll_main_compute_in[63:0];
  assign binop_in = {128'(resize_in[63:0]), {8'h80{1'h0}}};
  assign resize_inR1 = binop_in[255:128] >> binop_in[127:0];
  assign id_inR1 = zll_main_compute_in[127:64];
  assign id_inR2 = zll_main_compute_in[63:0];
  assign zll_main_loop2_in = {1'h0, {id_in[55:0], resize_inR1[7:0], id_inR1[63:56], id_inR2[63:8]}};
  assign zll_main_loop_in = zll_main_loop2_in[128:0];
  assign {__continue, __out0, __out1, __resumption_tag_next} = {1'h1, zll_main_loop_in[127:0]};
  initial __resumption_tag <= {8'h80{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {8'h80{1'h0}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule