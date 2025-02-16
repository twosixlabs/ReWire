module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [6:0] __in0,
  output logic [4:0] __out0);
  logic [6:0] main_loop1_in;
  logic [6:0] main_compute1_in;
  logic [6:0] resize_in;
  logic [6:0] resize_inR1;
  logic [255:0] binop_in;
  logic [127:0] resize_inR2;
  logic [4:0] resize_inR3;
  logic [255:0] binop_inR1;
  logic [255:0] binop_inR2;
  logic [127:0] resize_inR4;
  logic [5:0] zll_main_loop2_in;
  logic [5:0] zll_main_loop1_in;
  logic [0:0] __continue;
  logic [6:0] __resumption_tag;
  logic [6:0] __resumption_tag_next;
  assign main_loop1_in = __resumption_tag;
  assign main_compute1_in = main_loop1_in[6:0];
  assign resize_in = main_compute1_in[6:0];
  assign resize_inR1 = resize_in[6:0];
  assign binop_in = {128'(resize_inR1[6:0]), 128'h00000000000000000000000000000014};
  assign resize_inR2 = binop_in[255:128] % binop_in[127:0];
  assign resize_inR3 = resize_inR2[4:0];
  assign binop_inR1 = {128'(resize_inR3[4:0]), 128'h00000000000000000000000000000006};
  assign binop_inR2 = {binop_inR1[255:128] + binop_inR1[127:0], 128'h00000000000000000000000000000014};
  assign resize_inR4 = binop_inR2[255:128] % binop_inR2[127:0];
  assign zll_main_loop2_in = {1'h0, resize_inR4[4:0]};
  assign zll_main_loop1_in = zll_main_loop2_in[5:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop1_in[4:0]};
  initial __resumption_tag <= 7'h23;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 7'h23;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule