module top_level (input logic [6:0] __in0,
  output logic [6:0] __out0);
  logic [6:0] zll_main_dev_in;
  logic [6:0] resize_in;
  logic [13:0] binop_in;
  logic [6:0] resize_inR1;
  logic [255:0] binop_inR1;
  logic [127:0] resize_inR2;
  logic [0:0] __continue;
  assign zll_main_dev_in = __in0;
  assign resize_in = zll_main_dev_in[6:0];
  assign binop_in = {resize_in[6:0], 7'h1};
  assign resize_inR1 = binop_in[13:7] + binop_in[6:0];
  assign binop_inR1 = {128'(resize_inR1[6:0]), 128'h64};
  assign resize_inR2 = binop_inR1[255:128] % binop_inR1[127:0];
  assign {__continue, __out0} = resize_inR2[6:0];
endmodule