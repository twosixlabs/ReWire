module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] main_loop1_in;
  logic [7:0] main_myrotr1_in;
  logic [15:0] binop_in;
  logic [15:0] binop_inR1;
  logic [15:0] binop_inR2;
  logic [7:0] main_myarithrotr_in;
  logic [15:0] binop_inR3;
  logic [15:0] binop_inR4;
  logic [15:0] binop_inR5;
  logic [8:0] zll_main_loop1_in;
  logic [8:0] zll_main_loop3_in;
  logic [0:0] __continue;
  logic [7:0] __resumption_tag;
  logic [7:0] __resumption_tag_next;
  assign main_loop1_in = __resumption_tag;
  assign main_myrotr1_in = main_loop1_in[7:0];
  assign binop_in = {main_myrotr1_in[7:0], 8'h5};
  assign binop_inR1 = {main_myrotr1_in[7:0], 8'h3};
  assign binop_inR2 = {binop_in[15:8] << binop_in[7:0], binop_inR1[15:8] >> binop_inR1[7:0]};
  assign main_myarithrotr_in = binop_inR2[15:8] | binop_inR2[7:0];
  assign binop_inR3 = {main_myarithrotr_in[7:0], 8'h3};
  assign binop_inR4 = {main_myarithrotr_in[7:0], 8'h5};
  assign binop_inR5 = {binop_inR3[15:8] << binop_inR3[7:0], binop_inR4[15:8] >>> binop_inR4[7:0]};
  assign zll_main_loop1_in = {1'h0, binop_inR5[15:8] | binop_inR5[7:0]};
  assign zll_main_loop3_in = zll_main_loop1_in[8:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop3_in[7:0]};
  initial __resumption_tag <= 8'hfc;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 8'hfc;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule