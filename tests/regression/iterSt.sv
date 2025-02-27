module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] rewire_monad_iterst1_in;
  logic [2:0] zll_rewire_monad_iterst10_in;
  logic [2:0] zll_rewire_monad_iterst36_in;
  logic [1:0] main_f1_in;
  logic [1:0] zll_main_f2_in;
  logic [1:0] zll_main_f_in;
  logic [1:0] zll_main_f4_in;
  logic [1:0] zll_main_f3_in;
  logic [1:0] zll_main_f1_in;
  logic [1:0] binop_in;
  logic [0:0] msbit_in;
  logic [2:0] zll_rewire_monad_iterst30_in;
  logic [2:0] zll_rewire_monad_iterst29_in;
  logic [4:0] zll_rewire_monad_iterst8_in;
  logic [4:0] zll_rewire_monad_iterst2_in;
  logic [2:0] zll_rewire_monad_iterst11_in;
  logic [2:0] zll_rewire_monad_iterst34_in;
  logic [2:0] zll_rewire_monad_iterst_in;
  logic [2:0] zll_rewire_monad_iterst9_in;
  logic [0:0] zll_rewire_monad_iterst19_in;
  logic [5:0] zll_rewire_monad_iterst5_in;
  logic [5:0] zll_rewire_monad_iterst35_in;
  logic [1:0] zll_rewire_monad_iterst31_in;
  logic [0:0] __continue;
  logic [1:0] __padding;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign rewire_monad_iterst1_in = {__in0, __st0};
  assign zll_rewire_monad_iterst10_in = {rewire_monad_iterst1_in[1], rewire_monad_iterst1_in[0], rewire_monad_iterst1_in[0]};
  assign zll_rewire_monad_iterst36_in = {zll_rewire_monad_iterst10_in[2], zll_rewire_monad_iterst10_in[1:0]};
  assign main_f1_in = {zll_rewire_monad_iterst36_in[2], zll_rewire_monad_iterst36_in[1]};
  assign zll_main_f2_in = {main_f1_in[1], main_f1_in[0]};
  assign zll_main_f_in = zll_main_f2_in[1:0];
  assign zll_main_f4_in = {zll_main_f_in[0], zll_main_f_in[1]};
  assign zll_main_f3_in = {zll_main_f4_in[1], zll_main_f4_in[0]};
  assign zll_main_f1_in = zll_main_f3_in[1:0];
  assign binop_in = {zll_main_f1_in[1], zll_main_f1_in[0]};
  assign msbit_in = binop_in[1] ^ binop_in[0];
  assign zll_rewire_monad_iterst30_in = {{msbit_in[0], zll_main_f_in[0]}, zll_rewire_monad_iterst36_in[0]};
  assign zll_rewire_monad_iterst29_in = zll_rewire_monad_iterst30_in[2:0];
  assign zll_rewire_monad_iterst8_in = {2'h0, zll_rewire_monad_iterst29_in[2:1], zll_rewire_monad_iterst29_in[0]};
  assign zll_rewire_monad_iterst2_in = zll_rewire_monad_iterst8_in[4:0];
  assign zll_rewire_monad_iterst11_in = {zll_rewire_monad_iterst2_in[2:1], zll_rewire_monad_iterst2_in[0]};
  assign zll_rewire_monad_iterst34_in = {zll_rewire_monad_iterst11_in[0], zll_rewire_monad_iterst11_in[2:1]};
  assign zll_rewire_monad_iterst_in = {zll_rewire_monad_iterst34_in[1], zll_rewire_monad_iterst34_in[2], zll_rewire_monad_iterst34_in[0]};
  assign zll_rewire_monad_iterst9_in = {zll_rewire_monad_iterst_in[2], zll_rewire_monad_iterst_in[0], zll_rewire_monad_iterst_in[1]};
  assign zll_rewire_monad_iterst19_in = zll_rewire_monad_iterst9_in[1];
  assign zll_rewire_monad_iterst5_in = {zll_rewire_monad_iterst9_in[2], {4'h4, zll_rewire_monad_iterst19_in[0]}};
  assign zll_rewire_monad_iterst35_in = {zll_rewire_monad_iterst5_in[5], zll_rewire_monad_iterst5_in[4:0]};
  assign zll_rewire_monad_iterst31_in = {zll_rewire_monad_iterst35_in[5], zll_rewire_monad_iterst35_in[0]};
  assign {__continue, __padding, __out0, __st0_next} = {3'h4, zll_rewire_monad_iterst31_in[1], zll_rewire_monad_iterst31_in[0]};
  initial __st0 <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule