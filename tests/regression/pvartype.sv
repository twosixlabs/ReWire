module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] gzdLLzicase2741;
  logic [1:0] gzdLLzilambda2712;
  logic [0:0] gMainzigo;
  logic [1:0] gzdLLzilambda2760;
  logic [1:0] gzdLLzilambda2710;
  logic [1:0] id;
  logic [1:0] idR1;
  logic [0:0] gzdLLzicase2753;
  logic [2:0] gzdLLzilambda2750;
  logic [2:0] gzdLLzicase2748;
  logic [0:0] gzdLLzilambda2714;
  logic [0:0] __continue;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign gzdLLzicase2741 = {__st0, __in0};
  assign gzdLLzilambda2712 = {gzdLLzicase2741[0], gzdLLzicase2741[1]};
  assign gMainzigo = gzdLLzilambda2712[0];
  assign gzdLLzilambda2760 = {gMainzigo[0], gMainzigo[0]};
  assign gzdLLzilambda2710 = gzdLLzilambda2760[1:0];
  assign id = {gzdLLzilambda2710[1], gzdLLzilambda2710[0]};
  assign idR1 = {gzdLLzilambda2710[1], gzdLLzilambda2710[0]};
  assign gzdLLzicase2753 = (idR1[1] == 1'h1) ? idR1[0] : id[0];
  assign gzdLLzilambda2750 = {2'h0, gzdLLzicase2753[0]};
  assign gzdLLzicase2748 = gzdLLzilambda2750[2:0];
  assign gzdLLzilambda2714 = gzdLLzicase2748[0];
  assign {__continue, __out0, __st0_next} = {2'h2, gzdLLzilambda2714[0]};
  initial __st0 <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule