module sample (
	clk,
	rst_n,
	a,
	b,
	c, d, e
);
input clk, rst_n;
input [7:0] a, b, c;
output d;
inout [1:0] e;

reg	hoge, _dff123;
wire		w1;
wire [1:0]	v2;
always@(posedge clk or negedge rst_n) hoge = 1;

endmodule
