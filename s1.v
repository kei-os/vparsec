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
always@(posedge clk or negedge rst_n)
	if (a)	hoge = 1; 
	else	hoge = 0;

always@(posedge clk or negedge rst_n)
	if (a)		foo <= 1;
	else if (b)	foo <= 2;
	else		foo <= 3;

endmodule
