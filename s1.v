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
wire [3:0]  w2, w3, w4;
time t1;
time t2, t3;
integer int1;
integer int_a, int_b, int_hogehogec;
reg [3:0] memory[0:1023];

parameter param = 10;

initial hoge = 100;

initial begin
	foo = 10;
	bar = 20;
end
assign a = 10;

always@(posedge clk or negedge rst_n) begin
	if (a)		foo <= 1 + 2;
	else if (b)	foo <= 2;
	else		foo <= 3;
end

always@(hoge) begin
	if (a) begin
		bar <= 1;
		baz <= 1;
	end else if (b) begin
		bar <= 2;
		baz <= 2;
	end else begin
		bar <= 3;
		baz <= 3;
	end
end


endmodule
