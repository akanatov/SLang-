val unit Real extend Float [Platform.RealBitsCount]
	epsilon: Real is RealMinMantissa * 10 ** -RealMaxOrder
	{} RealMaxMantissa is 2 ** Platform.RealMantissaBitsCount - 1
	{} RealMinMantissa is 0.000001 /// TBD
	{} RealMaxOrder is 2 ** Platform.RealOderBitsCount - 1
end