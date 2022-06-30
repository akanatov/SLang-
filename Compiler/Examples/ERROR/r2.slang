val unit Real //extend Float [Platform.realBitsCount]
//	epsilon: Real is realMinMantissa * 10 ** -realMaxOrder
	{} realMaxMantissa is 2 ** Platform.realMantissaBitsCount - 1
	{} realMinMantissa is 0.000001
	{} realMaxOrder is 2 ** Platform.realOderBitsCount - 1
end