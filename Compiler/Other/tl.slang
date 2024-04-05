type Color
	const:
		red, yellow, green, off
	end
end
type TrafficLight use const Color
	TrafficLight none
	TrafficLight (:=light) none
	light: Color is off
	switchOn do
		light := red
	end
	switchNext
	require
		light /= off /// Traffic light malfunction
	do
		if light
			case red
				light := yellow
			case yellow
				light := green
			case green
				light := red
		end
	end
end
tLight: TrafficLight
tLight.switchOn
while tLight /= off do
	tLight.switchNext
end
