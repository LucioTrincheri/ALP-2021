STATES [s1, s2, s3, s4, s5]
TRANSITIONS [(s1, s4), (s4, s3), (s3, s2), (s2, s1), (s2, s5), (s5, s2)]
VALUATIONS [(r, s1), (a, s2), (v, s3), (a, s4), (r, s4), (n, s5)] 
CTLEXP r -> AX v
CTLEXP AF v
CTLEXP AF a
CTLEXP AG a
CTLEXP AG AF a
CTLEXP E [n U r]
CTLEXP A [n U !n]
CTLEXP A [!n U n]