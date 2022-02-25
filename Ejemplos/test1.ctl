STATES [s0, s1, s2, s3, s4, s5]
TRANSITIONS [(s0, s1), (s0, s4), (s1, s2), (s2, s4), (s3, s2), (s3, s0), (s3, s3), (s4, s3), (s4, s1), (s4, s5), (s5, s5), (s5, s0)]
VALUATIONS [(a, s1), (a, s2), (a, s3), (b, s2), (b, s4), (b, s5)] 
CTLEXP EG AF !b
CTLEXP a
VALUATIONS [(a, s4)]
CTLEXP a