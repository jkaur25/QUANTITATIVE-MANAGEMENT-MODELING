Maximize
 obj: 420 P1L + 360 P1M + 300 P1S
    + 420 P2L + 360 P2M + 300 P2S
    + 420 P3L + 360 P3M + 300 P3S

Subject To
 c1: P1L + P1M + P1S <= 750
 c2: P2L + P2M + P2S <= 900
 c3: P3L + P3M + P3S <= 450
 c4: 20 P1L + 15 P1M + 12 P1S <= 13000
 c5: 20 P2L + 15 P2M + 12 P2S <= 12000
 c6: 20 P3L + 15 P3M + 12 P3S <= 5000
 c7: P1L + P2L + P3L <= 900
 c8: P1M + P2M + P3M <= 1200
 c9: P1S + P2S + P3S <= 750
 eq1: 900 P1L + 900 P1M + 900 P1S - 750 P2L - 750 P2M - 750 P2S = 0
 eq2: 450 P1L + 450 P1M + 450 P1S - 750 P3L - 750 P3M - 750 P3S = 0

Bounds
 P1L >= 0
 P1M >= 0
 P1S >= 0
 P2L >= 0
 P2M >= 0
 P2S >= 0
 P3L >= 0
 P3M >= 0
 P3S >= 0

End
