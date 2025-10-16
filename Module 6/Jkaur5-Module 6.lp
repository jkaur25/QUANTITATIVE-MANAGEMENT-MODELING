\
\ Heart Start AED Transportation Model (combined production + shipping cost)

Minimize
 obj: 622 x_A1 + 614 x_A2 + 630 x_A3
    + 641 x_B1 + 645 x_B2 + 649 x_B3

Subject To
 \ Demands
 d1: x_A1 + x_B1 = 80
 d2: x_A2 + x_B2 = 60
 d3: x_A3 + x_B3 = 70

 \ Plant capacities
 capA: x_A1 + x_A2 + x_A3 <= 100
 capB: x_B1 + x_B2 + x_B3 <= 120

Bounds
 x_A1 >= 0
 x_A2 >= 0
 x_A3 >= 0
 x_B1 >= 0
 x_B2 >= 0
 x_B3 >= 0

End
