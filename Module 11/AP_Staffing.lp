minimize
 obj: 775 x1 + 800 x2 + 800 x3 + 800 x4 + 800 x5 + 775 x6 + 750 x7
subject to
 Sun: x3 + x4 + x5 + x6 >= 18
 Mon: x4 + x5 + x6 + x7 >= 27
 Tue: x1 + x5 + x6 + x7 >= 22
 Wed: x1 + x2 + x6 + x7 >= 26
 Thu: x1 + x2 + x3 + x7 >= 25
 Fri: x1 + x2 + x3 + x4 >= 21
 Sat: x2 + x3 + x4 + x5 >= 19
bounds
 x1 >= 0
 x2 >= 0
 x3 >= 0
 x4 >= 0
 x5 >= 0
 x6 >= 0
 x7 >= 0
integers x1 x2 x3 x4 x5 x6 x7
end
