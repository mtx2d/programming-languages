fun is_older ((y1, m1, d1), (y2, m2, d2)) = 
    if y2 > y1 then true 
    else if m2 > m1 then true 
    else if d2 > d1 then true 
    else false