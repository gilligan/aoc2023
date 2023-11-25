module Day1 = struct
    let rec foo n = if n = 1 then 1 else n * foo (n - 1)
end
