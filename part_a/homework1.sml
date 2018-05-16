

fun is_older(x: (int * int * int), y: (int * int * int)) =
    if #1 x < #1 y then true
    else if #1 x > #1 y then false
    else if #2 x < #2 y then true
    else if #2 x > #2 y then false
    else if #3 x < #3 y then true
    else false


fun number_in_month(dates: (int * int * int) list, m: int) =
    if null dates then 0
    else
        let val cur = if #2 (hd dates) = m then 1 else 0
        in 
            cur  + number_in_month(tl dates, m)
        end


fun number_in_months(dates: (int * int * int) list, ms: int list) =
    if null ms then 0
    else number_in_month(dates, hd ms) + number_in_months(dates, tl ms)


fun dates_in_month(dates: (int * int * int) list, m: int) =
    if null dates then []
    else
        if #2 (hd dates) = m
        then (hd dates) :: dates_in_month(tl dates, m)
        else dates_in_month(tl dates, m)


fun dates_in_months(dates: (int * int * int) list, ms: int list) =
    if null ms then []
    else dates_in_month(dates, hd ms) @ dates_in_months(dates, tl ms)


fun get_nth(xs: string list, n: int) =
    if n = 1 then hd xs
    else get_nth(tl xs, n - 1)


fun date_to_string((date: (int * int * int))) =
    let 
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum(max: int, numbers: int list) =
    let 
        fun helper(ms: int list, sum: int, idx: int) =
            if sum >= max
            then idx
            else helper(tl ms, sum + (hd ms), idx + 1)
    in
        helper(numbers, 0, ~1)
    end


fun what_month(day: int) =
    let
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, months) + 1
    end


fun month_range(from: int, to: int) =
    if  from > to then []
    else what_month(from) :: month_range(from + 1, to)


fun oldest(dates: (int * int * int) list) =
    if null dates then NONE
    else
        let fun helper(dates: (int * int * int) list, max: (int * int * int)) =
            if null dates then max
            else if is_older(hd dates, max) then helper(tl dates, hd dates)
            else helper(tl dates, max)
        in
            SOME(helper(tl dates, hd dates))
        end
