open System

let POINTS = 10.
let EPS = 1e-14

let rec iter f i a b =
    if a >= b then  
        i
    else
        iter f (f i) (a + 1) b

let table_taylor builtin_f dumb_taylor_f smart_taylor_f (points: float) (a: float) (b: float) =
    printfn "+-----+-----------+--------------+---------+-------------+---------+"
    printfn "|  x  |  Builtin  | Smart Taylor | # terms | Dumb Taylor | # terms |"
    printfn "+-----+-----------+--------------+---------+-------------+---------+"
    for i = 0 to int points do
        let x = (b - a) / points * (float i) + a
        let res_smart_taylor = smart_taylor_f x
        let res_dumb_taylor = dumb_taylor_f x
        printfn "| %3.1f | %9.6f | %12.6f | %7d | %11.6f | %7d |" x (builtin_f x) (fst res_smart_taylor) (snd res_smart_taylor) (fst res_dumb_taylor) (snd res_dumb_taylor)
    printfn "+-----+-----------+--------------+---------+-------------+---------+"

let cmp x = x <= EPS


let rec my_while f cmp (tuple: int * float) sum step iters =
    if f tuple |> cmp then
        (sum, iters)
    else
        my_while f cmp ((step + (fst tuple)), (f tuple)) (sum + (f tuple)) step (iters + 1)

let rec factorial x =
    if x < 1 then
        1
    else
        x * factorial (x - 1)

let dumb_taylor x =
    let n_member (n, _) = (x ** (float n)) / float (factorial n)
    my_while n_member cmp (1, 0) 0. 2 1

let smart_taylor x =
    let n_member (n, prev) = 
        if n = 1 then
            x
        else
            prev * (x ** 2.) / float (n * (n - 1))
    my_while n_member cmp (1, 0) 0. 2 1

let fabs (x: float) =
    if x < 0 then
        -x
    else
        x

let rec _iterations F x cond next =
    if cond x then
        x
    else
        _iterations F (F x) cond next

let iterations F x =
    let cond x = fabs (F x - x) <= EPS
    let next x = F x
    _iterations F x cond next

let derivative f x =
    (f (x + EPS) - f x) / EPS

let newthon F x =
    let cond x = fabs (F x - x) <= EPS
    let next x = x - F x / derivative F x
    _iterations F x cond next

let rec dichotomy f (a: float) (b: float) =
    if b - a <= EPS then
        (a + b) / 2.
    else
        let c = (a + b) / 2.
        if f b * f c < 0. then
            dichotomy f c b
        else
            dichotomy f a c

let f1 (x: float) = x + Math.Cos(x ** 0.52 + 2.)
let F1 (x: float) = - Math.Cos(x ** 0.52 + 2.)

let f2 (x: float) = 3. * (Math.Log x) ** 2. + 6. * Math.Log x - 5.
let F2 (x: float) = Math.Exp ((5. - 3. * (Math.Log x) ** 2.) / 6.)

let f3 (x: float) = 0.6 * 3. ** x - 2.3 * x - 3.
let F3 (x: float) = Math.Log((2.3 * x + 3.) / 0.6, 3)

let table_eq f1 F1 f2 F2 f3 F3 a1 b1 a2 b2 a3 b3 = 
    printfn "+------+---------+-----------+---------+"
    printfn "| # eq |  Iters  | Dichotomy | Newthon |"
    printfn "+------+---------+-----------+---------+"
    printfn "|    1 | %7.4f | %9.4f | %7.4f |" (iterations F1 a1) (dichotomy f1 a1 b1) (newthon F1 a1)
    printfn "|    2 | %7.4f | %9.4f | %7.4f |" (iterations F2 a2) (dichotomy f2 a2 b2) (newthon F2 a2)
    printfn "|    3 | %7.4f | %9.4f | %7.4f |" (iterations F3 a3) (dichotomy f3 a3 b3) (newthon F3 a3)
    printfn "+------+---------+-----------+---------+"

table_taylor Math.Sinh dumb_taylor smart_taylor POINTS 0. 1.
printfn ""
table_eq f1 F1 f2 F2 f3 F3 0.5 1 1 3 2 3
