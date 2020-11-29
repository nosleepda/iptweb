namespace MathStatistics

open System

type RankCorrelation(ysArr: double[], xsArr: double[], alf: double) =
    
    let xs = xsArr |> List.ofSeq
    
    let ys = ysArr |> List.ofSeq
    
    let n = double xs.Length 

    let xIndexed =
        xs |> List.mapi (fun i x -> (i + 1, x)) |> dict
        
    let xRank =
        let tmp1 = xIndexed |> Seq.sortByDescending (fun (KeyValue(k,v)) -> v)
        let tmp3 = tmp1 |> Seq.map (fun (KeyValue(k,v)) -> k)
                |> Seq.mapi (fun i x -> (x, i + 1))
                |> dict
        let tmp2 = tmp3
                |> Seq.sortBy (fun (KeyValue(k,v)) -> k)
                |> Seq.map (fun (KeyValue(k,v)) -> v)
                |> List.ofSeq
        tmp2
   

    let yIndexed = ys |> List.mapi (fun i y -> (i + 1, y)) |> dict
    
    let yRank =
        let tmp1 = yIndexed |> Seq.sortByDescending (fun (KeyValue(k,v)) -> v)
        let tmp3 = tmp1 |> Seq.map (fun (KeyValue(k,v)) -> k)
                |> Seq.mapi (fun i y -> (y, i + 1))
                |> dict
        let tmp2 = tmp3
                |> Seq.sortBy (fun (KeyValue(k,v)) -> k)
                |> Seq.map (fun (KeyValue(k,v)) -> v)
                |> List.ofSeq
        tmp2
    
    let d = (xRank, yRank) ||> List.map2 (fun x y -> double (abs (x - y)) ** 2.0 ) |> List.sum
    
    let correlationSpearman = 1.0 - 6.0 * d / (n ** 3.0 - n)
    
    let R =
        let mutable tmp = 0
        let mutable yi = yRank.Tail
        
        for i in [0 .. yRank.Length - 2] do
            for y in yi do
                if yRank.[i] < y then
                    tmp <- tmp + 1            
            yi <- yi.Tail
            
        double tmp 
        
    let correlationKendall = 4.0 * R / (n * (n - 1.0)) - 1.0

    member this.CorrelationSpearman = Math.Round(correlationSpearman, 4)
    
    member this.SignificanceCorrelationSpearman =
        let l = correlationSpearman
        let r = sqrt ((1.0 - correlationSpearman ** 2.0) / (n - 2.0)) * Utilities2.Student2 (n - 2.0) alf
        let bool = if l > r then "" else "не "
        let bool2 = if l > r then ">" else "<"
        String.Format("Ранговая корреляционная связь {0}значима, \nт.к. {1} {2} {3} ", bool, l, bool2, r)
    
    member this.CorrelationKendall = Math.Round(correlationKendall, 4)
    
    member this.SignificanceCorrelationKendall =
        let l = abs correlationKendall
        let z = 1.96// f(1,96) = 0,475 = (1 - 0.05 )/ 2
        let r =  z * sqrt ((2.0 * (2.0 * n + 5.0)) / ( 9.0 * n * (n - 1.0)))
        let bool = if l > r then "" else "не "
        let bool2 = if l > r then ">" else "<"
        String.Format("Ранговая корреляционная связь {0}значима, \nт.к. {1} {2} {3} ", bool, l, bool2, r)