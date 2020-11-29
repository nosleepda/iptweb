namespace MathStatistics

open System
open System.Collections.Generic
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

type NonlinearDependencies(xsArr: double[], ysArr: double[], alf: double) =
    
    let xs = xsArr |> List.ofSeq
    
    let ys = ysArr |> List.ofSeq
    
    let n = xs.Length |> float
    
    let my = ys |> List.sum |> (*) (1.0 / n)
    
    let mx = xs |> List.sum |> (*) (1.0 / n)
    
    let sx = 
        xs
        |> List.map (fun x -> (x - mx) ** 2.0)
        |> List.sum
        |> (*) (1.0 / (n - 1.0)) 
        |> sqrt
        
    let sy =
        ys
        |> List.map (fun y -> (y - my) ** 2.0)
        |> List.sum
        |> (*) (1.0 / (n - 1.0))
        |> sqrt
    
    let x1 = sqrt (List.max xs * List.min xs)
    
    let x2 = (List.max xs + List.min xs) / 2.0
    
    let x3 = List.max xs * List.min xs * 2.0 / (List.max xs + List.min xs)
    
    let y1 = sqrt (List.max ys * List.min ys)
    
    let y2 = List.max ys * List.min ys * 2.0 / (List.max ys + List.min ys)
    
    let y3 = (List.max ys + List.min ys) / 2.0
    
    let xfs = [x1;x2;x2;x1;x3;x3]
    
    let yfs = [y1;y1;y2;y3;y3;y2]
    
    let fs = ([x1;x2;x2;x1;x3;x3], [y1;y1;y2;y3;y3;y2])
    
    let interpol=
        let lastElem (list: List<double>) = list.[list.Count - 1]
        let predLastElem (list: List<double>) = list.[list.Count - 2]
        let calc (listX: Collections.list<double>) (listY: Collections.list<double>) =
            let resultTempX = List<double>[xs.[0]]
            let resultTempY = List<double>[ys.[0]]
            (listX.Tail, listY.Tail)
            ||> List.iter2 (fun x y ->
                resultTempX.Add (lastElem resultTempX + (x - lastElem resultTempX) / 2.0)
                resultTempY.Add (lastElem resultTempY + (lastElem resultTempX - predLastElem resultTempX) / (x - predLastElem resultTempX) * (y - lastElem resultTempY))
                resultTempX.Add x;
                resultTempY.Add y)
            (List.ofSeq resultTempX, List.ofSeq resultTempY)
            
        (xs, ys) ||> calc ||> calc
    
    let numberOfFunc =
        let kv (v:int) (k:double) = (k,v)
        
        let valuesOfFunc =
            (xfs, yfs)
            ||> List.mapi2 (fun index xp yp ->
                interpol
                ||> List.map2 (fun x y -> sqrt ((x - xp) ** 2.0 + (y - yp) ** 2.0))
                |> List.min
                |> kv index)
            |> dict
            
        let minKey = valuesOfFunc.Keys
                     |> List.ofSeq
                     |> List.min
                     
        valuesOfFunc.Item minKey
    
    let calcMatrix t t2 g gt =
        let list = [|n; t; t; t2|]
        let m = DenseMatrix(2,2,list) 
        let fs = [g;gt] |> vector
        m.Solve fs |> List.ofSeq
        
    //y=a*x^b
    let regressionOne =
        let t = xs |> List.map (fun x -> log x) |> List.sum
        let t2 = xs |> List.map (fun x -> (log x) ** 2.0) |> List.sum
        let g = ys |> List.map (fun y -> log y) |> List.sum
        let gt = (xs, ys) ||> List.map2 (fun x y -> log x * log y) |> List.sum
        let r = calcMatrix t t2 g gt
        [exp r.[0]; r.[1]]
    
    let funcRegressionOne x =
        regressionOne.[0] * x ** regressionOne.[1]
    
    //y=a*b^2
    let regressionTwo =
        let t = xs |> List.sum
        let t2 = xs |> List.map (fun x -> x ** 2.0) |> List.sum
        let g = ys |> List.map (fun y -> log y) |> List.sum
        let gt = (xs, ys) ||> List.map2 (fun x y -> x ** 2.0 * log y) |> List.sum
        let r = calcMatrix t t2 g gt
        [exp r.[0]; exp r.[1]]
    
    let funcRegressionTwo x =
        regressionTwo.[0] * regressionTwo.[1] ** x
     
    //y=1/(a+b*x)
    let regressionThree =
        let t = xs |> List.sum
        let t2 = xs |> List.map (fun x -> x ** 2.0) |> List.sum
        let g = ys |> List.map (fun y -> 1.0 / y) |> List.sum
        let gt = (xs, ys) ||> List.map2 (fun x y -> x * (1.0 / y)) |> List.sum
        let r = calcMatrix t t2 g gt
        [r.[0]; r.[1]]
    
    let funcRegressionThree x =
        1.0 / (regressionThree.[0] + regressionThree.[1] * x)
    
    //y=a+b*lgx
    let regressionFour =
        let t = xs |> List.map (fun x -> log10 x) |> List.sum
        let t2 = xs |> List.map (fun x -> (log10 x) ** 2.0) |> List.sum
        let g = ys |> List.sum
        let gt = (xs, ys) ||> List.map2 (fun x y -> log10 x * y) |> List.sum
        let r = calcMatrix t t2 g gt
        [r.[0]; r.[1]]
    
    let funcRegressionFour x =
        regressionFour.[0] + regressionFour.[1] * (log10 x)
    
    //y=a+b/x
    let regressionFive =
        let t = xs |> List.map (fun x -> 1.0 / x) |> List.sum
        let t2 = xs |> List.map (fun x -> (1.0 / x) ** 2.0) |> List.sum
        let g = ys |> List.sum
        let gt = (xs, ys) ||> List.map2 (fun x y -> (1.0 / x) * y) |> List.sum
        let r = calcMatrix t t2 g gt
        [r.[0]; r.[1]]
    
    let funcRegressionFive x =
        regressionFive.[0] + regressionFive.[1] / x
    
    //y=a*x/(b+x)
    let regressionSix =
        let t = xs |> List.map (fun x -> 1.0 / x) |> List.sum
        let t2 = xs |> List.map (fun x -> (1.0 / x) ** 2.0) |> List.sum
        let g = ys |> List.map (fun y -> 1.0 / y) |> List.sum
        let gt = (xs, ys) ||> List.map2 (fun x y -> (1.0 / x) * (1.0 / y)) |> List.sum
        let r = calcMatrix t t2 g gt
        [1.0 / r.[0]; r.[1] * r.[0]]
    
    let funcRegressionSix x =
        regressionSix.[0] * x / (regressionSix.[1] + x)
    
    let coeffAB =
        match numberOfFunc with
        | 0 -> regressionOne
        | 1 -> regressionTwo
        | 2 -> regressionThree
        | 3 -> regressionFour
        | 4 -> regressionFive
        | 5 -> regressionSix
        | _ -> failwith "Illegal number of regression functions"
    
    let funcRegression =
        match numberOfFunc with
        | 0 -> funcRegressionOne
        | 1 -> funcRegressionTwo
        | 2 -> funcRegressionThree
        | 3 -> funcRegressionFour
        | 4 -> funcRegressionFive
        | 5 -> funcRegressionSix
        | _ -> failwith "Illegal number of regression functions"
    
    let fxs = xs |> List.map (fun x -> funcRegression x)
    
    let R2 =
        let yp2 = (fxs, ys) ||> List.map2 (fun p y -> (y - p) ** 2.0) |> List.sum
        let ymy2 = ys |> List.map (fun y -> (y - my) ** 2.0) |> List.sum
        1.0 - yp2 / ymy2
    
    member this.MeanX = Math.Round(mx, 4)
    
    member this.MeanY = Math.Round(my, 4)
    
    member this.Sx = Math.Round(sx, 4)
    
    member this.Sy = Math.Round(sy, 4)
    
    member this.Determination = Math.Round(R2, 4)
    
    member this.Adequacy =
        let l = R2 * (n - 2.0) / (1.0 - R2)
        let r = Utilities2.Fisher 1.0 (n - 2.0) alf
        let bool = if l > r then "" else "не "
        let bool2 = if l > r then ">" else "<"
        String.Format("Уравнение регрессии статистически {0}значимо, \nт.к. {1} {2} {3} ", bool, Math.Round(l, 4), bool2, Math.Round(r, 4))
        
    member this.RegressionFunction =
        match numberOfFunc with
        | 0 -> printfn "y = %f * x ^ %f" coeffAB.[0] coeffAB.[1]
        | 1 -> printfn "y = %f * %f ^ x" coeffAB.[0] coeffAB.[1]
        | 2 -> printfn "y = 1 / (%f + %f * x)" coeffAB.[0] coeffAB.[1]
        | 3 -> printfn "y = %f + %f * lgx" coeffAB.[0] coeffAB.[1]
        | 4 -> printfn "y = %f + %f / x" coeffAB.[0] coeffAB.[1]
        | 5 -> printfn "y = %f * x / (%f + x)" coeffAB.[0] coeffAB.[1]
        | _ -> failwith "Illegal number of regression functions"
    
    member this.CorrelationIndex =
        let syx2 =
            (ys, fxs)
            ||> List.map2 (fun y fx -> (y - fx) ** 2.0)
            |> List.sum
            |> (*) (1.0 / n)
        
        let sy2 =
            ys
            |> List.map (fun y -> (y - my) ** 2.0)
            |> List.sum
            |> (*) (1.0 / n)
        
        Math.Round(sqrt (1.0 - syx2 / sy2), 4)
    
    member this.Xs = xs |> Seq.cast<obj> |> List
    
    member this.Ys = ys |> Seq.cast<obj> |> List
    
    member this.Xfs = xfs |> Seq.cast<obj> |> List
    
    member this.Yfs = yfs |> Seq.cast<obj> |> List
    
    member this.Fxs = fxs |> Seq.cast<obj> |> List