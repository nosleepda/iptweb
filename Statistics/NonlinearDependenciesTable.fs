namespace MathStatistics

open System
open System.Collections.Generic
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

type NonlinearDependenciesTable(xs: double[], ys: double[], ns: double[,], alf: double) =

    let xTitle = xs |> List.ofSeq
    
    let yTitle = ys |> List.ofSeq
    
    let matrix = DenseMatrix.OfArray ns

    let ps =
        matrix.RowSums()
        |> List.ofSeq
    
    let pys =
        List.map2 (fun p y -> p * y) ps yTitle
        
    let ws =
        matrix.ColumnSums()
        |> List.ofSeq
    
    let ny =
        matrix.EnumerateColumns()
        |> List.ofSeq
        |> List.map (fun column ->
           List.ofSeq column
           |> List.map2 (fun elem1 elem2 -> elem1 * elem2) yTitle
           |> List.sum)
    
    let ys =
        ny
        |> List.map2 (fun elem1 elem2 -> elem2 / elem1) ws
    
    let k = double xTitle.Length 
    
    let n = List.sum ps
    
    let my =
        pys
        |> List.sum
        |> (*) (1.0 / n)
    
    let mx = xTitle |> List.sum |> (*) (1.0 / n)
    
    let sx = 
        xTitle
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
    
    let pymy2 =
        (yTitle, ps)
        ||> List.map2 (fun elem1 elem2 -> (elem1 - my) ** 2.0 * elem2) 
        |> List.sum
    
    let wysmy2 =
        (ys, ws)
        ||> List.map2 (fun elem1 elem2 -> (elem1 - my) ** 2.0 * elem2)
        |> List.sum
    
    let x1 = sqrt (List.max xTitle * List.min xTitle)
    
    let x2 = (List.max xTitle + List.min xTitle) / 2.0
    
    let x3 = List.max xTitle * List.min xTitle * 2.0 / (List.max xTitle + List.min xTitle)
    
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
            let resultTempX = List<double>[xTitle.[0]]
            let resultTempY = List<double>[ys.[0]]
            (listX.Tail, listY.Tail)
            ||> List.iter2 (fun x y ->
                resultTempX.Add (lastElem resultTempX + (x - lastElem resultTempX) / 2.0)
                resultTempY.Add (lastElem resultTempY + (lastElem resultTempX - predLastElem resultTempX) / (x - predLastElem resultTempX) * (y - lastElem resultTempY))
                resultTempX.Add x;
                resultTempY.Add y)
            (List.ofSeq resultTempX, List.ofSeq resultTempY)
            
        (xTitle, ys) ||> calc ||> calc
    
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
        
        let minKey =
            valuesOfFunc.Keys
            |> List.ofSeq
            |> List.min
                     
        valuesOfFunc.Item minKey
    
    let calcMatrix t t2 g gt =
        let list = [|n; t; t; t2|]
        let m = DenseMatrix(2,2,list) 
        let fs = [g;gt] |> vector
        m.Solve fs |> List.ofSeq
    
    let nfy (f:float->float) =
        matrix.EnumerateColumns()
        |> List.ofSeq
        |> List.map (fun column ->
           List.ofSeq column
           |> List.map2 (fun elem1 elem2 -> f elem1 * elem2) yTitle
           |> List.sum)
    
    //y=a*x^b
    let regressionOne =
        let t = (xTitle, ws) ||> List.map2 (fun x w -> w * log x) |> List.sum
        let t2 = (xTitle, ws) ||> List.map2 (fun x w -> w * (log x) ** 2.0) |> List.sum
        let g = (yTitle, ps) ||> List.map2 (fun y p -> p * log y) |> List.sum
        let gt = (xTitle, nfy log) ||> List.map2 (fun x ny -> log x * ny) |> List.sum
        let r = calcMatrix t t2 g gt
        [exp r.[0]; r.[1]]
    
    let funcRegressionOne x =
        regressionOne.[0] * x ** regressionOne.[1]
    
        //y=a*b^2
    let regressionTwo =
        let t = (xTitle, ws) ||> List.map2 (fun x w -> w * x) |> List.sum
        let t2 = (xTitle, ws) ||> List.map2 (fun x w -> w * x ** 2.0) |> List.sum
        let g = (yTitle, ps) ||> List.map2 (fun y p -> p * log y) |> List.sum
        let gt = (xTitle, nfy log) ||> List.map2 (fun x ny -> x * ny) |> List.sum
        let r = calcMatrix t t2 g gt
        [exp r.[0]; exp r.[1]]
    
    let funcRegressionTwo x =
        regressionTwo.[0] * regressionTwo.[1] ** x
    
    //y=1/(a+b*x)
    let regressionThree =
        let t = (xTitle, ws) ||> List.map2 (fun x w -> w * x) |> List.sum
        let t2 = (xTitle, ws) ||> List.map2 (fun x w -> w * x ** 2.0) |> List.sum
        let g = (yTitle, ps) ||> List.map2 (fun y p -> p / y) |> List.sum
        let gt = (xTitle, nfy (fun y -> 1.0 / y)) ||> List.map2 (fun x ny -> x * ny) |> List.sum
        let r = calcMatrix t t2 g gt
        [r.[0]; r.[1]]
    
    let funcRegressionThree x =
        1.0 / (regressionThree.[0] + regressionThree.[1] * x)
    
    //y=a+b*lgx
    let regressionFour =
        let t = (xTitle, ws) ||> List.map2 (fun x w -> w * log10 x) |> List.sum
        let t2 = (xTitle, ws) ||> List.map2 (fun x w -> w * (log10 x) ** 2.0) |> List.sum
        let g = (yTitle, ps) ||> List.map2 (fun y p -> p * y) |> List.sum
        let gt = (xTitle, nfy (fun y -> y)) ||> List.map2 (fun x ny -> log10 x * ny) |> List.sum
        let r = calcMatrix t t2 g gt
        [r.[0]; r.[1]]
    
    let funcRegressionFour x =
        regressionFour.[0] + regressionFour.[1] * (log10 x)
    
    //y=a+b/x
    let regressionFive =
        let t = (xTitle, ws) ||> List.map2 (fun x w -> w / x) |> List.sum
        let t2 = (xTitle, ws) ||> List.map2 (fun x w -> w / (x ** 2.0)) |> List.sum
        let g = (yTitle, ps) ||> List.map2 (fun y p -> p * y) |> List.sum
        let gt = (xTitle, nfy (fun y -> y)) ||> List.map2 (fun x ny -> (1.0 / x) * ny) |> List.sum
        let r = calcMatrix t t2 g gt
        [r.[0]; r.[1]]
    
    let funcRegressionFive x =
        regressionFive.[0] + regressionFive.[1] / x
    
    //y=a*x/(b+x)
    let regressionSix =
        let t = (xTitle, ws) ||> List.map2 (fun x w -> w / x) |> List.sum
        let t2 = (xTitle, ws) ||> List.map2 (fun x w -> w / (x ** 2.0)) |> List.sum
        let g = (yTitle, ps) ||> List.map2 (fun y p -> p / y) |> List.sum
        let gt = (xTitle, nfy (fun y -> 1.0 / y)) ||> List.map2 (fun x ny -> (1.0 / x) * ny) |> List.sum
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
    
    let fxs = xTitle |> List.map (fun x -> funcRegression x)
    
    let s1 =
        matrix.EnumerateColumns()
        |> List.ofSeq
        |> List.mapi (fun index column ->
           List.ofSeq column
           |> List.map2 (fun y n -> (y - ys.[index]) ** 2.0 * n) yTitle
           |> List.sum)
        |> List.sum
        |> (*) (1.0 / (n - k))
    
    let s2 =
        List.map (fun x -> funcRegression x) xTitle
        |> List.map3 (fun elem2 elem3 elem1 -> (elem1 - elem2) ** 2.0 * elem3) ys ws
        |> List.sum
        |> (*) (1.0 / (k - 2.0))
   
    member this.MeanX = Math.Round(mx, 4)
    
    member this.MeanY = Math.Round(my, 4)
    
    member this.Sx = Math.Round(sx, 4)
    
    member this.Sy = Math.Round(sy, 4)
    
    member this.CorrelationRatio = sqrt ((wysmy2 / n) / (pymy2 / n))
    
    member this.RegressionFunction =
        match numberOfFunc with
        | 0 -> printfn "y = %f * x ^ %f" coeffAB.[0] coeffAB.[1]
        | 1 -> printfn "y = %f * %f ^ x" coeffAB.[0] coeffAB.[1]
        | 2 -> printfn "y = 1 / (%f + %f * x)" coeffAB.[0] coeffAB.[1]
        | 3 -> printfn "y = %f + %f * lgx" coeffAB.[0] coeffAB.[1]
        | 4 -> printfn "y = %f + %f / x" coeffAB.[0] coeffAB.[1]
        | 5 -> printfn "y = %f * x / (%f + x)" coeffAB.[0] coeffAB.[1]
        | _ -> failwith "Illegal number of regression functions"
    
    member this.Determination =
        let r1 =
            List.map2 (fun elem1 elem2 -> (elem1 - my) ** 2.0 * elem2) yTitle ps
            |> List.sum
            
        let r2 =
            fxs
            |> List.map2 (fun elem2 elem1 -> (elem1 - my) ** 2.0 * elem2) ws
            |> List.sum
            
        r2 / r1
    
    member this.Adequacy =
        let l = s2 / s1
        let r = Utilities2.Fisher 1.0 (n - 2.0) alf
        let bool = if l < r then "" else "не "
        let bool2 = if l < r then "<" else ">"
        String.Format("Уравнение регрессии статистически {0}значимо, \nт.к. {1} {2} {3}", bool, l, bool2, r )
        
    member this.Xs = xs |> Seq.cast<obj> |> List
    
    member this.Ys = ys |> Seq.cast<obj> |> List
    
    member this.Xfs = xfs |> Seq.cast<obj> |> List
    
    member this.Yfs = yfs |> Seq.cast<obj> |> List
    
    member this.Fxs = fxs |> Seq.cast<obj> |> List