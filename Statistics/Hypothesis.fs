namespace MathStatistics

open System
open System.Collections.Generic

type Hypothesis(sourceData: double[], alf: double) =
    
    let statistics = DescriptiveStatistics(sourceData, alf)
    
    let frequenciesDiscreteShort =
        let result = List<int>()
        let mutable temp = 0
        for i = 0 to statistics.Data.FrequenciesDiscrete.Count - 1 do
            temp <- temp + statistics.Data.FrequenciesDiscrete.[i]
            if temp >= 5 then
                result.Add temp
                temp <- 0
        if temp <> 0 then
            result.[result.Count - 1] <- result.[result.Count - 1] + temp
        List.ofSeq result
    
    let getMinValue =
        statistics.Data.Discrete |> List.min |> (-) -(statistics.Data.H * 1000.0)
    
    let getMaxValue = statistics.Data.Discrete |> List.max |> (+) (statistics.Data.H * 1000.0)
    
    let intervalShort =
        let interval = statistics.Data.Interval
        let freq = statistics.Data.FrequenciesInterval
        let result = List<double>[getMinValue]
        let mutable temp = 0
        for i = 1 to freq.Count - 1 do
            temp <- temp + freq.[i]
            if temp >= 5 then
                result.Add interval.[i]
                temp <- 0
                
        if temp < 5 then
            result.[result.Count - 1] <- getMaxValue
        else
            result.Add getMaxValue

        List.ofSeq result
    
    let frequenciesDiscreteExp =
        let mean = statistics.Mean
        let std = statistics.StandardDeviation
        let us = List.map (fun elem -> (elem - mean) / std) statistics.Data.Discrete
        let value1 = 1.0 / sqrt (2.0 * Math.PI)
        let value2 = statistics.Data.NDouble * statistics.Data.H / statistics.StandardDeviation
        
        let result = List.map (fun elem -> value1 * exp (elem ** 2.0 / -2.0)) us
                     |> List.map (fun elem -> value2 * elem)
                     |> List.map (fun elem -> Math.Round elem)
                     |> List.map (fun elem -> int elem)
                     |> List<int>
                     
        List.ofSeq result
        
    let isShort =
       List.ofSeq statistics.Data.FrequenciesDiscrete
       |> List.tryFind (fun x -> x < 5)
    
    let frequenciesDiscreteDouble = List.ofSeq statistics.Data.FrequenciesDiscrete |> List.map (fun elem -> double elem)
    
    let frequenciesDiscreteExpDouble =
        let r = List.map (fun elem -> double elem) frequenciesDiscreteExp
        r
    
    let frequenciesDiscreteShortDouble = List.map (fun elem -> double elem) frequenciesDiscreteShort
    
    let freedom = (frequenciesDiscreteExp.Length - 2 - 1) |> double
    
    let freedomShort = (frequenciesDiscreteShort.Length - 2 - 1) |> double
    
    let ps =
        let mean = statistics.Mean
        let std = statistics.StandardDeviation
        let p a b = abs ((Utilities.Normal mean std a) - (Utilities.Normal mean std b))
        let result = List<double>()

        for i = 1 to intervalShort.Length - 1 do
            result.Add (p intervalShort.[i - 1] intervalShort.[i])
        List.ofSeq result
        
    let chiSquaredShort =
        let n = statistics.Data.NDouble
        List.map (fun elem -> elem * n) ps
        |> List.map2 (fun elem1 elem2 -> (elem1 - elem2) ** 2.0 / elem2) frequenciesDiscreteShortDouble
        |> List.sum
    
    let chiSquaredFull =
        List.map2 (fun n n' -> (n - n') ** 2.0 / n') frequenciesDiscreteDouble frequenciesDiscreteExpDouble
        |> List.sum
        
    member this.ChiSquared =
        let f = if isShort.IsSome then freedomShort else freedom
        let cs = if isShort.IsSome then chiSquaredShort else chiSquaredFull
        let chiSquaredCritical = Utilities.ChiSquared f (1.0 - alf) 
        
        let bool =
            if cs < chiSquaredCritical then "" else "не "
        let bool2 = if cs < chiSquaredCritical then "<" else ">="
        String.Format("Условие критерия Пирсона {0}выполняется {1} {2} {3} ", bool, cs, bool2, chiSquaredCritical)
    
    member this.Kolmogorov =
        let cumulative (list:list<int>) =
            let result = List<int>[list.Item 0]
            for i = 1 to list.Length - 1 do
                result.Add (result.[i - 1] + (list.Item i))
            result |> List.ofSeq
            
        let funcK a =
            [-1000.0 .. 1000.0]
            |> List.map (fun elem -> ((-1.0) ** elem) * exp (-2.0 * (elem ** 2.0) * (a ** 2.0)))
            |> List.sum
            |> (fun x -> 1.0 - x)
            
        let c1 = cumulative (List.ofSeq statistics.Data.FrequenciesDiscrete)
        let c2 = cumulative (List.ofSeq frequenciesDiscreteExp)
        let max = List.map2 (fun elem1 elem2 -> abs (elem1 - elem2)) c1 c2 |> List.max |> double
        let k = max / sqrt statistics.Data.NDouble |> funcK
        let bool =
            if (k > alf) then "" else "не "
        let bool2 = if k > alf then ">" else "<="
        String.Format("Условие критерия Колмогорова {0}выполняется {1} {2} {3}", bool, k, bool2, alf)
       
    member this.Romanovsky =
        let f = if isShort.IsSome then freedomShort  else freedom
        let cs = if isShort.IsSome then chiSquaredShort else chiSquaredFull
        
        let r = abs (cs - f) / sqrt (2.0 * f)
        let bool =
            if r < 3.0 then "" else "не "
        let bool2 = if r < 3.0 then "<" else ">="
        String.Format("Условие критерия Романовского {0}выполняется {1} {2} 3 ", bool, r, bool2)
        
    member this.Yastremsky =
        let n = statistics.Data.NDouble
        let cs = List.map (fun elem -> elem * n) ps
                |> List.map2 (fun elem1 elem2 -> (elem1 - elem2) ** 2.0 / (elem2 * (1.0 - elem2 / n))) frequenciesDiscreteShortDouble
                |> List.sum
        
        let ss = List.map2 (fun elem1 elem2 -> (elem1 - elem2) ** 2.0) frequenciesDiscreteDouble frequenciesDiscreteExpDouble
        
        let cf = List.map (fun elem -> elem / n) frequenciesDiscreteExpDouble
                |> List.map (fun elem -> 1.0 - elem)
                |> List.map2 (fun elem1 elem2 -> elem1 * elem2) frequenciesDiscreteExpDouble
                |> List.map2 (fun elem1 elem2 -> (elem1 / elem2)) ss
                |> List.sum
        
        let k =  if isShort.IsSome then frequenciesDiscreteShort.Length else frequenciesDiscreteExp.Length 
        let c = if isShort.IsSome then cs else cf
        let t = 0.6       
        let j = abs (c - double k) / sqrt (2.0 * double k + 4.0 * t)
        
        let bool =
            if (j <= 3.0) then "" else "не "
            
        let bool2 = if j <= 3.0 then "<=" else ">"    
        String.Format("Условие критерия Ястремского {0}выполняется {1} {2} 3 ", bool, j, bool2)
    
    member this.Approximate =
        let n = statistics.Data.NDouble
        let sas = sqrt (6.0 * (n - 1.0) / ((n + 1.0) * (n + 3.0)))
        let sex = sqrt ( 24.0 * n * (n - 2.0) * (n - 3.0) / (((n - 1.0) ** 2.0) * (n + 3.0) * n + 5.0))
        let bool1 = (abs statistics.Skewness <= sas) && (abs statistics.Kurtosis <= sex) 
        
        let chi =
            statistics.Skewness ** 2.0 / sas ** 2.0 + statistics.Kurtosis ** 2.0 / sex ** 2.0
        let chiCritical = Utilities.ChiSquared 2.0 alf
        
        let bool2 =
            if bool1 && (chi < chiCritical) then "" else "не "
            
        let bool3 = if bool1 && (chi < chiCritical) then "<" else ">="
       
        String.Format("Условие приближенного критерия {0}выполняется {1} {2} {3} ", bool2, chi, bool3, chiCritical)
        
    member this.FrequenciesDiscreteExpObj = frequenciesDiscreteExp |> Seq.cast<obj> |> List  