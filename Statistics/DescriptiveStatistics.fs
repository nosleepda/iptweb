namespace Statistics

open System
open System.Collections.Generic

type DescriptiveStatistics(sourceData: double[], alf: double) =    
    inherit StatisticsData(sourceData)
       
    member this.Mean =
        let mutable sum = 0.0
        for x in base.DictData do
            sum <- sum + x.Key * double x.Value

        Math.Round(sum / base.NDouble, 4)
    
    member this.Variance =
        let mutable sum = 0.0
        for x in base.DictData do
            sum <- sum + ((this.Mean - x.Key) ** 2.0) * (double x.Value)

        sum <- sum / double (if base.N > 50 then base.N - 1 else base.N)

        Math.Round(sum, 4)
    
    member this.StandardDeviation =
        Math.Round(sqrt this.Variance, 4)
    
    member this.Mode =
        let max = base.DictData.Values |> List.ofSeq |> List.max
        let mutable key = 0.0
        for x in base.DictData do
            if max = x.Value then key <- x.Key
        Math.Round(key, 4)
    
    member this.Median: double =
        let mutable m = 0.0
        if base.N % 2 = 0 then
            let kk = base.N / 2
            m <- (base.SortedData.Item kk + base.SortedData.Item(kk + 1)) / 2.0
        else
            let kk = (base.N - 1) / 2
            m <- base.SortedData.Item(kk + 1)
        Math.Round(m, 4)
    
    member this.CoefficientVariation =
        Math.Round(this.StandardDeviation / this.Mean, 4)
    
    member this.Kurtosis =
        let mutable sum = 0.0
        for x in base.DictData do
            sum <- sum + ((x.Key - this.Mean) ** 4.0) * (double x.Value)

        sum <- sum / base.NDouble
        
        Math.Round(sum / (this.StandardDeviation ** 4.0) - 3.0, 4)
        
    member this.Skewness =
        let mutable sum = 0.0
        for x in base.DictData do
            sum <- sum + ((x.Key - this.Mean) ** 3.0) * (double x.Value)
            
        sum <- sum / base.NDouble

        Math.Round(sum / (this.StandardDeviation ** 3.0), 4)
        
    member this.ConfidenceIntervalMean =
        let t = Utilities.Student (base.NDouble - 1.0) alf
        let temp = t * this.StandardDeviation / (sqrt (base.NDouble))
        String.Format("{0} < M < {1}", Math.Round(this.Mean - temp, 4), Math.Round(this.Mean + temp, 4))
    
    member this.ConfidenceIntervalVariance =
        let n1 = (base.NDouble - 1.0)
        let temp a =
            this.Variance * n1 / Utilities.ChiSquared (n1 / 2.0) a
        let l = sqrt (temp (1.0 + alf))
        let r = sqrt (temp (1.0 - alf))
        String.Format("{0} < D < {1}", Math.Round(l, 4), Math.Round(r, 4))
    
    member this.Ws =
        let n = base.NDouble
        base.FrequenciesDiscrete
        |> List.ofSeq
        |> List.map (fun x -> double x / n)
        
    member this.Cumulate =
        let da = List<float>(this.Ws.Length)
        da.Add this.Ws.Head
        for x = 1 to this.Ws.Length - 1 do
            da.Add(da.[x - 1] + this.Ws.[x])
        da