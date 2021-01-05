namespace MathStatistics

open System.Collections.Generic
open MathNet.Numerics.Distributions

type StatisticsData(sourceData: double[]) =
    
    let rand mean stdDev =
        let normalDist = Normal(mean, stdDev);
        normalDist.Sample()
    
    let sortedData = List.init 300000 (fun _ -> rand 100.0 10.0) |> List<double> |> List.ofSeq |> List.sort
    
    let k =
        sortedData.Length
        |> double
        |> sqrt
    
    let kRounded =
        sortedData.Length
        |> double
        |> sqrt
        |> round
    
    let h = (List.max sortedData - List.min sortedData) / k
    
    let initElem = List.min sortedData - 0.5 * h
    
    let lastElem = List.max sortedData + 0.5 * h
            
    member this.Interval =
        let arr = ResizeArray<double>(int kRounded + 1)
        let mutable temp = initElem
        arr.Add temp
        for i = 1 to int kRounded do
            temp <- temp + h
            arr.Add (temp)
        
        arr.Add lastElem
        arr |> List.ofSeq
    
    member this.Discrete =
        List.map2 (fun s e -> (s + e) / 2.0) (List.take (this.Interval.Length - 1) this.Interval) this.Interval.Tail
        
    member this.DiscreteObj = this.Discrete |> Seq.cast<obj> |> List 
    
    member this.FrequenciesInterval =
        let mutable data = List<double> sortedData

        let frequencies2 =
            List<int>(List.init (this.Interval.Length) (fun _ -> 0))

        for i = 0 to this.Interval.Length - 1 do
            while (data.Count > 0 && data.[0] <= this.Interval.[i]) do
                frequencies2.[i] <- frequencies2.[i] + 1
                data.RemoveAt(0)
        frequencies2
    
    member this.FrequenciesDiscrete =
        this.FrequenciesInterval.GetRange(1, this.FrequenciesInterval.Count - 1)
        
    member this.FrequenciesDiscreteObj = this.FrequenciesDiscrete |> Seq.cast<obj> |> List 
    
    member this.DictData =
        Seq.map2 (fun d f -> d, f) this.Discrete this.FrequenciesDiscrete
        |> dict
    
    member this.NGroup = this.Discrete.Length
    
    member this.N = sortedData.Length
    
    member this.NDouble = sortedData.Length |> double
    
    member this.SortedData = sortedData
    
    member this.H = h