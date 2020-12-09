using System;
using System.Collections.Generic;
using ExamplesUtil;

namespace WebApplication.Data
{
    public static class Utils
    {
        public static double StringToDouble(string x)
        {
            return double.Parse(x, System.Globalization.CultureInfo.InvariantCulture);
        }

        public static TOutput[,] ConvertAll<TInput, TOutput>(TInput[,] data, Converter<TInput, TOutput> converter)
        {
            var output = new TOutput[data.GetLength(0), data.GetLength(1)];
            for (var i = 0; i < data.GetLength(0); i++)
            {
                for (var j = 0; j < data.GetLength(1); j++)
                {
                    output[i, j] = converter(data[i, j]);
                }
            }

            return output;
        }
        
        public static ISet<DataPoint> ClusterParse(IReadOnlyCollection<string[]> data)
        {
            var dataPoints = new HashSet<DataPoint>();

            foreach (var row in data)
            {
                var list = new List<double>();
                for (var i = 0; i < row.Length - 1; i++)
                {
                    var field = row[i];
                    if (double.TryParse(field, out var val))
                    {
                        list.Add(val);
                    }
                }

                if (list.Count > 0)
                {
                    dataPoints.Add(new DataPoint(row[^1], list.ToArray()));
                }
            }

            return dataPoints;
        }
        
        
    }
}