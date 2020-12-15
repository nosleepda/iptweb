using System;
using System.Collections.Generic;
using System.Globalization;
using ExamplesUtil;

namespace WebApplication.Data
{
    public static class Utils
    {
        private static double StringToDouble(string x)
        {
            return double.Parse(x, CultureInfo.InvariantCulture);
        }

        private static double StringToInt(string x)
        {
            return int.Parse(x, CultureInfo.InvariantCulture);
        }

        public static double[] StringToNumber(string rawData)
        {
            var data = rawData.Replace(" ", "").Split(";");

            return double.TryParse(data[0], NumberStyles.Number, CultureInfo.InvariantCulture, out _)
                ? Array.ConvertAll(data, StringToDouble)
                : Array.ConvertAll(data, StringToInt);
        }
        
        public static double[,] StringToNumber(string[,] rawData)
        {
            return double.TryParse(rawData[0, 0], out _)
                ? ConvertAll(rawData, StringToDouble)
                : ConvertAll(rawData, StringToInt);
        }

        public static TOutput[,] ConvertAll<TInput, TOutput>(TInput[,] data, Converter<TInput, TOutput> converter)
        {
            var output = new TOutput[data.GetLength(0), data.GetLength(1)];
            for (var i = 0; i < data.GetLength(0); i++)
            {
                for (var j = 0; j < data.GetLength(1); j++)
                {
                    var temp = data[i, j];
                    if (CheckElement<TInput>(temp))
                    {
                        throw new ArgumentException();
                    }
                    output[i, j] = converter(temp);
                }
            }

            return output;
        }

        private static bool CheckElement<T>(object elem)
        {
            return typeof(T) == typeof(string) && string.IsNullOrEmpty((string) elem);
        }

        public static ISet<DataPoint> ClusterParse(IEnumerable<string[]> data)
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