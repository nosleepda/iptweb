using System;

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
    }
}