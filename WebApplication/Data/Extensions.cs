using System;
using System.IO;
using System.Text;
using Aglomera;
using Newtonsoft.Json;

namespace WebApplication.Data
{
    public static class Extensions
    {
        public static string GetDendrogramJson<TInstance>(
            this ClusteringResult<TInstance> clustering,
            bool printNames = true,
            Formatting formatting = Formatting.None)
            where TInstance : IComparable<TInstance>
        {
            var sb = new StringBuilder();
            var sbw = new StringWriter(sb);
            var writer = new JsonTextWriter(sbw) {Formatting = formatting};
            WriteJson(clustering.SingleCluster, writer, printNames);
            return sb.ToString();
        }

        private static void WriteJson<TInstance>(
            Cluster<TInstance> cluster, JsonWriter writer, bool printNames)
            where TInstance : IComparable<TInstance>
        {
            writer.WriteStartObject();

            writer.WritePropertyName("n");
            writer.WriteValue(printNames ? cluster.ToString() : string.Empty);

            writer.WritePropertyName("d");
            writer.WriteValue(Math.Round(cluster.Dissimilarity, 2));

            writer.WritePropertyName("c");
            writer.WriteStartArray();
            if (cluster.Parent1 != null)
            {
                WriteJson(cluster.Parent2, writer, printNames);
                WriteJson(cluster.Parent1, writer, printNames);
            }

            writer.WriteEndArray();
            writer.WriteEndObject();
        }
    }
}