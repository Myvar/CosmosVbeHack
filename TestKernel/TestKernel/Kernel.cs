using System;
using System.Collections.Generic;
using System.Text;
using VbeHack;
using Sys = Cosmos.System;
namespace TestKernel
{
    public class Kernel : Sys.Kernel
    {
        protected override void BeforeRun()
        {
            var vbeh = new CosmosVbeHack();
            vbeh.Test();
        }

        protected override void Run()
        {
            Console.Write("Input: ");
            var input = Console.ReadLine();
            Console.Write("Text typed: ");
            Console.WriteLine(input);
        }
    }
}
