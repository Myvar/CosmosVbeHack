using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VbeHackHal;

namespace VbeHack
{
    public class CosmosVbeHack
    {
        public void Test()
        {
            VBEHackDriver.SetVBeMode(0x118);
        }


    }
}
