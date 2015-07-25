using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VbeHackCore;

namespace VbeHackHal
{
   
    public class VBEHackDriver
    {
        public static void SetVBeMode( ushort ModeNumber)
        {
            var x = new regs16_t();
            x.ax = 0X4F02;
            x.bx = ModeNumber;
            JumStatment.Jump(0x10, x);
        }
    }
}
