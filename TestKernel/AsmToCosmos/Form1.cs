using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace AsmToCosmos
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            OpenFileDialog dlg = new OpenFileDialog();
            if (dlg.ShowDialog() == DialogResult.OK)
            {
               richTextBox1.Text = CreatePlug(File.ReadAllText(dlg.FileName), new FileInfo(dlg.FileName).Name);
            }

        }

        public string CreatePlug(string asm, string name)
        {
            //parseng
            string[] rawplugs = Properties.Resources.Plug.Split('~') ;
            //stuf we need
            string PlugRaw = rawplugs[0];
            string PlugRawLine = rawplugs[1];
            string pllines = "";
            foreach (var i in asm.Replace("\r\n", "\n"/* Linux File new line suport */).Split('\n'))
            {
                pllines += "\t\t" + PlugRawLine.Replace("::{ASM}::", i) + "\n";
            }

            return PlugRaw.Replace("::{NAME}::", name.Replace(".","")).Replace("::{PLUG}::", pllines);

        }
    }
}
