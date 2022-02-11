using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace Generator
{
    public static class DoTheWork
    {
        public static void ShowValue(ref int value)
        {
            DialogResult result = MessageBox.Show("C# Message Box",
                    "C# Message Box", MessageBoxButtons.OKCancel);

            if (result == DialogResult.OK)
                value = 1;
            else
                value = 2;
            return;
        }
    }
}