// This code models a principal part of a compiler.
// It should be compiled in "managed" mode that enables
// interoperability with C# code.

#include "conio.h"
#include <iostream>
#include <windows.h>

using namespace System;
using namespace System::Reflection;
using namespace Generator;

namespace Compiler {

    //public ref
    class DoGeneration
    {
        public:void ShowCSharpMessageBox(int& value)
        {
            Generator::DoTheWork::ShowValue(value);
            return;
        } 
    };
}

__declspec(dllexport) void ShowMessageBox(int& value)
{
    Compiler::DoGeneration work;
    work.ShowCSharpMessageBox(value);
}

int main()
{
    int result;

    ShowMessageBox(result);

    if (result == 1)
        printf("Ok Was Pressed \n");
    else
        if (result == 2)
            printf("Cancel Was Pressed \n");
        else
            printf("Unknown result \n");

    system("pause");

    return 0;
}