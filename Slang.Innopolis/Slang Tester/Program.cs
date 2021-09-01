using System;
using System.Collections.Generic;
using System.Diagnostics;

using SLang;

namespace SLangTester
{
    class Program
    {
        public static void Main(string[] args)
        {
            Test test = new Test();
            test.test();

            Options options = new Options();
            Message messagePool = new Message(options);

            Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\Part01.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\Core ver 0.79.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\T011.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Config Min\Version.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\Core ver 0.72.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\Core ver 0.6.2.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\Core ver 0.7 Updated.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\Comparable.slang",options);
        //  Reader reader = new Reader(messagePool,@"c:\Zouev\SLang\SLang Tests\Any.slang",options);
        //  Reader reader = new Reader(messagePool,@"unit Integer is end",options);

            if ( !reader.wasOpen ) return;  // Message was issued by new Reader
            Tokenizer tokenizer = new Tokenizer(reader,options,messagePool);
            ENTITY.init(tokenizer,0,messagePool,options);
            COMPILATION compilation = COMPILATION.parse();

            compilation.report(4);
        }
    }
}
