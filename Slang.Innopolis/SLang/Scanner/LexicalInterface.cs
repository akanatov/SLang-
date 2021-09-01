using System;
using System.Collections.Generic;

namespace SLang
{
    public static class LexicalInterface
    {
        /// <summary>
        /// 
        /// </summary>
        public static Dictionary<uint, Token> TokenSequence = new Dictionary<uint, Token>();

        static uint add(Token token)
        {
            uint key = ((uint)token.span.begin.line << 16) | (uint)token.span.begin.pos;
            TokenSequence[key] = token;
            return key; 
        }
    }

    /// <summary>
    /// if, then, else, do, loop, unit etc: key of the token with the corresponding end
    /// end  : key of the corresponding starting token
    /// (, [ : key of the corresponding ), ]
    /// ), ] : key of the corresponding (, [
    /// reference identifier: key of the token that represents the declaration
    ///             of the entity named by the identifier.
    /// defining identifier: ...
    /// this
    /// , ;   key of the token starting the list/sequence: ( or [ or ...
    /// </summary>
    public class TokenSemantics
    {
        // Empty
    }

    public class Test
    {
        public Dictionary<TokenCode, Token> dict = new Dictionary<TokenCode, Token>();

        public List<TokenCode> codes = new List<TokenCode>();

        public void test()
        {
            codes.Add(TokenCode.Abstract);
            codes.Add(TokenCode.Alias);
            codes.Add(TokenCode.Ampersand);

            var e = codes.GetEnumerator();
            var b = e.MoveNext();
            TokenCode t = e.Current;
            b = e.MoveNext();
            t = e.Current;

            var e1 = codes.GetEnumerator();
            b = e1.MoveNext();
            t = e1.Current;
            b = e1.MoveNext();
            t = e1.Current;

        //  codes.Add(TokenCode.Assign);
        //  t = e1.Current;
        //  b = e1.MoveNext();
        //  t = e1.Current;

            LinkedList<TokenCode> linked = new LinkedList<TokenCode>();
            linked.AddLast(TokenCode.Abstract);
            linked.AddLast(TokenCode.Alias);
            linked.AddLast(TokenCode.Ampersand);
            linked.AddLast(TokenCode.Assign);

            var curr = linked.First;
            t = curr.Value;
            t = curr.Next.Value;
            t = curr.Next.Next.Value;
            curr = curr.Next;
            t = curr.Value;

            var curr1 = linked.First;
            linked.AddLast(TokenCode.Caret);
            t = curr.Value;
            t = curr1.Value;


        }
    }

}
