//------------------------------------------------------------------------------
/// @brief SnuPL/0 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 16

char ETokenName[][TOKEN_STRLEN] = {
  "tCharacter",                     ///< a character
  "tString",                        ///< a string
  "tIdent",                         ///< an ident
  "tNumber",                        ///< a number
  "tBoolean",                       ///< a boolean
  "tBaseType",                      ///< a basetype
  "tFactOp",                        ///< '*' or '/' or "&&"
  "tTermOp",                        ///< '+' or '-' or "||" 
  "tRelOp",                         ///< relational operator
  "tNot",                           ///< a '!' 
  "tLBrak",                         ///< a '('
  "tRBrak",                         ///< a ')'
  "tLSqrBrak",                      ///< a '['
  "tRSqrBrak",                      ///< a ']'
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tComma",                         ///< a comma
  "tDot",                           ///< a dot
  "tModule",                        ///< module
  "tBegin",                         ///< begin
  "tEnd",                           ///< end
  "tIf",                            ///< if
  "tThen",                          ///< then
  "tElse",                          ///< else
  "tWhile",                         ///< while
  "tDo",                            ///< do
  "tReturn",                        ///< return
  "tVar",                           ///< var
  "tProcedure",                     ///< procedure
  "tFunction",                      ///< function


  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined",                     ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tCharacter (%s)",                ///< a character
  "tString (%s)",                   ///< a string
  "tIdent (%s)",                    ///< an ident
  "tNumber (%s)",                   ///< a number
  "tBoolean (%s)",                  ///< a boolean
  "tBaseType (%s)",                 ///< a basetype
  "tFactOp (%s)",                   ///< '*' or '/' or "&&"
  "tTermOp (%s)",                   ///< '+' or '-' or "||" 
  "tRelOp (%s)",                    ///< relational operator
  "tNot",                           ///< a '!' 
  "tLBrak",                         ///< a '('
  "tRBrak",                         ///< a ')'
  "tLSqrBrak",                      ///< a '['
  "tRSqrBrak",                      ///< a ']'
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tComma",                         ///< a comma
  "tDot",                           ///< a dot
  "tModule",                        ///< module
  "tBegin",                         ///< begin
  "tEnd",                           ///< end
  "tIf",                            ///< if
  "tThen",                          ///< then
  "tElse",                          ///< else
  "tWhile",                         ///< while
  "tDo",                            ///< do
  "tReturn",                        ///< return
  "tVar",                           ///< var
  "tProcedure",                     ///< procedure
  "tFunction",                      ///< function


  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)",                ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
{
  make_pair("true", tBoolean),
  make_pair("false", tBoolean),
  make_pair("boolean", tBaseType),
  make_pair("char", tBaseType),
  make_pair("integer", tBaseType),
  make_pair("module", tModule),
  make_pair("begin", tBegin),
  make_pair("end", tEnd),
  make_pair("if", tIf),
  make_pair("then", tThen),
  make_pair("else", tElse),
  make_pair("while", tWhile),
  make_pair("do", tDo),
  make_pair("return", tReturn),
  make_pair("var", tVar),
  make_pair("procedure", tProcedure),
  make_pair("function", tFunction)
};



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  _value = escape(value);
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const
{
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(const string text)
{
  const char *t = text.c_str();
  string s;

  while (*t != '\0') {
    switch (*t) {
      case '\n': s += "\\n";  break;
      case '\t': s += "\\t";  break;
      case '\0': s += "\\0";  break;
      case '\'': s += "\\'";  break;
      case '\"': s += "\\\""; break;
      case '\\': s += "\\\\"; break;
      default :  s += *t;
    }
    t++;
  }

  return s;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
}

void CScanner::RecordStreamPosition()
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan()
{
  EToken token;
  string tokval;
  char c;

  while (_in->good() && IsWhite(_in->peek())) GetChar();

  RecordStreamPosition();

  if (_in->eof()) return NewToken(tEOF);
  if (!_in->good()) return NewToken(tIOError);

  c = GetChar();
  tokval = c;
  token = tUndefined;

  switch (c) {
    case ':':
      if (_in->peek() == '=') {
        tokval += GetChar();
        token = tAssign;
      }
      else {
        token = tColon;
      }
      break;

    case '+':
    case '-':
      token = tTermOp;
      break;

    case '|':
      if(_in->peek() == '|') {
        tokval += GetChar();
        token = tTermOp;
      }
      break;

    case '*':
      token = tFactOp;
      break;

    case '/':
      if(_in->peek() == '/') {
        GetChar();
        while(_in->peek() != EOF && _in->peek() != '\n') GetChar();
        if(_in->peek() == EOF) {
          return NewToken(tEOF);
        } else {
          GetChar();
          return Scan();
        }
      } else {
        token = tFactOp;
      }
      break;

    case '&':
      if(_in->peek() == '&') {
        tokval += GetChar();
        token = tFactOp;
      }
      break;

    case '=':
    case '#':
      token = tRelOp;
      break;

    case '<':
    case '>':
      token = tRelOp;
      if(_in->peek() == '=') {
        tokval += GetChar();
      }
      break;

    case ';':
      token = tSemicolon;
      break;

    case ',':
      token = tComma;
      break;

    case '.':
      token = tDot;
      break;

    case '!':
      token = tNot;
      break;

    case '(':
      token = tLBrak;
      break;

    case ')':
      token = tRBrak;
      break;

    case '[':
      token = tLSqrBrak;
      break;

    case ']':
      token = tRSqrBrak;
      break;

    case '\'':
      while(true) {
        if(_in->peek() == EOF) break;
        if(_in->peek() == '\'') break;
        if(_in->peek() == '\\') {
          tokval += GetChar();
          if(_in->peek() != EOF) tokval += GetChar();
        } else {
          tokval += GetChar();
        }
      }
      if(_in->peek() == EOF) {
        string tmp = "Unmatched single quote : ";
        tokval = tmp + tokval;
      } else {
        GetChar();
        tokval = tokval.substr(1); /// get rid of quotation mark
        if(IsValidChar(tokval)) {
          token = tCharacter;
        } else {
          string tmp = "Invalid character description : ";
          tokval = tmp + tokval;
        }
      }
      break;

    case '"':
       while(true) {
        if(_in->peek() == EOF) break;
        if(_in->peek() == '\"') break;
        if(_in->peek() == '\\') {
          tokval += GetChar();
          if(_in->peek() != EOF) tokval += GetChar();
        } else {
          tokval += GetChar();
        }
      }
      if(_in->peek() == EOF) {
        string tmp = "Unmatched double quote : ";
        tokval = tmp + tokval;
      } else {
        GetChar();
        tokval = tokval.substr(1); // get rid of quotation mark
        if(IsValidString(tokval)) {
          token = tString;
        } else {
          string tmp = "Invalid string description : ";
          tokval = tmp + tokval;
        }
      }
      break;


    default:
      if (IsDigit(c)) {
        token = tNumber;
        while(IsDigit(_in->peek())) tokval += GetChar();
      } else
      if (IsLetter(c)) {
        while(IsLetterDigit(_in->peek())) tokval += GetChar();
        map<string, EToken>::iterator iter = keywords.find(tokval); 
        if(iter != keywords.end()) {
          token = iter->second;
        } else {
          token = tIdent;
        }
      } else {
        tokval = "invalid character '";
        tokval += c;
        tokval += "'";
      }
      break;
  }

  return NewToken(token, tokval);
}

char CScanner::GetChar()
{
  char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsWhite(char c) const
{
  return ((c == ' ') || (c == '\n'));
}

bool CScanner::IsDigit(char c) const
{
  return (('0' <= c) && (c <= '9'));
}

bool CScanner::IsLetter(char c) const
{
  return (('a' <= c) && (c <= 'z')) ||
         (('A' <= c) && (c <= 'Z')) ||
         (c == '_');
}

bool CScanner::IsLetterDigit(char c) const
{
  return IsLetter(c) || IsDigit(c);
}

bool CScanner::IsAsciiChar(char c) const
{
  return (32 <= c) && (c <= 126);
}

bool CScanner::IsValidChar(string s) const
{
  if(s.length() > 2 || s.length() == 0) return false;
  else if(s.length() == 2) {
    if(s[0] == '\\' && 
      (s[1] == 'n' || s[1] == 't' || s[1] == '"' || s[1] == '\'' || s[1] == '\\' || s[1] == '0'))
        return true;
    return false;
  } else {
    return IsAsciiChar(s[0]);
  }
}

bool CScanner::IsValidString(string s) const
{
  const char* c = (char *)s.c_str();
  while(*c != '\0') {
    if(!IsAsciiChar(*c)) return false;
    c++;
  }
  return true;
} 
