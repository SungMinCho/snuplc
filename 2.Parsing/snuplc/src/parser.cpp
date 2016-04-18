//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      //if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  // TODO: add predefined functions here
  CSymProc *DIM, *DOFS, *ReadInt, *WriteChar, *WriteInt, *WriteLn, *WriteStr;

  DIM = new CSymProc("DIM", tm->GetInt());
  DIM->AddParam(new CSymParam(0, "arg0", tm->GetPointer(tm->GetNull()))); // TODO : what is argument name? same TODO for below
  DIM->AddParam(new CSymParam(1, "arg1", tm->GetInt()));

  DOFS = new CSymProc("DOFS", tm->GetInt());
  DOFS->AddParam(new CSymParam(0, "arg0", tm->GetPointer(tm->GetNull())));

  ReadInt = new CSymProc("ReadInt", tm->GetInt());

  WriteChar = new CSymProc("WriteChar", tm->GetNull());
  WriteChar->AddParam(new CSymParam(0, "c", tm->GetChar()));

  WriteInt = new CSymProc("WriteInt", tm->GetNull());
  WriteInt->AddParam(new CSymParam(0, "i", tm->GetInt()));

  WriteLn = new CSymProc("WriteLn", tm->GetNull());

  WriteStr = new CSymProc("WriteStr", tm->GetNull());
  // TODO : second tm->GetPointer should be tm->GetArray but i don't know the size of the ary
  WriteStr->AddParam(new CSymParam(0, "s", tm->GetPointer(tm->GetPointer(tm->GetChar())))); 

  s->AddSymbol(DIM);
  s->AddSymbol(DOFS);
  s->AddSymbol(ReadInt);
  s->AddSymbol(WriteChar);
  s->AddSymbol(WriteInt);
  s->AddSymbol(WriteLn);
  s->AddSymbol(WriteStr);
}

const CType* CParser::type() {
  const CType* t;
  CToken basetype;
  Consume(tBaseType, &basetype);
  if(basetype.GetValue() == "boolean") t = CTypeManager::Get()->GetBool();
  else if(basetype.GetValue() == "char") t = CTypeManager::Get()->GetChar();
  else t = CTypeManager::Get()->GetInt(); // ensured by my scanner design

  while(true) {
    if(_scanner->Peek().GetType() != tLSqrBrak) return t;
    Consume(tLSqrBrak);
    if(_scanner->Peek().GetType() != tRSqrBrak) {
      CAstConstant* num = number();
      Consume(tRSqrBrak);
      t = CTypeManager::Get()->GetArray(num->GetValue(), t); // TODO : mind that num->GetValue long long to int...
    } else {
      Consume(tRSqrBrak);
      t = CTypeManager::Get()->GetPointer(t);
    }
  }

  return t;
}

void CParser::varDecl(CAstScope* s) {
  // varDecl = ident { "," ident } ":" type
  vector<CToken> vars;
  while(true) {
    CToken var;
    Consume(tIdent, &var);
    vars.push_back(var);
    if(_scanner->Peek().GetType() == tColon) break;
    Consume(tComma);
  }
  Consume(tColon);
  const CType* typ = type();

  vector<CToken>::iterator iter;
  for(iter = vars.begin(); iter != vars.end(); iter++) {
    s->GetSymbolTable()->AddSymbol(s->CreateVar(iter->GetValue(), typ));
  }
}

void CParser::varDeclSequence(CAstScope* s) {
  // varDeclSequence ::= varDecl { ";" varDecl }
  while(true) {
    varDecl(s);
    if(_scanner->Peek().GetType() != tSemicolon) return;
    Consume(tSemicolon);
    if(_scanner->Peek().GetType() == tBegin ||
       _scanner->Peek().GetType() == tProcedure ||
       _scanner->Peek().GetType() == tFunction ) return; // in this case, the semicolon we've eaten belongs to varDeclaration
  }
}

void CParser::varDeclaration(CAstScope* s) {
  // varDeclaration ::= [ "var" varDeclSequence ";" ]
  if(_scanner->Peek().GetType() != tVar) return;

  Consume(tVar);
  varDeclSequence(s);
  //Consume(tSemicolon);    varDeclSequence eats this
}

CAstModule* CParser::module(void)
{
  //
  // old module ::= statSequence  ".".
  // module ::= "module" ident ";"' varDeclaration { subroutineDecl } "begin" statSequence "end" ident "."
  //
  Consume(tModule);
  CToken moduleName;
  Consume(tIdent, &moduleName);

  // TODO. is it right to put moduleName for 1st parameter?
  CAstModule *m = new CAstModule(moduleName, moduleName.GetValue());

  InitSymbolTable(m->GetSymbolTable());

  Consume(tSemicolon);
  varDeclaration(m);

  while(true) {
    EToken tt = _scanner->Peek().GetType();
    if(tt == tBegin) break;
    CAstProcedure* subroutine = subroutineDecl(m);
    m->AddProcedure(subroutine);
  }

  Consume(tBegin);

  CAstStatement *statseq = NULL;
  statseq = statSequence(m);

  Consume(tEnd);

  CToken endingName;
  Consume(tIdent, &endingName);
  assert(moduleName.GetValue() == endingName.GetValue() && "module should end with \"end\" modulename");

  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}

void CParser::formalParam(CAstScope *s) {
  // formalParam ::= "(" [ varDeclSequence ] ")"
  Consume(tLBrak);
  varDeclSequence(s);
  Consume(tRBrak);
}

CAstProcedure* CParser::subroutineDecl(CAstScope *s) {
  // subroutineDecl ::= (procedureDecl | functionDecl) subroutineBody ident ";"
  // procedureDecl ::= "procedure" ident [ formalParam ] ";"
  // functionDecl ::= "function" ident [ formalParam] ":" type ";"
  
  CToken procedureName;
  CAstProcedure* proc;
  EToken tt = _scanner->Peek().GetType();
  if(tt == tProcedure) {
    Consume(tProcedure);
    Consume(tIdent, &procedureName);
    proc = new CAstProcedure(procedureName, procedureName.GetValue(), 
                             s, new CSymProc(procedureName.GetValue(), CTypeManager::Get()->GetNull())); 
    if(_scanner->Peek().GetType() == tLBrak) formalParam(proc);
    Consume(tSemicolon);
  } else if(tt == tFunction) {
    Consume(tFunction);
    Consume(tIdent, &procedureName);
    proc = new CAstProcedure(procedureName, procedureName.GetValue(),
                             s, new CSymProc(procedureName.GetValue(), CTypeManager::Get()->GetNull())); // null for temporary
    if(_scanner->Peek().GetType() == tLBrak) formalParam(proc);
    Consume(tColon);
    const CType* typ = type();
    Consume(tSemicolon);
    // TODO !!!! set return type of proc to be typ
  } else {
    Consume(tProcedure); // intention is to make error. this kind of indirect code should be avoided i think... fix later
  }

  CAstStatement* stat = subroutineBody(proc);
  CToken endingName;
  Consume(tIdent, &endingName);
  assert(procedureName.GetValue() == endingName.GetValue() && "procedure should end with \"end\" procedurename");
  Consume(tSemicolon);

  proc->SetStatementSequence(stat);
  return proc;
}

CAstStatement* CParser::subroutineBody(CAstScope *s) {
  // subroutineBody ::= varDeclaration "begin" statSequence "end"
  varDeclaration(s);
  Consume(tBegin);
  CAstStatement *stat = statSequence(s);
  Consume(tEnd);
  return stat;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // old statement ::= assignment.
  // statement ::= assignment | subroutineCall | ifStatement | whileStatement | returnStatement
  // FIRST(statSequence) = { ident, ident, if, while, return }
  // FOLLOW(statSequence) = { end, else, end }
  //
  // assignment ::= ident { "[" expression "]" } ":=" expression
  // subroutineCall ::= ident "(" [ expression { "," expression } ] ")"
  // ifStatement ::= "if" "(" expression ")" "then" statSequence [ "else" statSequence ] "end"
  // whileStatement ::= "while" "(" expression ")" "do" statSequence "end"
  // returnStatement ::= "return" [ expression ]
  //
  // FIRST(expression) = { "+", "-", ident, number, boolean, char, string, "(", ident, "!" }
  
  if(_scanner->Peek().GetType() == tEnd ||
     _scanner->Peek().GetType() == tElse) return NULL;

  CAstStatement *stat = NULL;
  CAstStatement *tail = NULL;

  EToken tt = _scanner->Peek().GetType();
  if(tt == tEnd || tt == tElse) return stat;

  while(true) {
    CAstStatement *temp;
    if(tt == tIf) {
      CToken iftoken;
      Consume(tIf, &iftoken);
      Consume(tLBrak);
      CAstExpression* cond = expression(s);
      Consume(tRBrak);
      Consume(tThen);
      CAstStatement *body = statSequence(s);
      CAstStatement *elsebody = NULL;

      if(_scanner->Peek().GetType() == tElse) {
        Consume(tElse);
        elsebody = statSequence(s);
      }

      Consume(tEnd);

      temp = new CAstStatIf(iftoken, cond, body, elsebody); 
    } else if(tt == tWhile) {
      CToken whiletoken;
      Consume(tWhile, &whiletoken);
      Consume(tLBrak);
      CAstExpression* cond = expression(s);
      Consume(tRBrak);
      Consume(tDo);
      CAstStatement* body = statSequence(s);
      Consume(tEnd);

      temp = new CAstStatWhile(whiletoken, cond, body);
    } else if(tt == tReturn) {
      // FOLLOW(returnStatement) = { ";" } + FOLLOW(statSequence) ?
      CToken returntoken;
      Consume(tReturn, &returntoken);

      CAstExpression* returnexpr = NULL;
      if(true) { // TODO : when to read expression?
        returnexpr = expression(s);
      }

      temp = new CAstStatReturn(returntoken, s, returnexpr);
    } else if(tt == tIdent) {
      CToken id;
      Consume(tIdent, &id);
      EToken ttt = _scanner->Peek().GetType();
      if(ttt == tLSqrBrak || ttt == tAssign) {
        // assignment
        
        // read qualident ...
        if(_scanner->Peek().GetType() == tLSqrBrak) {
          assert(false && "qualident with array access not implemented yet");
        }

        const CSymbol* sym = s->GetSymbolTable()->FindSymbol(id.GetValue()); // TODO : scope?
        
        Consume(tAssign);

        CAstConstant* lhs = new CAstConstant(id, sym->GetDataType(), sym->GetOffset()); // TODO : GetOffset right?
        CAstExpression* rhs = expression(s);

        temp = new CAstStatAssign(id, lhs, rhs);
      } else if(ttt == tLBrak) {
        // subroutineCall
        
        temp = new CAstStatCall(id, subroutineCall(s, id));
      } else {
        SetError(_scanner->Peek(), "assignment or subroutineCall expected."); // TODO: not exactly the right error token. we ate the id.
      }
    } else {
      SetError(_scanner->Peek(), "statement expected."); // same error msg as Egger's
    }
    
    if(!stat) {
      stat = tail = temp;
    } else {
      tail->SetNext(temp);
      tail = temp;
    }

    if(_scanner->Peek().GetType() == tSemicolon) Consume(tSemicolon);
    else return stat;
  }

}

CAstFunctionCall* CParser::subroutineCall(CAstScope* s, CToken id) {
  // subroutineCall ::= ident "(" [ expression { "," expression } ] ")"
  // ident is already read and given to us
 
  const CSymbol* sym = s->GetSymbolTable()->FindSymbol(id.GetValue(), sLocal);
  if(!sym) sym = s->GetSymbolTable()->FindSymbol(id.GetValue(), sGlobal);
  const CSymProc *symproc = dynamic_cast<const CSymProc*>(sym);
  
  CAstFunctionCall* func = new CAstFunctionCall(id, symproc); // TODO: NULL has to be some const CSymProc*
  
  Consume(tLBrak);
  while(true) {
    CAstExpression* expr = expression(s);
    func->AddArg(expr);
    if(_scanner->Peek().GetType() == tComma) Consume(tComma);
    else break;
  }
  Consume(tRBrak);
  
  return func;
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpression ].
  // simpleexpr ::= ["+"|"-"] term {termOp term}
  // term ::= factor {factOp factor}
  // factor ::= qualident | number | boolean | char | string | "(" expression ")" | subroutineCall | "!" factor
  // qualident ::= ident { "[" expression "]" }
  // subroutineCall ::= ident "(" [ expression {"," expression} ] ")"
  //
  // FIRST(expression) = { "+", "-", ident, number, boolean, char, string, "(", ident, "!" }
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= term { termOp term }.
  //
  // simpleexpr ::= ["+"|"-"] term {termOp term}
  // term ::= factor {factOp factor}
  // factor ::= qualident | number | boolean | char | string | "(" expression ")" | subroutineCall | "!" factor
  // qualident ::= ident { "[" expression "]" }
  // subroutineCall ::= ident "(" [ expression {"," expression} ] ")"



  bool unary = false;
  EOperation unaryOp;
  CToken unaryToken;
  if(_scanner->Peek().GetType() == tTermOp && _scanner->Peek().GetValue() != "||") { // plus minus
    Consume(tTermOp, &unaryToken);
    if(unaryToken.GetValue() == "+") unaryOp = opPos;
    else if(unaryToken.GetValue() == "-") unaryOp = opNeg; // scanner guarantees that these two are the only case
    unary = true;
  }


  CAstExpression *n = NULL;

  n = term(s);

  while (_scanner->Peek().GetType() == tTermOp) {
    CToken t;
    CAstExpression *l = n, *r;
    EOperation op;

    Consume(tTermOp, &t);

    r = term(s);

    if(t.GetValue() == "+") op = opAdd;
    else if(t.GetValue() == "-") op = opSub;
    else op = opOr;

    n = new CAstBinaryOp(t, op, l, r);
  }

  if(unary) return new CAstUnaryOp(unaryToken, unaryOp, n);
  else return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/") factor }.
  //
  // term ::= factor { factOp factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while (tt == tFactOp) {
    CToken t;
    CAstExpression *l = n, *r;
    EOperation op;

    Consume(tFactOp, &t);

    r = factor(s);

    if(t.GetValue() == "*") op = opMul;
    else if(t.GetValue() == "/") op = opDiv;
    else op = opAnd;

    n = new CAstBinaryOp(t, op, l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= number | "(" expression ")"
  //
  // FIRST(factor) = { tNumber, tLBrak }
  //
  // factor ::= qualident | number | boolean | char | string | "(" expression ")" | subroutineCall | "!" factor
  // FIRST(factor) = { ident, number, boolean, char, string, "(", ident, "!" }
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *n = NULL;

  if(tt == tNumber) {
    n = number();
  } else if(tt == tBoolean) {
    // TODO
  } else if(tt == tCharacter) {
    // TODO
    Consume(tCharacter, &t);
    n = new CAstConstant(t, CTypeManager::Get()->GetChar(), (int)t.GetValue()[0]);
  } else if(tt == tString) {
    // TODO
  } else if(tt == tLBrak) {
    Consume(tLBrak);
    n = expression(s);
    Consume(tRBrak);
  } else if(tt == tNot) {
    CToken nottoken;
    Consume(tNot, &nottoken);
    n = factor(s);
    n = new CAstUnaryOp(nottoken, opNot, n);
  } else if(tt == tIdent) {
    // TODO. qualident or subroutineCall
  }

  return n;
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

