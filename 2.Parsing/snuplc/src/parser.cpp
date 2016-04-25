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

#define DEBUG(s)


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
  DEBUG(cout << "consume " << CToken::Name(type) << endl;)
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  DEBUG(cout << "before consume returns" << endl;)
  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  CSymProc *DIM, *DOFS, *ReadInt, *WriteChar, *WriteInt, *WriteLn, *WriteStr;

  DIM = new CSymProc("DIM", tm->GetInt());
  DIM->AddParam(new CSymParam(0, "arg0", tm->GetPointer(tm->GetNull())));
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
  WriteStr->AddParam(new CSymParam(0, "s", tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar())))); 

  s->AddSymbol(DIM);
  s->AddSymbol(DOFS);
  s->AddSymbol(ReadInt);
  s->AddSymbol(WriteChar);
  s->AddSymbol(WriteInt);
  s->AddSymbol(WriteLn);
  s->AddSymbol(WriteStr);
}

// parses type and returns CType*
// isArgument indicates whether the type is for argument of a procedure/function or not
const CType* CParser::type(bool isArgument) {
  // type ::= basetype | type "[" [ number ] "]"
  // basetype ::= "boolean" | "char" | "integer"

  const CType* t;
  CToken basetype;
  Consume(tBaseType, &basetype);
  if(basetype.GetValue() == "boolean") t = CTypeManager::Get()->GetBool();
  else if(basetype.GetValue() == "char") t = CTypeManager::Get()->GetChar();
  else t = CTypeManager::Get()->GetInt(); // ensured by my scanner design

  // lengths = vector for storing array lengths of a type.
  // ex) if type = integer[1][][3], then lengths = <1, OPEN, 3>
  vector<int> lengths;
  while(true) {
    if(_scanner->Peek().GetType() != tLSqrBrak) {
      // done reading all lengths. now construct the type and return it
      vector<int>::reverse_iterator rit;
      for(rit = lengths.rbegin(); rit != lengths.rend(); rit++) {
        t = CTypeManager::Get()->GetArray(*rit, t); // recursively nest types
      }

      if(isArgument &&  t->IsArray()) {
        // if it's for procedure/function argument and it is an array type, it should be a pointer to that type
        // because in SnuPL/1, complex objects are dereferenced when passed to a function as an argument
        t = CTypeManager::Get()->GetPointer(t);
      }
      break; // break to return
    }
    Consume(tLSqrBrak);
    if(_scanner->Peek().GetType() != tRSqrBrak) {
      // "[" is not immediately closed by "]", so we should expect a number
      CAstConstant* num = number();
      // number() guarantees that it will generate appropriate error when it reads a non-number token
      // array length = num
      lengths.push_back(num->GetValue());
      Consume(tRSqrBrak);
    } else {
      // "[" is closed immediately by "]", so we assume it is OPEN length
      // array length = OPEN
      int open = CArrayType::OPEN;
      lengths.push_back(open);
      Consume(tRSqrBrak);
    }
  }

  return t;
}

// parses varDecl and saves them in symbol table of scope s
void CParser::varDecl(CAstScope* s, bool isGlobal, CSymProc* symproc) {
  // varDecl ::= ident { "," ident } ":" type

  // vars = vector for storing all given idents
  vector<CToken> vars;
  while(true) { // reads idents (and commas inbetween) until ":" appears
    CToken var;
    Consume(tIdent, &var);
    vars.push_back(var); // saves the ident
    if(_scanner->Peek().GetType() == tColon) break;
    Consume(tComma);
  }
  Consume(tColon);

  // if symproc is not NULL, the caller expects these variables to be a parameter of a procedure/function
  // so we pass isArgument=true to type(). otherwise we pass false
  const CType* typ = type(symproc != NULL);

  vector<CToken>::iterator iter;
  int index = 0; // index for creating a parameter symbol
  if(symproc) index = symproc->GetNParams(); // if n params exist, we start indexing from n
  for(iter = vars.begin(); iter != vars.end(); iter++) {
    // if the scope has a local variable with the same name, it is already an error (duplicate)
    if(s->GetSymbolTable()->FindSymbol(iter->GetValue(), sLocal) != NULL)
      SetError(*iter, "duplicate variable declaration '" + iter->GetValue() + "'.");
    
    CSymbol *sym;
    if(isGlobal) { // symbol is global
      // if global variable with the same name exists, it is a duplicate declaration
      if(s->GetSymbolTable()->FindSymbol(iter->GetValue(), sGlobal) != NULL)
        SetError(*iter, "duplicate variable declaration '" + iter->GetValue() + "'.");
      sym = new CSymGlobal(iter->GetValue(), typ);
    } else if(symproc)  { // symbol is param
      sym = new CSymParam(index, iter->GetValue(), typ);
    } else { //symbol is local
      sym = new CSymLocal(iter->GetValue(), typ);
    }
    
    s->GetSymbolTable()->AddSymbol(sym);
    if(symproc) {
      // if symproc is not null, the caller expects these variables to be added as a parameter to that procedure
      symproc->AddParam(new CSymParam(index, iter->GetValue(), typ));
    }
    index++;
  }
}

// parses varDeclSequence and saves them in symbol table of scope s
void CParser::varDeclSequence(CAstScope* s, bool isGlobal, CSymProc *symproc) {
  // varDeclSequence ::= varDecl { ";" varDecl }

  while(true) { // reads varDecl until the next thing doesn't appear to be varDecl
    varDecl(s, isGlobal, symproc);
    if(_scanner->Peek().GetType() != tSemicolon) return;
    Consume(tSemicolon);
    // problem : FOLLOW(varDeclSequence) is also ";". how do we know when to end?
    // solution : read one more. 
    // If next token is tIdent, we need to go to varDecl. 
    // Else we end because tIdent is not one of FOLLOW(varDeclaration) = {"begin", "procedure", "function"}
    if(_scanner->Peek().GetType() != tIdent ) return; // in this case, the semicolon we've eaten belongs to varDeclaration
  }
}

// parses varDeclaration and saves them in symbol table of scope s
void CParser::varDeclaration(CAstScope* s, bool isGlobal) {
  // varDeclaration ::= [ "var" varDeclSequence ";" ]

  // tVar is not one of FOLLOW(varDeclaration) = {"begin", "procedure", "function"}
  // so if we see "var", we assume varDeclaration (the whole thing) appears
  // otherwise, we assume it doesn't
  if(_scanner->Peek().GetType() != tVar) return;

  Consume(tVar);
  varDeclSequence(s, isGlobal);
  //Consume(tSemicolon);  don't eat ";" because varDeclSequence() eats it
}

CAstModule* CParser::module(void)
{
  //
  // old module ::= statSequence  ".".
  // module ::= "module" ident ";"' varDeclaration { subroutineDecl } "begin" statSequence "end" ident "."
  //
  CToken moduleToken;
  Consume(tModule, &moduleToken);
  CToken moduleName;
  Consume(tIdent, &moduleName);

  // TODO. is it right to put moduleName for 1st parameter?
  CAstModule *m = new CAstModule(moduleToken, moduleName.GetValue());

  InitSymbolTable(m->GetSymbolTable());

  Consume(tSemicolon);
  varDeclaration(m, true);

  while(true) {
    EToken tt = _scanner->Peek().GetType();
    if(tt != tProcedure && tt != tFunction) break;
    CAstProcedure* subroutine = subroutineDecl(m);
    //m->AddProcedure(subroutine);
  }

  Consume(tBegin);

  CAstStatement *statseq = NULL;
  statseq = statSequence(m);

  Consume(tEnd);

  CToken endingName;
  Consume(tIdent, &endingName);
  if(moduleName.GetValue() != endingName.GetValue())
    SetError(endingName, "module identifier mismatch ('" + moduleName.GetValue() + "' != '" + endingName.GetValue() + "').");

  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}

void CParser::formalParam(CAstScope *s, CSymProc *symproc) {
  // formalParam ::= "(" [ varDeclSequence ] ")"
  Consume(tLBrak);
  if(_scanner->Peek().GetType() == tIdent)
    varDeclSequence(s, false, symproc);
  Consume(tRBrak);
}

CAstProcedure* CParser::subroutineDecl(CAstScope *s) {
  // subroutineDecl ::= (procedureDecl | functionDecl) subroutineBody ident ";"
  // procedureDecl ::= "procedure" ident [ formalParam ] ";"
  // functionDecl ::= "function" ident [ formalParam] ":" type ";"
  
  CToken procedureName;
  CAstProcedure* proc;
  CSymProc *symproc;
  EToken tt = _scanner->Peek().GetType();
  if(tt == tProcedure) {
    Consume(tProcedure);
    Consume(tIdent, &procedureName);
    if(s->GetSymbolTable()->FindSymbol(procedureName.GetValue(), sGlobal) != NULL)
      SetError(procedureName, "duplicate procedure/function declaration '" + procedureName.GetValue() + "'.");
    symproc = new CSymProc(procedureName.GetValue(), CTypeManager::Get()->GetNull());
    proc = new CAstProcedure(procedureName, procedureName.GetValue(), s, symproc);
    if(_scanner->Peek().GetType() == tLBrak) formalParam(proc, symproc);
    Consume(tSemicolon);
  } else if(tt == tFunction) {
    Consume(tFunction);
    Consume(tIdent, &procedureName);
    if(s->GetSymbolTable()->FindSymbol(procedureName.GetValue(), sGlobal) != NULL)
      SetError(procedureName, "duplicate procedure/function declaration '" + procedureName.GetValue() + "'.");
    symproc = new CSymProc(procedureName.GetValue(), CTypeManager::Get()->GetNull());
    proc = new CAstProcedure(procedureName, procedureName.GetValue(), s, symproc);
    if(_scanner->Peek().GetType() == tLBrak) formalParam(proc, symproc);
    Consume(tColon);
    const CType* typ = type(true);
    symproc->SetDataType(typ);
    Consume(tSemicolon);
  } else {
    Consume(tProcedure); // intention is to make error. this kind of indirect code should be avoided i think... fix later
  }

  s->GetSymbolTable()->AddSymbol(symproc);

  CAstStatement* stat = subroutineBody(proc);
  CToken endingName;
  Consume(tIdent, &endingName);
  if(procedureName.GetValue() != endingName.GetValue()) {
    SetError(endingName, "procedure/function identifier mismatch ('" + procedureName.GetValue() + "' != '" + endingName.GetValue() + "').");
  }
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
      EToken ttt = _scanner->Peek().GetType();
      if(!(ttt == tSemicolon || ttt == tEnd || ttt == tElse)) { // TODO : correct condition?
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
        CAstExpression* lhs = qualident(s, id); 

        Consume(tAssign);

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

    if(_scanner->Peek().GetType() == tSemicolon) {
      Consume(tSemicolon);
      tt = _scanner->Peek().GetType();
    }
    else return stat;
  }
}

CAstFunctionCall* CParser::subroutineCall(CAstScope* s, CToken id) {
  // subroutineCall ::= ident "(" [ expression { "," expression } ] ")"
  // ident is already read and given to us
 
  const CSymbol* sym = s->GetSymbolTable()->FindSymbol(id.GetValue(), sGlobal);
  if(!sym) SetError(id, "undefined identifier.");
  const CSymProc *symproc = dynamic_cast<const CSymProc*>(sym);
  if(!symproc) SetError(id, "invalid procedure/function identifier.");
  
  CAstFunctionCall* func = new CAstFunctionCall(id, symproc); // TODO: NULL has to be some const CSymProc*
  
  int index = 0;
  Consume(tLBrak);
  while(_scanner->Peek().GetType() != tRBrak) {
    CAstExpression* expr = expression(s);
    if(expr->GetType()->IsArray()) expr = new CAstSpecialOp(expr->GetToken(), opAddress, expr, NULL);

    func->AddArg(expr);
    
    const CSymParam* param = symproc->GetParam(index);
    if(!expr->GetType()->Match(param->GetDataType()))
      //SetError(expr->GetToken(), "argument type mismatch");
    index++;
    if(_scanner->Peek().GetType() == tComma) Consume(tComma);
    else break;
  }
  Consume(tRBrak);
  
  return func;
}

CAstExpression* CParser::expression(CAstScope* s)
{
  DEBUG(cout << "expression on " << _scanner->Peek() << endl;)
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
  DEBUG(cout << "after left = simpleexpr() in expression()" << endl;)

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == "<=") relop = opLessEqual;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == ">=") relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    DEBUG(cout << "right before expression returns" << endl;)
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{ 
  DEBUG(cout << "simpleexpr on " << _scanner->Peek() << endl;)
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
  DEBUG(cout << "after term returns to simplexpr" << endl;)
  if(unary) {
    if(CAstConstant *constant = dynamic_cast<CAstConstant*>(n)) {
      if(unaryOp == opNeg) {
        constant->SetValue(-constant->GetValue());
      }
      n = constant;
    } else {
      n = new CAstUnaryOp(unaryToken, unaryOp, n);
    }
  }

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

  DEBUG(cout << "right before simpleexpr returns" << endl;)
  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  DEBUG(cout << "term on " << _scanner->Peek() << endl;)
  //
  // term ::= factor { ("*"|"/") factor }.
  //
  // term ::= factor { factOp factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);
  DEBUG(cout << "after factor returns to term" << endl;)

  EToken tt = _scanner->Peek().GetType();

  while (tt == tFactOp) {
    DEBUG(cout << "found factop in term" << endl;)
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

CAstExpression* CParser::qualident(CAstScope *s, CToken id) {
  DEBUG(cout << "qualident on " << _scanner->Peek() << endl;)
  // qualident ::= ident { "[" expression "]" }
  // ident is already read and given as id
   
  // later, i might replace below 2 lines with 1 line with just stGlobal as scope
  const CSymbol* sym = s->GetSymbolTable()->FindSymbol(id.GetValue(), sGlobal);
  if(!sym) SetError(id, "undefined identifier.");

  CAstExpression* n;
  EToken ttt = _scanner->Peek().GetType();
  if(ttt != tLSqrBrak) {
    n = new CAstDesignator(id, sym);
  } else {
    CAstArrayDesignator* var = new CAstArrayDesignator(id, sym);
    //if(var->GetType() == NULL) cout << "var type NULL create" << endl;
    while(_scanner->Peek().GetType() == tLSqrBrak) {
      Consume(tLSqrBrak);
      CAstExpression* expr = expression(s);
      var->AddIndex(expr);
      Consume(tRSqrBrak);
    }
    //if(var->GetType() == NULL) cout << "var type NULL before" << endl;
    var->IndicesComplete();
    //if(var->GetType() == NULL) cout << "var type NULL after" << endl;
    n = var;
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  DEBUG(cout << "factor on " << _scanner->Peek() << endl;)
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
    n = boolean();
  } else if(tt == tCharacter) {
    Consume(tCharacter, &t);
    n = new CAstConstant(t, CTypeManager::Get()->GetChar(), (int)_scanner->unescape(t.GetValue())[0]);
  } else if(tt == tString) {
    n = stringConstant(s);
    DEBUG(cout << "after n = stringConstant(s)" << endl;)
    DEBUG(if(n->GetType() == NULL) cout << "n's type is NULL" << endl;)
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
    CToken id;
    Consume(tIdent, &id);
    EToken ttt = _scanner->Peek().GetType();
    if(ttt == tLBrak) {
      // subroutine call
      n = subroutineCall(s, id);
    } else {
      // qualident
      n = qualident(s, id);
    }
  } else {
    // strange case
    CToken unexpected = _scanner->Peek();
    SetError(unexpected, "factor expected.");
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

CAstConstant* CParser::boolean(void)
{
  // boolean ::= true | false
  
  CToken t;
  Consume(tBoolean, &t);

  long long v = 0;
  if(t.GetValue() == "true") v = 1;

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), v);
}

CAstStringConstant* CParser::stringConstant(CAstScope* s)
{
  // string ::=
  
  CToken t;
  Consume(tString, &t);

//  return new CAstStringConstant(t, _scanner->unescape(t.GetValue()), s);
  return new CAstStringConstant(t, t.GetValue(), s);
}


