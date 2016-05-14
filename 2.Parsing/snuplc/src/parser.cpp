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
      if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
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

// initialize symbol table with predefined functions
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
        // because in SnuPL/1, complex objects are referenced when passed to a function as an argument
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
      if(!isArgument) {
        // if this is not an argument, declaration of open sized arrays are not allowed.
        Consume(tNumber); // consume number to cause error
      }
      int open = CArrayType::OPEN;
      lengths.push_back(open);
      Consume(tRSqrBrak);
    }
  }

  return t;
}

// parses varDecl and saves them in symbol table of scope s
// isGlobal is optionally given to indicate whether the variables are declared globally
// symproc is optionally given when we should also store the variables as parameters of a procedure/function
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
// isGlobal is optionally given to indicate whether the variables are declared globally
// symproc is optionally given when we should also store the variables as parameters of a procedure/function
// fromFormalParam indicates whether this was called while parsing formalParam 
void CParser::varDeclSequence(CAstScope* s, bool isGlobal, CSymProc *symproc, bool fromFormalParam) {
  // varDeclSequence ::= varDecl { ";" varDecl }

  while(true) { // reads varDecl until the next thing doesn't appear to be varDecl
    varDecl(s, isGlobal, symproc);
    if(!fromFormalParam) {
      Consume(tSemicolon);
      // problem : FOLLOW(varDeclSequence) is also ";". how do we know when to end?
      // solution : read one more. 
      // If next token is tIdent, we need to go to varDecl. 
      // Else we end because tIdent is not one of FOLLOW(varDeclaration) = {"begin", "procedure", "function"}
      if(_scanner->Peek().GetType() != tIdent ) return; // in this case, the semicolon we've eaten belongs to varDeclaration
    } else { // called from formalParam. in this case ";" always belongs to { ";" varDecl }
      if(_scanner->Peek().GetType() != tSemicolon) return;
      Consume(tSemicolon);
    }
  }
}

// parses varDeclaration and saves them in symbol table of scope s
// isGlobal indicates whether the variables are declared globally
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

// parses module and returns CAstModule* for it
CAstModule* CParser::module(void)
{
  // module ::= "module" ident ";"' varDeclaration { subroutineDecl } "begin" statSequence "end" ident "."

  CToken moduleToken;
  Consume(tModule, &moduleToken);
  CToken moduleName;
  Consume(tIdent, &moduleName);

  CAstModule *m = new CAstModule(moduleToken, moduleName.GetValue());

  // initialize symbol table for module
  InitSymbolTable(m->GetSymbolTable());

  Consume(tSemicolon);

  // call varDeclaration. isGlobal=true because declarations are happening in module
  varDeclaration(m, true);

  while(true) {
    EToken tt = _scanner->Peek().GetType();
    // parse subroutine declarations until procedureDecl/functionDecl doesn't appear
    if(tt != tProcedure && tt != tFunction) break;
    // the new subroutine automatically adds itself as child of m
    CAstProcedure* subroutine = subroutineDecl(m);
  }

  Consume(tBegin);

  CAstStatement *statseq = statSequence(m);

  Consume(tEnd);

  CToken endingName;
  Consume(tIdent, &endingName);

  // if module name doesn't match the ending name, it is an error
  if(moduleName.GetValue() != endingName.GetValue())
    SetError(endingName, "module identifier mismatch ('" + moduleName.GetValue() + "' != '" + endingName.GetValue() + "').");

  Consume(tDot);

  // set module's statement sequence
  m->SetStatementSequence(statseq);

  return m;
}

// parses formalParam of procedure/function
// s is the procedure/function scope in which we should save symbols of parameters
// symproc is the CSymProc of the procedure/function in which we should add parameter types
void CParser::formalParam(CAstScope *s, CSymProc *symproc) {
  // formalParam ::= "(" [ varDeclSequence ] ")"

  Consume(tLBrak);
  // if varDeclSequence appears, we will be able to see tIdent appear
  if(_scanner->Peek().GetType() == tIdent)
    varDeclSequence(s, false, symproc, true); // isGlobal=false because parameters are local
                                              // fromFormalParam=true because it is
  Consume(tRBrak);
}

// parses subroutineDecl and returns CAstProcedure* for it
// s is the module scope where this subroutine belongs
CAstProcedure* CParser::subroutineDecl(CAstScope *s) {
  // subroutineDecl ::= (procedureDecl | functionDecl) subroutineBody ident ";"
  // procedureDecl ::= "procedure" ident [ formalParam ] ";"
  // functionDecl ::= "function" ident [ formalParam] ":" type ";"
  
  CToken procedureName;
  CAstProcedure* proc;
  CSymProc *symproc;
  EToken tt = _scanner->Peek().GetType();
  if(tt == tProcedure) { // procedureDecl
    Consume(tProcedure);
    Consume(tIdent, &procedureName); // get procedure name

    // if procedure name already exists as global symbol, it is duplicate declaration
    if(s->GetSymbolTable()->FindSymbol(procedureName.GetValue(), sGlobal) != NULL)
      SetError(procedureName, "duplicate procedure/function declaration '" + procedureName.GetValue() + "'.");

    // initialize signature of procedure to be () -> NULL
    symproc = new CSymProc(procedureName.GetValue(), CTypeManager::Get()->GetNull());

    // make procedure AST. it automatically adds itself as child scope to the module
    proc = new CAstProcedure(procedureName, procedureName.GetValue(), s, symproc);

    // if we see "(" which is in FIRST(formalParam)={"("}, we call formalParam()
    // formalParam will also update the parameters of procedure signature (symproc) for us
    if(_scanner->Peek().GetType() == tLBrak) formalParam(proc, symproc);

    Consume(tSemicolon);
  } else if(tt == tFunction) { // functionDecl
    Consume(tFunction);
    Consume(tIdent, &procedureName); // get function name

    // if function name already exists as global symbol, it is duplicate declaration
    if(s->GetSymbolTable()->FindSymbol(procedureName.GetValue(), sGlobal) != NULL)
      SetError(procedureName, "duplicate procedure/function declaration '" + procedureName.GetValue() + "'.");

    // initialize signature of function to be () -> NULL
    symproc = new CSymProc(procedureName.GetValue(), CTypeManager::Get()->GetNull());

    // make procedure AST. it automatically adds itself as child scope to the module
    proc = new CAstProcedure(procedureName, procedureName.GetValue(), s, symproc);

    // if we see "(" which is in FIRST(formalParam)={"("}, we call formalParam()
    // formalParam will also update the parameters of function signature (symproc) for us
    if(_scanner->Peek().GetType() == tLBrak) formalParam(proc, symproc);

    Consume(tColon);

    const CType* typ = type(false); // read the return type. isArgument=false because it isn't an argument 
    symproc->SetDataType(typ); // adjust the return type of the function

    Consume(tSemicolon);
  } else {
    Consume(tProcedure); // intention is to make error
    // currently, it is guaranteed the control flow doesn't reach this point because
    // module() only calls subroutineDecl() when it sees "procedure" or "function"
  }

  // register procedure/function symbol in module
  s->GetSymbolTable()->AddSymbol(symproc);

  // read body of the procedure/function
  CAstStatement* stat = subroutineBody(proc);

  CToken endingName;
  Consume(tIdent, &endingName); // get ending name

  // if procedure name doesn't match the ending name, it is an error
  if(procedureName.GetValue() != endingName.GetValue()) {
    SetError(endingName, "procedure/function identifier mismatch ('" + procedureName.GetValue() + "' != '" + endingName.GetValue() + "').");
  }

  Consume(tSemicolon);

  // set the statement sequence of the procedure
  proc->SetStatementSequence(stat);
  return proc;
}

// parses subroutineBody and returns CAstStatement* for it
// s is the scope of the procedure where this body belongs
CAstStatement* CParser::subroutineBody(CAstScope *s) {
  // subroutineBody ::= varDeclaration "begin" statSequence "end"

  varDeclaration(s);
  Consume(tBegin);
  CAstStatement *stat = statSequence(s);
  Consume(tEnd);
  return stat;
}

// parses statSequence and returns CAstStatement* for it
// s is the scope where statements belong
CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall | ifStatement | whileStatement | returnStatement
  // FIRST(statSequence) = { ident, if, while, return }
  // FOLLOW(statSequence) = { end, else }
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

  CAstStatement *stat = NULL; // head of the linked list of statements. eventually return this
  CAstStatement *tail = NULL; // tail of the linked list of statements

  EToken tt = _scanner->Peek().GetType();
  // if FOLLOW(statSequence) appears, stop
  if(tt == tEnd || tt == tElse) return stat;

  while(true) {
    CAstStatement *temp; // to save the next statement
    if(tt == tIf) { // ifStatement
      CToken iftoken;
      Consume(tIf, &iftoken);
      Consume(tLBrak);

      // read if-condition expression
      CAstExpression* cond = expression(s);

      Consume(tRBrak);

      Consume(tThen);
      // reads if-then statement
      CAstStatement *body = statSequence(s); // then body
      CAstStatement *elsebody = NULL; // else body

      // if "else" appears, we read if-else statement
      if(_scanner->Peek().GetType() == tElse) {
        Consume(tElse);
        elsebody = statSequence(s);
      }

      Consume(tEnd);

      temp = new CAstStatIf(iftoken, cond, body, elsebody); // make if AST
    } else if(tt == tWhile) { // whileStatement
      CToken whiletoken;
      Consume(tWhile, &whiletoken);
      Consume(tLBrak);

      // read while-condition expression
      CAstExpression* cond = expression(s);

      Consume(tRBrak);

      Consume(tDo);
      // read while-do statement
      CAstStatement* body = statSequence(s);

      Consume(tEnd);

      temp = new CAstStatWhile(whiletoken, cond, body); // make while AST
    } else if(tt == tReturn) { //returnStatement
      // FOLLOW(returnStatement) = { ";" } + FOLLOW(statSequence)
      CToken returntoken;
      Consume(tReturn, &returntoken);

      CAstExpression* returnexpr = NULL;
      EToken ttt = _scanner->Peek().GetType();
      // reads return expression if we don't see FOLLOW(returnStatement)
      if(!(ttt == tSemicolon || ttt == tEnd || ttt == tElse)) {
        returnexpr = expression(s);
      }

      temp = new CAstStatReturn(returntoken, s, returnexpr); // make return AST
    } else if(tt == tIdent) {
      CToken id;
      Consume(tIdent, &id);
      EToken ttt = _scanner->Peek().GetType();
      if(ttt == tLSqrBrak || ttt == tAssign) { // assignment
        
        // read qualident (lhs)
        CAstExpression* lhs = qualident(s, id); 

        Consume(tAssign);

        // read rhs expression
        CAstExpression* rhs = expression(s);

        temp = new CAstStatAssign(id, lhs, rhs); // make assign AST
      } else if(ttt == tLBrak) { // subroutineCall
        
        temp = new CAstStatCall(id, subroutineCall(s, id)); // make call AST
      } else {
        SetError(_scanner->Peek(), "assignment or subroutineCall expected."); // TODO: not exactly the right error token. we ate the id.
      }
    } else {
      SetError(_scanner->Peek(), "statement expected."); // generate error because none of the statements matched
    }
    
    if(!stat) { // init statement linked list
      stat = tail = temp;
    } else { // append to the statement linked list
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

// parses subroutineCall and returns CAstFunctionCall* for it
// s is the scope where the call belongs
// id is the ident token which is the first token of subroutineCall. it is already consumed and passed to us
CAstFunctionCall* CParser::subroutineCall(CAstScope* s, CToken id) {
  // subroutineCall ::= ident "(" [ expression { "," expression } ] ")"
 
  // get the function symbol from the symbol table
  const CSymbol* sym = s->GetSymbolTable()->FindSymbol(id.GetValue(), sGlobal);
  // if no such symbol exists, the subroutine is not declared yet, so it is an error
  if(!sym) SetError(id, "undefined identifier.");
  const CSymProc *symproc = dynamic_cast<const CSymProc*>(sym);
  // if symbol is not CSymProc, it is not a procedure. Therefore it is invalid identifier of a procedure
  if(!symproc) SetError(id, "invalid procedure/function identifier.");
  
  CAstFunctionCall* func = new CAstFunctionCall(id, symproc);
  
  int index = 0; // index of the expression as a parameter to the procedure
  Consume(tLBrak);
  while(_scanner->Peek().GetType() != tRBrak) {
    CAstExpression* expr = expression(s);
    // if we pass an array, we have to instead pass a pointer to that array
    if(expr->GetType()->IsArray()) expr = new CAstSpecialOp(expr->GetToken(), opAddress, expr, NULL);

    // adds the argument
    func->AddArg(expr);

    index++;
    if(_scanner->Peek().GetType() == tComma) Consume(tComma);
    else break;
  }
  Consume(tRBrak);
  
  return func;
}

// parses expression and returns CAstExpression* for it
// s is scope where the expression belongs
CAstExpression* CParser::expression(CAstScope* s)
{
  // expression ::= simpleexpr [ relOp simpleexpr ].
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

  left = simpleexpr(s); // read the first simpleexpr

  if (_scanner->Peek().GetType() == tRelOp) { // simpleexpr relOp simpleexpr
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
  } else { // simpleexpr
    return left;
  }
}

// parses simpleexpr and returns CAstExpression* for it
// s is scope where the simpleexpr belongs
CAstExpression* CParser::simpleexpr(CAstScope *s)
{ 
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
  CAstConstant *constant = dynamic_cast<CAstConstant*>(n);
  if(unary) {
    if(constant != NULL && constant->GetType()->IsInt()) {
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

// parses term and returns CAstExpression* for it
// s is scope where the term belongs
CAstExpression* CParser::term(CAstScope *s)
{
  DEBUG(cout << "term on " << _scanner->Peek() << endl;)
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

// parses qualident and returns CAstExpression* for it
// id is the ident token which is the first token of qualident. it is already consumed and passed to us
CAstExpression* CParser::qualident(CAstScope *s, CToken id) {
  // qualident ::= ident { "[" expression "]" }
   
  const CSymbol* sym = s->GetSymbolTable()->FindSymbol(id.GetValue(), sGlobal);
  if(!sym) SetError(id, "undefined identifier.");

  CAstExpression* n;
  EToken ttt = _scanner->Peek().GetType();
  if(ttt != tLSqrBrak) {
    n = new CAstDesignator(id, sym);
  } else {
    CAstArrayDesignator* var = new CAstArrayDesignator(id, sym);
    while(_scanner->Peek().GetType() == tLSqrBrak) {
      Consume(tLSqrBrak);
      CAstExpression* expr = expression(s);
      var->AddIndex(expr);
      Consume(tRSqrBrak);
    }
    var->IndicesComplete();
    n = var;
  }

  return n;
}

// parses factor and returns CAstExpression* for it
// s is scope where the factor belongs
CAstExpression* CParser::factor(CAstScope *s)
{
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
    string str = _scanner->unescape(t.GetValue());
    int val;
    if(str.size() == 0) val = 0;
    else val = (int)(str[0]);
    n = new CAstConstant(t, CTypeManager::Get()->GetChar(), val);
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

// parses a number and returns CAstConstant* for it
CAstConstant* CParser::number(void)
{
  // number ::= digit { digit }.

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

// parses a boolean and returns CAstConstant* for it
CAstConstant* CParser::boolean(void)
{
  // boolean ::= true | false
  
  CToken t;
  Consume(tBoolean, &t);

  long long v = 0;
  if(t.GetValue() == "true") v = 1;

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), v);
}


// parses a string and returns CAstConstant* for it
CAstStringConstant* CParser::stringConstant(CAstScope* s)
{
  // string ::=
  
  CToken t;
  Consume(tString, &t);

  return new CAstStringConstant(t, t.GetValue(), s);
}


