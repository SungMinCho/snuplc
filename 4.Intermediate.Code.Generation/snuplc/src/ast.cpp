//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2014/04/08 Bernhard Egger assignment 2: AST for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016 Bernhard Egger
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
#include <cassert>
#include <cstring>

#include <typeinfo>

#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::GetTacAddr(void) const
{
  return _addr;
}

ostream& operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

void CAstScope::AddProcedure(CAstScope* proc)
{
  this->AddChild(proc);
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope* CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;

  try {
    CAstStatement *s = _statseq;
    while(result && (s != NULL)) { // do TypeCheck for all statements under this scope
      result = s->TypeCheck(t, msg);
      s = s->GetNext();
    }

    vector<CAstScope*>::const_iterator it = _children.begin();
    while(result && (it != _children.end())) { // do TypeCheck for all scopes under this scope
      result = (*it)->TypeCheck(t, msg);
      it++;
    }
  } catch(...) {
    result = false;
  }

  return result;
}

ostream& CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent+4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent+4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i=0; i<_children.size(); i++) {
      _children[i]->print(out, indent+4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
  assert (cb != NULL);

  CAstStatement *s = GetStatementSequence();
  while(s != NULL) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    cb->AddInstr(next);
    s = s->GetNext();
  }

  cb->CleanupControlFlow();

  return NULL;
}

CCodeBlock* CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name,
                             CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol)
{
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType(); // return the data type of the procedure symbol
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}


//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const
{
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const
{
  return _next;
}

CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t,
                               CAstExpression *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstExpression* CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const
{
  if(!(_lhs->TypeCheck(t, msg) && _rhs->TypeCheck(t, msg))) {
    return false; // if lhs or rhs doesn't TypeCheck, it is already an error
  }
  if(!_lhs->GetType()->Match(_rhs->GetType())) { // lhs's type should match rhs's type
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "assign to different type";
    return false;
  }
  if(!_lhs->GetType()->IsScalar()) { // we don't support assignment to compound types
    if(t != NULL) *t = GetToken();
    if(msg != NULL) {
      ostringstream ss;
      ss << "assignments to compound types are not supported.";
      ss << "\n  LHS: ";
      _lhs->GetType()->print(ss, 0);
      ss << "\n  RHS: ";
      _rhs->GetType()->print(ss, 0);

      *msg = ss.str();
    }
    return false;
  }
  return true;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType(); // the type of lhs represents the type of this assignment
                          // whether the lhs's type matches rhs's type is checked in TypeCheck()
}

ostream& CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent+2);
  _rhs->print(out, indent+2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  assert(cb != NULL);
  CTacAddr* t = _rhs->ToTac(cb);
  CTacAddr* l = _lhs->ToTac(cb);
  cb->AddInstr(new CTacInstr(opAssign, l, t));
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const
{
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const
{
  return GetCall()->TypeCheck(t, msg); // Do a TypeCheck on the CAstFunctionCall that it's holding
}

ostream& CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  _call->ToTac(cb);
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const
{
  const CType *st = GetScope()->GetType();

  if(st->Match(CTypeManager::Get()->GetNull())) { // return type is null
    if(_expr != NULL) { // if return type is null, it shouldn't return anything
      if(t != NULL) *t = _expr->GetToken();
      if(msg != NULL) *msg = "superfluous expression after return.";
      return false;
    }
    // else, there's nothing to check
  } else { // return type is not null
    if(_expr == NULL) { // if return type is not null, we should return something
      if(t != NULL) *t = GetToken();
      if(msg != NULL) *msg = "expression expected after return.";
      return false;
    }

    // return expression should be TypeChecked
    if(!_expr->TypeCheck(t, msg)) return false;

    if(!st->Match(_expr->GetType())) { // check whether the expression's type matches the return type
      if(t != NULL) *t = _expr->GetToken();
      if(msg != NULL) *msg = "return type mismatch.";
      return false;
    }
  }

  return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (_expr != NULL) {
    t = _expr->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent+2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  if(_expr == NULL) {
    cb->AddInstr(new CTacInstr(opReturn, NULL));
  } else {
    CTacAddr* t = _expr->ToTac(cb);
    cb->AddInstr(new CTacInstr(opReturn, t));
  }
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
                       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const
{
  if(!_cond->TypeCheck(t, msg)) return false; // Do TypeCheck on condition expression
  if(!_cond->GetType()->IsBoolean()) {
    if(t != NULL) *t = _cond->GetToken();
    if(msg != NULL) *msg = "boolean expression expected.";
    return false;
  }
  bool result = true;
  CAstStatement *s = _ifBody;
  while(result && (s != NULL)) { // Do TypeCheck on all ifBody-statements
    result = s->TypeCheck(t, msg);
    s = s->GetNext();
  }

  s = _elseBody;
  while(result && (s != NULL)) { // Do TypeCheck on all elseBody-statements
    result = s->TypeCheck(t, msg);
    s = s->GetNext();
  }

  return result;
}

ostream& CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" 
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  CTacLabel* truelabel = cb->CreateLabel("if_true");
  CTacLabel* falselabel = cb->CreateLabel("if_false");
  _cond->ToTac(cb, truelabel, falselabel);

  CAstStatement *s = _ifBody;
  cb->AddInstr(truelabel);
  while(s != NULL) {
    CTacLabel* tempnext = cb->CreateLabel();
    s->ToTac(cb, tempnext);
    cb->AddInstr(tempnext);
    s = s->GetNext();
  }
  cb->AddInstr(new CTacInstr(opGoto, next));

  s = _elseBody;
  cb->AddInstr(falselabel);
  while(s != NULL) {
    CTacLabel* tempnext = cb->CreateLabel();
    s->ToTac(cb, tempnext);
    cb->AddInstr(tempnext);
    s = s->GetNext();
  }

  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t,
                             CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const
{
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const
{
  if(!_cond->TypeCheck(t, msg)) return false; // Do TypeCheck on condition expression
  if(!_cond->GetType()->IsBoolean()) {
    if(t != NULL) *t = _cond->GetToken();
    if(msg != NULL) *msg = "boolean expression expected.";
    return false;
  }
  bool result = true;
  CAstStatement *s = _body;
  while(result && (s != NULL)) { // Do TypeCheck on all body-statements
    result = s->TypeCheck(t, msg);
    s = s->GetNext();
  }

  return result;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  }
  else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t)
  : CAstNode(t)
{
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
                           CAstExpression *l,CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd)        || (oper == opSub)         ||
         (oper == opMul)        || (oper == opDiv)         ||
         (oper == opAnd)        || (oper == opOr)          ||
         (oper == opEqual)      || (oper == opNotEqual)    ||
         (oper == opLessThan)   || (oper == opLessEqual)   ||
         (oper == opBiggerThan) || (oper == opBiggerEqual)
        );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const
{
  return _right;
}

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const
{
  if(!(_left->TypeCheck(t, msg) && _right->TypeCheck(t, msg))) return false; // do TypeCheck on left and right operand
  if(GetType()) return true; // by the algorithm of GetType, if GetType is not null it correctly type-checks
  // otherwise, something doesn't match so we set appropriate error message
  if(t != NULL) *t = GetToken(); 
  if(msg != NULL) {
    ostringstream ss;
    ss << GetOperation();
    ss << ": type mismatch.";
    ss << "\n  left  operand: ";
    _left->GetType()->print(ss, 0);
    ss << "\n  right operand: ";
    _right->GetType()->print(ss, 0);
    *msg = ss.str();
  }
  return false;
}

const CType* CAstBinaryOp::GetType(void) const
{
  const CType *lt = _left->GetType();
  const CType *rt = _right->GetType();
  if(!lt || !rt) return NULL; // if lt or rt is NULL, there's nothing to work with in the first place

  // Just plain implementation of type system specification
  EOperation oper = GetOperation();
  if(oper == opEqual || oper == opLessEqual || oper == opLessThan ||
     oper == opBiggerThan || oper == opBiggerEqual || oper == opNotEqual) {
    if(oper == opEqual || oper == opNotEqual) {
      if(lt->Match(rt) && (lt->IsBoolean() || lt->IsInt() || lt->IsChar()))
        return CTypeManager::Get()->GetBool();
      return NULL;
    } else {
      if(lt->Match(rt) && (lt->IsInt() || lt->IsChar()))
        return CTypeManager::Get()->GetBool();
      return NULL;
    }
  } else if(oper == opOr || oper == opAnd) {
    if(lt->IsBoolean() && rt->IsBoolean())
       return CTypeManager::Get()->GetBool();
    return NULL;
  } else {
    if(lt->IsInt() && rt->IsInt())
       return CTypeManager::Get()->GetInt();
    return NULL;
  }
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _left->print(out, indent+2);
  _right->print(out, indent+2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb)
{
  if(!IsRelOp(GetOperation())) {
    if(GetOperation() == opAnd) {
      CTacLabel* ltrue = cb->CreateLabel();
      CTacLabel* lfalse = cb->CreateLabel();
      CTacLabel* lnext = cb->CreateLabel();
      CTacLabel* lchecksecond = cb->CreateLabel();

      CTacAddr* l = _left->ToTac(cb, lchecksecond, lfalse);

      cb->AddInstr(lchecksecond);
      CTacAddr* r = _right->ToTac(cb, ltrue, lfalse);

      CTacAddr* t = cb->CreateTemp(GetType());

      cb->AddInstr(ltrue);
      cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(1)));
      cb->AddInstr(new CTacInstr(opGoto, lnext));

      cb->AddInstr(lfalse);
      cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(0)));

      cb->AddInstr(lnext);

      return t;
    } else if(GetOperation() == opOr) {
      CTacLabel* ltrue = cb->CreateLabel();
      CTacLabel* lfalse = cb->CreateLabel();
      CTacLabel* lnext = cb->CreateLabel();
      CTacLabel* lchecksecond = cb->CreateLabel();

      CTacAddr* l = _left->ToTac(cb, ltrue, lchecksecond);

      cb->AddInstr(lchecksecond);
      CTacAddr* r = _right->ToTac(cb, ltrue, lfalse);

      CTacAddr* t = cb->CreateTemp(GetType());

      cb->AddInstr(ltrue);
      cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(1)));
      cb->AddInstr(new CTacInstr(opGoto, lnext));

      cb->AddInstr(lfalse);
      cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(0)));

      cb->AddInstr(lnext);

      return t;
    } else {
      CTacAddr* l = _left->ToTac(cb);
      CTacAddr* r = _right->ToTac(cb);
      CTacAddr* t = cb->CreateTemp(GetType());
      cb->AddInstr(new CTacInstr(GetOperation(), t, l, r));
      return t;
    }
  } else {
    CTacAddr* l = _left->ToTac(cb);
    CTacAddr* r = _right->ToTac(cb);

    CTacAddr* t = cb->CreateTemp(GetType());
    CTacLabel* ltrue = cb->CreateLabel();
    CTacLabel* lfalse = cb->CreateLabel();
    CTacLabel* lnext = cb->CreateLabel();
    cb->AddInstr(new CTacInstr(GetOperation(), ltrue, l, r));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));

    cb->AddInstr(ltrue);
    cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, lnext));

    cb->AddInstr(lfalse);
    cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(0)));

    cb->AddInstr(lnext);

    return t;
  }
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb,
                              CTacLabel *ltrue, CTacLabel *lfalse)
{
  if(!IsRelOp(GetOperation())) {
    if(GetOperation() == opAnd) {
      CTacLabel* lchecksecond = cb->CreateLabel();

      _left->ToTac(cb, lchecksecond, lfalse);
      
      cb->AddInstr(lchecksecond);
      _right->ToTac(cb, ltrue, lfalse);

      return NULL;
    } else if(GetOperation() == opOr) {
      CTacLabel* lchecksecond = cb->CreateLabel();

      _left->ToTac(cb, ltrue, lchecksecond);

      cb->AddInstr(lchecksecond);
      _right->ToTac(cb, ltrue, lfalse);

      return NULL;
    } else {
      assert(false && "condition expression should be of type boolean");
    }
  } else {
    CTacAddr* l = _left->ToTac(cb);
    CTacAddr* r = _right->ToTac(cb);

    cb->AddInstr(new CTacInstr(GetOperation(), ltrue, l, r));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));

    return NULL;
  }
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const
{
  if(!_operand->TypeCheck(t, msg)) return false; // Do TypeCheck on operand
  if(GetType()) return true; // By algorithm of GetType(), if GetType doesn't return NULL, it type-checsk
  // otherwise types doesn't match in some way... so we emit appropriate error message
  if(t != NULL) *t = GetToken(); 
  if(msg != NULL) {
    ostringstream ss;
    ss << GetOperation();
    ss << ": type mismatch.";
    ss << "\n  operand:       ";
    _operand->GetType()->print(ss, 0);
    ss << "\n";
    *msg = ss.str();
  }
  return false;
}

const CType* CAstUnaryOp::GetType(void) const
{
  // Just plain implementation of type system specification
  if(GetOperation() == opNot && _operand->GetType()->IsBoolean())
    return CTypeManager::Get()->GetBool();
  else if((GetOperation() == opPos || GetOperation() == opNeg) && _operand->GetType()->IsInt())
    return CTypeManager::Get()->GetInt();
  return NULL;
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb)
{
  if(GetOperation() == opNot) {
    CTacAddr* o = _operand->ToTac(cb);
    CTacAddr* t;

    CTacLabel* ltrue = cb->CreateLabel();
    CTacLabel* lfalse = cb->CreateLabel();
    CTacLabel* lnext = cb->CreateLabel();

    cb->AddInstr(new CTacInstr(opEqual, ltrue, o, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));

    cb->AddInstr(ltrue);
    cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, lnext));

    cb->AddInstr(lfalse);
    cb->AddInstr(new CTacInstr(opAssign, t, new CTacConst(0)));

    cb->AddInstr(lnext);

    return t;
  } else {
    CTacAddr* o = _operand->ToTac(cb);
    CTacAddr* t = cb->CreateTemp(GetType());
    cb->AddInstr(new CTacInstr(GetOperation(), t, o));
    return t;
  }
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb,
                             CTacLabel *ltrue, CTacLabel *lfalse)
{
  if(GetOperation() == opNot) {
    CTacAddr* o = _operand->ToTac(cb);

    cb->AddInstr(new CTacInstr(opEqual, ltrue, o, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));

    return NULL;
  } else {
    assert(false && "should be boolean");
  }
}


//------------------------------------------------------------------------------
// CAstSpecialOp
//
CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e,
                             const CType *type)
  : CAstOperation(t, oper), _operand(e), _type(type)
{
  assert((oper == opAddress) || (oper == opDeref) || (oper = opCast));
  assert(e != NULL);
  assert(((oper != opCast) && (type == NULL)) ||
         ((oper == opCast) && (type != NULL)));
}

CAstExpression* CAstSpecialOp::GetOperand(void) const
{
  return _operand;
}

bool CAstSpecialOp::TypeCheck(CToken *t, string *msg) const
{
  if(!_operand->TypeCheck(t, msg)) return false;
  if(!GetType()) {
    // if GetType is NULL, it implies that operation was invalid
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "Invalid special operation";
    return false;
  }
  if(GetOperation() == opAddress && !GetOperand()->GetType()->IsArray()) {
    // we don't support pointer to something that's not an array
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "We only support pointer to array";
    return false;
  }
  return true;
}

const CType* CAstSpecialOp::GetType(void) const
{
  if(GetOperation() == opAddress) {
    if(GetOperand()->GetType() == NULL) return NULL;
    return CTypeManager::Get()->GetPointer(GetOperand()->GetType());
  }
  // we don't support opDeref nor opCast, so we just return NULL
  return NULL;
}

ostream& CAstSpecialOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstSpecialOp::ToTac(CCodeBlock *cb)
{
  CTacAddr* o = _operand->ToTac(cb);
  CTacAddr* t = cb->CreateTemp(GetType());
  cb->AddInstr(new CTacInstr(GetOperation(), t, o));
  return t;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const
{
  return (int)_arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const
{
  if(GetNArgs() < _symbol->GetNParams()) {
    // number of given arguments are less than what is expected
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "not enough arguments.";
    return false;
  } else if(GetNArgs() > _symbol->GetNParams()) {
    // number of given arguments are more than what is expected
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "too many arguments.";
    return false;
  }

  int arg_len = GetNArgs();
  int i;
  for(i = 0; i < arg_len; i++) {
    if(!GetArg(i)->TypeCheck(t, msg)) return false; // run its own TypeCheck
    if(!_symbol->GetParam(i)->GetDataType()->Match(GetArg(i)->GetType())) {
      // symbol's type should match argument's type
      // note the direction of match (int[] matches int[5] but not backwards)
      if(t != NULL) *t = GetArg(i)->GetToken();
      if(msg != NULL) {
        ostringstream ss;
        ss << "parameter " << (i+1) << ": " << "argument type mismatch.";
        ss << "\n  expected ";
        _symbol->GetParam(i)->GetDataType()->print(ss, 0);
        ss << "\n  got      ";
        GetArg(i)->GetType()->print(ss,0);
        *msg = ss.str();
      }
      return false;
    }
  }
  return true;
}

const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->print(out, indent+2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb)
{
  int i, n;
  n = GetNArgs();
  for(i = n-1; i >= 0; i--) {
    CTacAddr* t = GetArg(i)->ToTac(cb);
    cb->AddInstr(new CTacInstr(opParam, new CTacConst(i), t));
  }
  if(GetType()->IsNull()) {
    cb->AddInstr(new CTacInstr(opCall, NULL, new CTacName(_symbol)));
    return NULL;
  } else{
    CTacAddr* t = cb->CreateTemp(GetType());
    cb->AddInstr(new CTacInstr(opCall, t, new CTacName(_symbol)));
    return t;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb,
                                  CTacLabel *ltrue, CTacLabel *lfalse)
{
  CTacAddr* t = ToTac(cb);
  cb->AddInstr(new CTacInstr(opEqual, ltrue, t, new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return t;
}



//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t)
{
}


//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol)
  : CAstOperand(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const
{
  return true; // there is nothing to check
}

const CType* CAstDesignator::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb)
{
  return new CTacName(_symbol);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  cb->AddInstr(new CTacInstr(opEqual, ltrue, ToTac(cb), new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstArrayDesignator
//
CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
  : CAstDesignator(t, symbol), _done(false), _offset(NULL)
{
}

void CAstArrayDesignator::AddIndex(CAstExpression *idx)
{
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete(void)
{
  assert(!_done);
  _done = true;
}

int CAstArrayDesignator::GetNIndices(void) const
{
  return (int)_idx.size();
}

CAstExpression* CAstArrayDesignator::GetIndex(int index) const
{
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;
  assert(_done);

  if(!GetType()) { // if GetType is NULL then it is an invalid array expression
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "invalid array expression.";
    return false;
  }

  int i;
  for(i = 0; i < GetNIndices(); i++) {
    CAstExpression *e = GetIndex(i);
    if(!e->TypeCheck(t, msg)) return false; // Do TypeCheck on indexing expression

    if(!e->GetType()->IsInt()) { // The indexing expression should be Int type
      if(t != NULL) *t = e->GetToken();
      if(msg != NULL) *msg = "invalid array index expression.";
      return false;
    }
  }

  return result;
}

const CType* CAstArrayDesignator::GetType(void) const
{
  const CSymbol *sym = GetSymbol();
  const CType *typ = sym->GetDataType();

  int i = GetNIndices();
  while(i > 0) { // strip outer "array" for number of indices
    if(const CPointerType *ptyp = dynamic_cast<const CPointerType *>(typ))
      typ = ptyp->GetBaseType(); // strip outermost pointer

    if(const CArrayType *artyp = dynamic_cast<const CArrayType *>(typ))
      typ = artyp->GetInnerType();
    else
      return NULL; // if we can't strip in the middle, return NULL
    
    i--;
  }
  return typ;
}

ostream& CAstArrayDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->print(out, indent+2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb,
                                     CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value)
{
}

void CAstConstant::SetValue(long long value)
{
  _value = value;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const
{
  if(_type->IsInt() && (_value < -2147483648 || _value > 2147483647)) {
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "integer constant outside valid range.";
    return false;
  }
  return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb)
{
  return new CTacConst(_value);
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  cb->AddInstr(new CTacInstr(opEqual, ltrue, ToTac(cb), new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value,
                                       CAstScope *s)
  : CAstOperand(t)
{
  CTypeManager *tm = CTypeManager::Get();

  _type = tm->GetArray(strlen(CScanner::unescape(value).c_str())+1,
                       tm->GetChar());
  _value = new CDataInitString(value);

  ostringstream o;
  o << "_str_" << ++_idx;

  _sym = new CSymGlobal(o.str(), _type);
  _sym->SetData(_value);
  s->GetSymbolTable()->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const
{
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr(void) const
{
  return GetValue();
}

bool CAstStringConstant::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstStringConstant::GetType(void) const
{
  return _type;
}

ostream& CAstStringConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const
{
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(GetValueStr())
      << "\\\"\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


