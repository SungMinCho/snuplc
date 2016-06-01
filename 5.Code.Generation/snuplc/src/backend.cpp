//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
/// 2016/04/04 Bernhard Egger adapted to SnuPL/1
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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern DIM" << endl
       << _ind << ".extern DOFS" << endl
       << _ind << ".extern ReadInt" << endl
       << _ind << ".extern WriteInt" << endl
       << _ind << ".extern WriteStr" << endl
       << _ind << ".extern WriteChar" << endl
       << _ind << ".extern WriteLn" << endl
       << endl;

  // TODO
  // forall s in subscopes do
  //   EmitScope(s)
  // EmitScope(program)
  SetScope(_m);
  const vector<CScope *> subscopes = GetScope()->GetSubscopes();

  for(int i = 0; i < subscopes.size(); i++) {
    EmitScope(subscopes[i]);
  }

  EmitScope(_m);

  _out << _ind << "# end of text section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);
  SetScope(scope);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

  // TODO
  // ComputeStackOffsets(scope)
  int callee_saved_size = 12;
  int stacksize = ComputeStackOffsets(scope->GetSymbolTable(), 8, callee_saved_size); // temp
  stacksize -= callee_saved_size;
  //
  // emit function prologue
  _out << _ind << "# stack offsets:" << endl;
  vector<CSymbol*> slist = scope->GetSymbolTable()->GetSymbols();
  vector<CSymbol*>::iterator i;
  for(i = slist.begin(); i != slist.end(); i++) {
    if(dynamic_cast<CSymLocal*>(*i) != NULL || dynamic_cast<CSymParam*>(*i) != NULL) {
      stringstream name, size, repr;
      name << (*i)->GetOffset() << "(" << (*i)->GetBaseRegister() << ")";
      size << (*i)->GetDataType()->GetSize();
      (*i)->print(repr);

      _out << _ind << "#" << setw(13) << right << name.str() << setw(4) << 
        right  << size.str() << "  " << repr.str() << endl;
    }
  }
  _out << endl;

  _out << _ind << "# prologue" << endl;
  EmitInstruction("pushl", "%ebp");
  EmitInstruction("movl", "%esp, %ebp");
  EmitInstruction("pushl", "%ebx", "save callee saved registers");
  EmitInstruction("pushl", "%esi");
  EmitInstruction("pushl", "%edi");
  stringstream stack;
  stack << "$" << stacksize << ", %esp";
  EmitInstruction("subl", stack.str(), "make room for locals");
  _out << endl;

  if(stacksize >= 20) { // memset 0
    EmitInstruction("cld", "", "memset local stack area to 0");
    EmitInstruction("xorl", "%eax, %eax");
    stringstream quarter_stack_ecx;
    quarter_stack_ecx << "$" << stacksize / 4 << ", %ecx";
    EmitInstruction("movl", quarter_stack_ecx.str());
    EmitInstruction("mov", "%esp, %edi");
    EmitInstruction("rep", "stosl");
  } else { // memset 0 manually
    EmitInstruction("xorl", "%eax, %eax", "memset local stack area to 0");
    int offset = stacksize - 4;
    while(offset >= 0) {
      stringstream ss;
      ss << "%eax, " << offset << "(%esp)";
      EmitInstruction("movl", ss.str());
      offset -= 4;
    }
  }
  EmitLocalData(scope);
  _out << endl;
  //
  // forall i in instructions do
  //   EmitInstruction(i)
  //

  _out << _ind << "# function body" << endl;
  const list<CTacInstr*> instrs = scope->GetCodeBlock()->GetInstr();
  list<CTacInstr*>::const_iterator it;
  for(it = instrs.begin(); it != instrs.end(); it++) {
    EmitInstruction(*it);
  }

  // emit function epilogue
  _out << endl;
  _out << Label("exit") << ":" << endl;
  _out << _ind << "# epilogue" << endl;
  EmitInstruction("addl", stack.str(), "remove locals");
  EmitInstruction("popl", "%edi");
  EmitInstruction("popl", "%esi");
  EmitInstruction("popl", "%ebx");
  EmitInstruction("popl", "%ebp");
  EmitInstruction("ret", "");
  _out << endl;
}

void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  // emit the globals for the current scope
  CSymtab *st = scope->GetSymbolTable();
  assert(st != NULL);

  bool header = false;

  vector<CSymbol*> slist = st->GetSymbols();

  _out << dec;

  size_t size = 0;

  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];
    const CType *t = s->GetDataType();

    if (s->GetSymbolType() == stGlobal) {
      if (!header) {
        _out << _ind << "# scope: " << scope->GetName() << endl;
        header = true;
      }

      // insert alignment only when necessary
      if ((t->GetAlign() > 1) && (size % t->GetAlign() != 0)) {
        size += t->GetAlign() - size % t->GetAlign();
        _out << setw(4) << " " << ".align "
             << right << setw(3) << t->GetAlign() << endl;
      }

      _out << left << setw(36) << s->GetName() + ":" << "# " << t << endl;

      if (t->IsArray()) {
        const CArrayType *a = dynamic_cast<const CArrayType*>(t);
        assert(a != NULL);
        int dim = a->GetNDim();

        _out << setw(4) << " "
          << ".long " << right << setw(4) << dim << endl;

        for (int d=0; d<dim; d++) {
          assert(a != NULL);

          _out << setw(4) << " "
            << ".long " << right << setw(4) << a->GetNElem() << endl;

          a = dynamic_cast<const CArrayType*>(a->GetInnerType());
        }
      }

      const CDataInitializer *di = s->GetData();
      if (di != NULL) {
        const CDataInitString *sdi = dynamic_cast<const CDataInitString*>(di);
        assert(sdi != NULL);  // only support string data initializers for now

        _out << left << setw(4) << " "
          << ".asciz " << '"' << sdi->GetData() << '"' << endl;
      } else {
        _out  << left << setw(4) << " "
          << ".skip " << dec << right << setw(4) << t->GetDataSize()
          << endl;
      }

      size += t->GetSize();
    }
  }

  _out << endl;

  // emit globals in subscopes (necessary if we support static local variables)
  vector<CScope*>::const_iterator sit = scope->GetSubscopes().begin();
  while (sit != scope->GetSubscopes().end()) EmitGlobalData(*sit++);
}

void CBackendx86::EmitLocalData(CScope *scope)
{
  assert(scope != NULL);

  CSymtab* symtab = scope->GetSymbolTable();
  vector<CSymbol*> slist = symtab->GetSymbols();
  vector<CSymbol*>::iterator it;
  for(it = slist.begin(); it != slist.end(); it++) {
    const CArrayType* typ = dynamic_cast<const CArrayType*>((*it)->GetDataType());
    if(typ != NULL && (*it)->GetSymbolType() == stLocal && (dynamic_cast<CSymParam*>(*it) == NULL)) {
      // is array and is local that is not param
      int offset = (*it)->GetOffset();
      string base = (*it)->GetBaseRegister();

      int dim = typ->GetNDim();

      stringstream ss, ss2;
      ss << "$" << dim << "," << offset << "(" << base << ")";
      ss2 << "local array '" << (*it)->GetName() << "': " << dim << " dimensions";

      EmitInstruction("movl", ss.str(), ss2.str());

      int dimcount = 1;
      while(typ != NULL) {
        stringstream ss, ss2;
        offset += 4;
        int n = typ->GetNElem();
        ss << "$" << n << "," << offset << "(" << base << ")";
        ss2 << "  dimension " << dimcount << ": " << n << " elements";

        EmitInstruction("movl", ss.str(), ss2.str());

        typ = dynamic_cast<const CArrayType*>(typ->GetInnerType());
        dim++;
      }
    }
  }
}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb)
{
  assert(cb != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++);
}

void CBackendx86::EmitInstruction(CTacInstr *i)
{
  assert(i != NULL);

  ostringstream cmt;
  string mnm;
  cmt << i;

  EOperation op = i->GetOperation();

  switch (op) {
    // binary operators
    // dst = src1 op src2
    // TODO
    case opAdd:
    mnm = "addl";
    case opSub:
    if(mnm.empty()) mnm = "subl";
    case opAnd:
    if(mnm.empty()) mnm = "andl";
    case opOr:
    if(mnm.empty()) mnm = "orl";
    Load(i->GetSrc(1), "%eax", cmt.str());
    Load(i->GetSrc(2), "%ebx");
    EmitInstruction(mnm, "%ebx, %eax");
    Store(i->GetDest(), 'a');
    break;

    case opMul:
    mnm = "imull";
    case opDiv:
    if(mnm.empty()) mnm = "idivl";
    Load(i->GetSrc(1), "%eax", cmt.str());
    Load(i->GetSrc(2), "%ebx");
    if(mnm == "idivl") EmitInstruction("cdq", "");
    EmitInstruction(mnm, "%ebx");
    Store(i->GetDest(), 'a');
    break;

    case opNeg:
    Load(i->GetSrc(1), "%eax", cmt.str());
    EmitInstruction("negl", "%eax");
    Store(i->GetDest(), 'a');
    case opPos:
    case opNot:
    break;

    case opAssign:
    Load(i->GetSrc(1), "%eax", cmt.str());
    Store(i->GetDest(), 'a');
    break;
    case opCast:

    break;
    case opGoto:
    EmitInstruction("jmp", Label(dynamic_cast<const CTacLabel*>(i->GetDest())), cmt.str());
    break;

    case opEqual:
    case opNotEqual:
    case opLessThan:
    case opLessEqual:
    case opBiggerThan:
    case opBiggerEqual:
    mnm = "j" + Condition(op);
    Load(i->GetSrc(1), "%eax", cmt.str());
    Load(i->GetSrc(2), "%ebx");
    EmitInstruction("cmpl", "%ebx, %eax");
    EmitInstruction(mnm, Label(dynamic_cast<const CTacLabel*>(i->GetDest())));

    break;

    case opCall:
    {
      CTacAddr* calltac = i->GetSrc(1);
      CTacName* callname = dynamic_cast<CTacName*>(calltac);
      EmitInstruction("call", callname->GetSymbol()->GetName(), cmt.str());
      const CSymProc* symproc = dynamic_cast<const CSymProc*>(callname->GetSymbol());
      int numargs = symproc->GetNParams();
      if(numargs > 0) {
        stringstream ss;
        ss << "$" << numargs*4 << ", %esp";
        EmitInstruction("addl", ss.str());
      }
      // save return value if we should
      if(i->GetDest()) {
        Store(i->GetDest(), 'a');
      }
    }
    break;
    case opReturn:
    if(i->GetSrc(1)) {
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("jmp", Label("exit"));
    } else {
      EmitInstruction("jmp", Label("exit"), cmt.str());
    }
    break;
    case opParam:
    Load(i->GetSrc(1), "%eax", cmt.str());
    EmitInstruction("pushl", "%eax");
    break;

    // unary operators
    // dst = op src1
    // TODO

    // memory operations
    // dst = src1
    // TODO

    // pointer operations
    // dst = &src1
    // TODO
    // dst = *src1
    case opAddress:
    {
      stringstream args;
      args << Operand(i->GetSrc(1)) << ", %eax";
      EmitInstruction("leal", args.str(), cmt.str());
      Store(i->GetDest(), 'a');
    }
    break;

    case opDeref:
      // opDeref not generated for now
      EmitInstruction("# opDeref", "not implemented", cmt.str());
      break;


    // unconditional branching
    // goto dst
    // TODO

    // conditional branching
    // if src1 relOp src2 then goto dst
    // TODO

    // function call-related operations
    // TODO

    // special
    case opLabel:
      _out << Label(dynamic_cast<CTacLabel*>(i)) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;


    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
  }
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

void CBackendx86::Load(CTacAddr *src, string dst, string comment)
{
  assert(src != NULL);

  string mnm = "mov";
  string mod = "l";

  // set operator modifier based on the operand size
  switch (OperandSize(src)) {
    case 1: mod = "zbl"; break;
    case 2: mod = "zwl"; break;
    case 4: mod = "l"; break;
  }

  // emit the load instruction
  EmitInstruction(mnm + mod, Operand(src) + ", " + dst, comment);
}

void CBackendx86::Store(CTac *dst, char src_base, string comment)
{
  assert(dst != NULL);

  string mnm = "mov";
  string mod = "l";
  string src = "%";

  // compose the source register name based on the operand size
  switch (OperandSize(dst)) {
    case 1: mod = "b"; src += string(1, src_base) + "l"; break;
    case 2: mod = "w"; src += string(1, src_base) + "x"; break;
    case 4: mod = "l"; src += "e" + string(1, src_base) + "x"; break;
  }

  // emit the store instruction
  EmitInstruction(mnm + mod, src + ", " + Operand(dst), comment);
}

string CBackendx86::Operand(const CTac *op)
{
  string operand = "TODO";

  // TODO
  // return a string representing op
  // hint: take special care of references (op of type CTacReference)
  
  if(const CTacName *name = dynamic_cast<const CTacName*>(op)) {
    stringstream ss;
    const CSymbol* sym = name->GetSymbol();
    if(dynamic_cast<const CTacReference*>(name)) {
      return "(%edi)";
    }
    if(sym->GetSymbolType() == stGlobal) {
      ss << sym->GetName();
    } else {
      ss << sym->GetOffset() << "(" << sym->GetBaseRegister() << ")";
    }
    operand = ss.str();
  } else if(const CTacConst *con = dynamic_cast<const CTacConst*>(op)) {
    stringstream ss;
    ss << "$" << con->GetValue();
    operand = ss.str();
  }

  return operand;
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(const CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
  return "l_" + cs->GetName() + "_" + label->GetLabel();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}

int CBackendx86::OperandSize(CTac *t) const
{
  CTacAddr* addr = dynamic_cast<CTacAddr*>(t);
  assert(addr != NULL);
  if(CTacName* name = dynamic_cast<CTacName*>(addr)) {
    const CSymbol* sym = name->GetSymbol();
    if(CTacReference* ref = dynamic_cast<CTacReference*>(name))
      sym = ref->GetDerefSymbol();
    int size = sym->GetDataType()->GetDataSize();
    if(size > 4) return 4;
    return size;
  } else {
    return 4;
  }

  // TODO
  // compute the size for operand t of type CTacName
  // Hint: you need to take special care of references (incl. references to pointers!)
  //       and arrays. Compare your output to that of the reference implementation
  //       if you are not sure.
}

size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab,
                                        int param_ofs,int local_ofs)
{
  assert(symtab != NULL);
  vector<CSymbol*> slist = symtab->GetSymbols();

  // TODO
  // foreach local symbol l in slist do
  //   compute aligned offset on stack and store in symbol l
  //   set base register to %ebp
  //
  // foreach parameter p in slist do
  //   compute offset on stack and store in symbol p
  //   set base register to %ebp
  //
  int size = local_ofs;
  int cursize;
  vector<CSymbol*>::iterator it;
  for(it = slist.begin(); it != slist.end(); it++) {
    if(CSymParam* param = dynamic_cast<CSymParam*>(*it)) {
      (*it)->SetBaseRegister("%ebp");
      (*it)->SetOffset(param_ofs + param->GetIndex() * 4);
    } else if(dynamic_cast<CSymLocal*>(*it)) {
      cursize = (*it)->GetDataType()->GetSize();
      size += cursize;
      if(((*it)->GetDataType()->IsInt() || (*it)->GetDataType()->IsArray() || (*it)->GetDataType()->IsPointer())
          && (size % 4 != 0)) {
        size += 4 - (size % 4);
      }
      (*it)->SetBaseRegister("%ebp");
      (*it)->SetOffset(-size);
    }
  }
  // align size
  //
  int rem = size % 4; // align by 4
  if(rem != 0) {
    size += 4 - rem;
  }
  // dump stack frame to assembly file

  return size;
}
