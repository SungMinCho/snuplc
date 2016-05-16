//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2016/03/09 Bernhard Egger adapted to SnuPL/!
/// 2016/04/08 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#ifndef __SnuPL_PARSER_H__
#define __SnuPL_PARSER_H__

#include "scanner.h"
#include "symtab.h"
#include "ast.h"


//------------------------------------------------------------------------------
/// @brief parser
///
/// parses a module
///
class CParser {
  public:
    /// @brief constructor
    ///
    /// @param scanner  CScanner from which the input stream is read
    CParser(CScanner *scanner);

    /// @brief parse a module
    /// @retval CAstNode program node
    CAstNode* Parse(void);

    /// @name error handling
    ///@{

    /// @brief indicates whether there was an error while parsing the source
    /// @retval true if the parser detected an error
    /// @retval false otherwise
    bool HasError(void) const { return _abort; };

    /// @brief returns the token that caused the error
    /// @retval CToken containing the error token
    const CToken* GetErrorToken(void) const;

    /// @brief returns a human-readable error message
    /// @retval error message
    string GetErrorMessage(void) const;
    ///@}

  private:
    /// @brief sets the token causing a parse error along with a message
    /// @param t token causing the error
    /// @param message human-readable error message
    void SetError(CToken t, const string message);

    /// @brief consume a token given type and optionally store the token
    /// @param type expected token type
    /// @param token If not null, the consumed token is stored in 'token'
    /// @retval true if a token has been consumed
    /// @retval false otherwise
    bool Consume(EToken type, CToken *token=NULL);


    /// @brief initialize symbol table @a s with predefined procedures and
    ///        global variables
    void InitSymbolTable(CSymtab *s);

    /// @name methods for recursive-descent parsing
    /// @{
    
    /// @brief parses type and returns CType*
    /// @param isArgument whether the type is for argument of a procedure/function or not
    /// @param isReturnType indicates whether the type is for return type of procedure/function
    /// @retval const CType * parsed type
    const CType*      type(bool isArgument, bool isReturnType=false);

    /// @brief parses varDecl and saves them in symbol table of scope s
    /// @param s CAstScope in which symbol table we store the declared variables
    /// @param isGlobal optional argument that indicates whether the variables are meant to be global
    /// @param symproc optionally given when we should also store the variables as parameters of a procedure/function
    void              varDecl(CAstScope *s, bool isGlobal=false, CSymProc *symproc = NULL);

    /// @brief parses varDeclSequence and saves them in symbol table of scope s
    /// @param s CAstScope in which symbol table we store the declared variables
    /// @param isGlobal optional argument that indicates whether the variables are meant to be global
    /// @param symproc optionally given when we should also store the variables as parameters of a procedure/function
    /// @param fromFormalParam indicates whether this was called while parsing formalParam 
    void              varDeclSequence(CAstScope *s, bool isGlobal=false, CSymProc *symproc = NULL, bool fromFormalParam=false);

    /// @brief parses varDeclaration and saves them in symbol table of scope s
    /// @param s CAstScope in which symbol table we store the declared variables
    /// @param isGlobal optional argument that indicates whether the variables are meant to be global
    void              varDeclaration(CAstScope *s, bool isGlobal=false);

    /// @brief parses formalParam of procedure/function
    /// @param s the procedure/function scope in which we should save symbols of parameters
    /// @param symproc the CSymProc of the function in which we should add parameter types
    void              formalParam(CAstScope *s, CSymProc *symproc = NULL);

    /// @brief parses module and returns CAstModule* for it
    /// @retval CAstModule * module AST
    CAstModule*       module(void);

    /// @brief parses subroutineDecl and returns CAstProcedure* for it
    /// @param s module scope where this subroutine belongs
    /// @retval CAstProcedure * procedure AST
    CAstProcedure*    subroutineDecl(CAstScope *s);

    /// @brief parses statSequence and returns CAstStatement* for it
    /// @param s the scope where statements belong
    /// @retval CAstStatement * statSequence AST
    CAstStatement*    statSequence(CAstScope *s);

    /// @brief parses subroutineBody and returns CAstStatement* for it
    /// @param s the scope of the procedure where this body belongs
    /// @retval CAstStatement * subroutine body AST
    CAstStatement*    subroutineBody(CAstScope *s);

    /// @brief parses expression and returns CAstExpression* for it
    /// @param s scope where the expression belongs
    /// @retval CAstExpression * expression AST
    CAstExpression*   expression(CAstScope *s);

    /// @brief parses simpleexpr and returns CAstExpression* for it
    /// @param s scope where the simpleexpr belongs
    /// @retval CAstExpression * simpleexpr AST
    CAstExpression*   simpleexpr(CAstScope *s);

    /// @brief parses term and returns CAstExpression* for it
    /// @param s scope where the term belongs
    /// @retval CAstExpression * term AST
    CAstExpression*   term(CAstScope *s);

    /// @brief parses factor and returns CAstExpression* for it
    /// @param s scope where the factor belongs
    /// @retval CAstExpression * factor AST
    CAstExpression*   factor(CAstScope *s);

    /// @brief parses subroutineCall and returns CAstFunctionCall* for it
    /// @param id the ident token which is the first token of subroutineCall. it is already consumed and passed to us
    /// @retval CAstFunctionCall * subroutineCall AST
    CAstFunctionCall* subroutineCall(CAstScope *s, CToken id);

    /// @brief parses qualident and returns CAstExpression* for it
    /// @param id the ident token which is the first token of qualident. it is already consumed and passed to us
    /// @retval CAstExpression * qualident AST
    CAstExpression*   qualident(CAstScope *s, CToken id);

    /// @brief parses a number and returns CAstConstant* for it
    /// @retval CAstConstant * number AST
    CAstConstant*     number(void);

    /// @brief parses a boolean and returns CAstConstant* for it
    /// @retval CAstConstant * boolean AST
    CAstConstant*     boolean(void);

    /// @brief parses a string and returns CAstStringConstant* for it
    /// @retval CAstStringConstant * string AST
    CAstStringConstant*   stringConstant(CAstScope* s);

    /// @}


    CScanner     *_scanner;       ///< CScanner instance
    CAstModule   *_module;        ///< root node of the program
    CToken        _token;         ///< current token

    /// @name error handling
    CToken        _error_token;   ///< error token
    string        _message;       ///< error message
    bool          _abort;         ///< error flag

};

#endif // __SnuPL_PARSER_H__
