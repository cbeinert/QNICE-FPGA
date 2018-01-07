//
//  main.c
//  BASIC_Interpreter
//
//  Created by Christoph Beinert on 12.07.17.
//  Copyright © 2017 Christoph Beinert. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#ifdef __QNICE__
char *strdup(char *s);
#endif

/* CONSTANTS */

/* Maximum number of characters in one line. APPLESOFT MANUAL S. 35 */
#define ROW_LENGTH 239

/* All supported token types */
#define FOREACH_TOKEN_TYPE(TOKEN_TYPE) \
TOKEN_TYPE(LEFT_PAREN) \
TOKEN_TYPE(RIGHT_PAREN) \
TOKEN_TYPE(SEMICOLON) \
TOKEN_TYPE(COMMA) \
TOKEN_TYPE(DOT) \
TOKEN_TYPE(QUOTATION_MARK) \
TOKEN_TYPE(MINUS) \
TOKEN_TYPE(PLUS) \
TOKEN_TYPE(SLASH) \
TOKEN_TYPE(STAR) \
TOKEN_TYPE(CARET) \
TOKEN_TYPE(COLON) \
TOKEN_TYPE(EQUAL) \
TOKEN_TYPE(EQUAL_GREATER) \
TOKEN_TYPE(EQUAL_LESS) \
TOKEN_TYPE(NOT_EQUAL_LESS_GREATER) \
TOKEN_TYPE(NOT_EQUAL_GREATER_LESS) \
TOKEN_TYPE(GREATER) \
TOKEN_TYPE(GREATER_EQUAL) \
TOKEN_TYPE(LESS) \
TOKEN_TYPE(LESS_EQUAL) \
TOKEN_TYPE(IDENTIFIER) \
TOKEN_TYPE(STRING) \
TOKEN_TYPE(NUMBER) \
TOKEN_TYPE(LINE_NUMBER) \
TOKEN_TYPE(ABS) \
TOKEN_TYPE(AND) \
TOKEN_TYPE(ASC) \
TOKEN_TYPE(AT) \
TOKEN_TYPE(ATN) \
TOKEN_TYPE(CALL) \
TOKEN_TYPE(CHR) \
TOKEN_TYPE(CLEAR) \
TOKEN_TYPE(COLOR) \
TOKEN_TYPE(CONT) \
TOKEN_TYPE(COS) \
TOKEN_TYPE(DATA) \
TOKEN_TYPE(DEF) \
TOKEN_TYPE(DEL) \
TOKEN_TYPE(DIM) \
TOKEN_TYPE(DOLLAR) \
TOKEN_TYPE(DRAW) \
TOKEN_TYPE(END) \
TOKEN_TYPE(EXP) \
TOKEN_TYPE(FLASH) \
TOKEN_TYPE(FN) \
TOKEN_TYPE(FOR) \
TOKEN_TYPE(FRE) \
TOKEN_TYPE(GET) \
TOKEN_TYPE(GOSUB) \
TOKEN_TYPE(GOTO) \
TOKEN_TYPE(GR) \
TOKEN_TYPE(HASH) \
TOKEN_TYPE(HCOLOR) \
TOKEN_TYPE(HGR) \
TOKEN_TYPE(HGR2) \
TOKEN_TYPE(HINEM) \
TOKEN_TYPE(HLIN) \
TOKEN_TYPE(HOME) \
TOKEN_TYPE(HPLOT) \
TOKEN_TYPE(HTAB) \
TOKEN_TYPE(IF) \
TOKEN_TYPE(IN) \
TOKEN_TYPE(INPUT) \
TOKEN_TYPE(INT) \
TOKEN_TYPE(INVERSE) \
TOKEN_TYPE(LEFT) \
TOKEN_TYPE(LEN) \
TOKEN_TYPE(LET) \
TOKEN_TYPE(LIST) \
TOKEN_TYPE(LOAD) \
TOKEN_TYPE(LOG) \
TOKEN_TYPE(LOMEM) \
TOKEN_TYPE(MID) \
TOKEN_TYPE(NEW) \
TOKEN_TYPE(NEXT) \
TOKEN_TYPE(NORMAL) \
TOKEN_TYPE(NOT) \
TOKEN_TYPE(NOTRACE) \
TOKEN_TYPE(ON) \
TOKEN_TYPE(ONERR) \
TOKEN_TYPE(OR) \
TOKEN_TYPE(PDL) \
TOKEN_TYPE(PEEK) \
TOKEN_TYPE(PLOT) \
TOKEN_TYPE(POKE) \
TOKEN_TYPE(POP) \
TOKEN_TYPE(POS) \
TOKEN_TYPE(PRINT) \
TOKEN_TYPE(PR) \
TOKEN_TYPE(READ) \
TOKEN_TYPE(RECALL) \
TOKEN_TYPE(REM) \
TOKEN_TYPE(RESTORE) \
TOKEN_TYPE(RESUME) \
TOKEN_TYPE(RETURN) \
TOKEN_TYPE(RIGHT) \
TOKEN_TYPE(RND) \
TOKEN_TYPE(ROT) \
TOKEN_TYPE(RUN) \
TOKEN_TYPE(SAVE) \
TOKEN_TYPE(SCALE) \
TOKEN_TYPE(SCRN) \
TOKEN_TYPE(SGN) \
TOKEN_TYPE(SHLOAD) \
TOKEN_TYPE(SIN) \
TOKEN_TYPE(SPC) \
TOKEN_TYPE(SPEED) \
TOKEN_TYPE(SQR) \
TOKEN_TYPE(STEP) \
TOKEN_TYPE(STOP) \
TOKEN_TYPE(STORE) \
TOKEN_TYPE(STR) \
TOKEN_TYPE(TAB) \
TOKEN_TYPE(TAN) \
TOKEN_TYPE(TEXT) \
TOKEN_TYPE(THEN) \
TOKEN_TYPE(TO) \
TOKEN_TYPE(TRACE) \
TOKEN_TYPE(USR) \
TOKEN_TYPE(VAL) \
TOKEN_TYPE(VLIN) \
TOKEN_TYPE(VTAB) \
TOKEN_TYPE(WAIT) \
TOKEN_TYPE(XDRAX) \

/* Generates an enumeration with all token types */
#define GENERATE_ENUM(ENUM) ENUM,

/* Generates a list of strings with all token types */
#define GENERATE_STRING(STRING) #STRING,

/* Debug switch for extra console output */
#define DEBUG_QNICE 0

/* Hashvalue for the dictionary */
#define HASHSIZE 101

/* Command to leave the interpreter */
#define EXIT "EXIT"

/* Command to print the current list of tokens */
#define PRINTTOKENS "PRINTTOKENS"

/* Increment for the token table if the table is full */
#define TOKEN_INCREMENT 50

/* Increment for the statement table if the table is full */
#define STATEMENT_INCREMENT 10

/* TYPES */

/* Token types */
typedef enum {
    FOREACH_TOKEN_TYPE(GENERATE_ENUM)
} TokenType_t;

/* Token */
typedef struct Token {
    char *lexeme;
    TokenType_t type;
} Token_t;

/* Abstract syntax tree for expressions */
//https://lambda.uta.edu/cse5317/notes/node26.html
typedef struct Expression {
    enum { NUMBER_EXP, STRING_EXP, BINARY_EXP, UNARY_EXP, GROUPING_EXP, IDENTIFIER_EXP } tag;
    
    union {
        int numberExp;
        
        char *stringExp;
        
        struct {
            struct Expression *left;
            struct Token *operator;
            struct Expression *right;
        } binaryExp;
        
        struct {
            struct Token *operator;
            struct Expression *operand;
        } unaryExp;
        
        //GROUPING_EXP
        struct Expression *groupingExp;
        
        //IDENTIFIER_EXP
        struct Token *identifierExp;
    } op;
    
    int lineNumber;
} Expression_t;

/* Type for the interpreted result of an expression */
typedef struct ExpressionResult{
    enum { NUMBER_RESULT, STRING_RESULT } tag;
    
    union {
        int numberResult;
        char *stringResult;
    } value;
} ExpResult_t;

/* Type for the statements of ABASIC */
typedef struct Statement {
    enum { EXPRESSION_STMT, PRINT_STMT, VAR_DECL_STMT, IF_STMT, FOR_STMT, NEXT_STMT, GOTO_STMT } tag;
    
    union {
        //EXPRESSION_STMT, PRINT_STMT, NEXT_STMT, GOTO_STMT
        Expression_t *expression;
        
        struct {
            struct Token *name;
            Expression_t *initializer;
        } varDeclStmt;
        
        struct {
            Expression_t *expression;
            struct Statement *thenBranch;
        } ifStmt;
        
        struct {
            struct Statement *initializer;
            Expression_t *loopEnd;
            Expression_t *increment;
        } forStmt;
    } value;
    
    int lineNumber;
} Statement_t;

/* Type for an entry in the symbol table for variables */
//https://stackoverflow.com/questions/4384359/quick-way-to-implement-dictionary-in-c
typedef struct Nlist { /* table entry: */
    struct Nlist *next; /* next entry in chain */
    char *name; /* defined name */
    ExpResult_t *expResult; /* expression */
} Nlist_t;

/* TABLES */

/* Names of the token types */
static const char *TOKEN_TYPE_STRING[] = {
    FOREACH_TOKEN_TYPE(GENERATE_STRING)
};

/* Table of tokens */
Token_t **Tokens = NULL;

/* Table of statements */
Statement_t **program = NULL;

/* Symbol table for variables */
static Nlist_t *hashtab[HASHSIZE];

/* DATA */

/* Size of the table of tokens */
int token_max_count = 50;

/* Next free index in the table of tokens */
int scannerTokenIndex = 0;

/* Index of the next token for the parser */
int parserTokenIndex = 0;

/* Size of the table of statements */
int statement_max_count = 25;

/* Next free index in the table of statements */
int stmtIndex = 0;

/* Index of the current statement of the interpreter */
int interpretedStmtIndex = 0;

/* Lexical analysis - Start position for the identification of a token */
int startPosition = 0;

/* Lexical analysis - Current position during the identification of a token */
int currentPosition = 0;

/* Line of user input */
char *currentInputRow;

/* True if the current line is a comment */
bool isComment = false;

/* True if there is an error during the lexical or syntactical analysis */
bool hadError = false;

/* True if there is an error during the interpretation */
bool hadRuntimeError = false;

/* Current line number for the error messages */
int currentLineNumber = 0;

/* FUNCTION PROTOTYPES */

/* Read current input line and create tokens */
void scanAndInterpretInput(void);

/* Create an Expression and check the syntax */
Expression_t* orExpression(void);

/* Create a duplicate of a ExpressionResult */
ExpResult_t* expResultDub(ExpResult_t *result);

/* Create a statement */
Statement_t* statement(void);

/* Create a variable declaration statement */
Statement_t* varDecl(void);

/* Interpret the statement */
void interpretStatement(Statement_t *statement);

/* Create a duplicate of a Token */
Token_t* tokenDup(Token_t *token);

/* Compare the line numbers of two statements.
 This is necessary for the ordering of statements. */
int compareLineNumberOfStatements(const void *a, const void *b);

/* Add a variable to the symbol table */
Nlist_t* install(char *name, ExpResult_t *expResult);

/* Look for a variable in the symbol table */
Nlist_t* lookup(char *s);

//TODO In der LA kann die Zeilennummer beim Error auch entfernt werden.
/* Show an error message and set hadError to true */
void report (int line, const char *where, const char *message);

/* Show an error message */
void reportParserError(char *exceptionMessage, Token_t *exceptionToken);

/* Show an error message and set hadRuntimeError to true */
void reportRuntimeError(int line, const char *where, const char *message);

/* Show an error message */
void error(int line, const char *message);

/* Deallocation of all tokens in the token table */
void freeTokens();

/* Deallocation of a Token */
void freeToken(Token_t *token);

/* Deallocation of a specific Token in the token table */
void freeSpecificToken(Token_t *token, int tokenIndex);

/* Deallocation of all statements in the statement table */
void freeStatements(void);

/* Deallocation of a Statement */
void freeStatement(Statement_t *stmt);

/* Deallocation of a Expression */
void freeExpression(Expression_t *exp);

/* Deallocation of a ExpressionResult */
void freeExpressionResult(ExpResult_t *result);

/* Deallocation of a symbol table entry */
void freeHashTabEntry(Nlist_t *np);

//Scanner Section

/* Add a token to the table of tokens */
void addToken(TokenType_t type)
{
    Tokens[scannerTokenIndex] = (Token_t*) malloc(sizeof(Token_t));
    
    if (Tokens[scannerTokenIndex] == NULL){
        printf("addToken: malloc failed!!\n");
        hadError = true;
        return;
    }
    
    Tokens[scannerTokenIndex]->type = type;
    Tokens[scannerTokenIndex]->lexeme = NULL;
    
    //For other types is the lexeme not necessary, because the type is sufficient
    if (type == STRING ||
        type == NUMBER ||
        type == IDENTIFIER){
        
        Tokens[scannerTokenIndex]->lexeme = malloc((currentPosition - startPosition + 1) * sizeof(char));
        
        if (Tokens[scannerTokenIndex]->lexeme == NULL){
            printf("addToken: malloc failed!!\n");
            hadError = true;
            return;
        }
        
        strncpy(Tokens[scannerTokenIndex]->lexeme, currentInputRow + startPosition, currentPosition - startPosition);
        Tokens[scannerTokenIndex]->lexeme[(currentPosition - startPosition)] = '\0';
    }
    
    scannerTokenIndex++;
    
    if (scannerTokenIndex == token_max_count){
        token_max_count += TOKEN_INCREMENT;
        
        Tokens = (Token_t**) realloc(Tokens, token_max_count * sizeof(Token_t*));
        
        if (Tokens == NULL){
            printf("addToken: realloc failed!!! token_max_count: %d\n", token_max_count);
            hadError = true;
        }
    }
}

/* Print a list of all scanned tokens */
void printTokens(void)
{
    for (int i = 0; i < scannerTokenIndex; i++){
        Token_t t = *Tokens[i];
        
        if (t.lexeme != NULL)
            printf("Type: %s, Lexeme: %s\n", TOKEN_TYPE_STRING[t.type], t.lexeme);
        else
            printf("Type: %s\n", TOKEN_TYPE_STRING[t.type]);
    }
    
    printf("ScannerTokenIndex: %d\n", scannerTokenIndex);
}

/* Check if the end of line is reached */
bool isAtEndOfLine(void) {
    return currentPosition >= ROW_LENGTH || *(currentInputRow + currentPosition) == '\n' || *(currentInputRow + currentPosition) == EOF;
}

/* Return true if the current character of the input line is equal expected */
bool match(char expected)
{
    if (isAtEndOfLine())
        return false;
    if (*(currentInputRow + currentPosition) != expected)
        return false;
    
    currentPosition++;
    return true;
}

/* Lookahead on the the next character */
char lookahead(void)
{
    if (isAtEndOfLine())
        return '\n';
    
    return *(currentInputRow + currentPosition);
}

/* Get the next character of the line */
char getNextChar(void)
{
    currentPosition++;
    return *(currentInputRow + currentPosition - 1);
}

/* Create a Token of type string */
void string(void)
{
    while (lookahead() != '"' && !isAtEndOfLine()){
        getNextChar();
    }
    
    //Unterminated string.
    if (isAtEndOfLine()){
        error(0, "Unterminated string.");
        return;
    }
    
    //The closing ".
    getNextChar();
    
    addToken(STRING);
}

/* Create a Token of type nummber */
void number(void)
{
    while (isdigit(lookahead()))
        getNextChar();
    
    addToken(NUMBER);
}

/* Create a Token of type identifier or a reserved word */
void identifier(void)
{
    //Identifier for strings end with a $
    while (isalnum(lookahead()) || (lookahead() == '$'))
        getNextChar();
    
    TokenType_t type = IDENTIFIER;
    
    for (int i=ABS; i<=XDRAX; i++){
        int reservedWordLength = (int) strlen(TOKEN_TYPE_STRING[i]);
        
        //Reserved Word is equal with current string
        if (reservedWordLength == currentPosition - startPosition &&
            strncmp(TOKEN_TYPE_STRING[i], currentInputRow + startPosition, reservedWordLength) == 0){
            
            if (i == REM){
                // This is a comment. Ignore the rest of the line.
                while(!isAtEndOfLine())
                    getNextChar();
            }
            
            type = i;
            break;
        }
    }
    
    addToken(type);
}

/* Scans the current line and creates tokens */
void scanCurrentLine(void)
{
    currentPosition = 0;
    
    while (!isAtEndOfLine()){
        startPosition = currentPosition;
        char currentChar = getNextChar();
        
        switch (currentChar){
            case '(': addToken(LEFT_PAREN); break;
            case ')': addToken(RIGHT_PAREN); break;
            case ',': addToken(COMMA); break;
                //            case '.': addToken(DOT, NULL); break;
            case '-': addToken(MINUS); break;
            case '+': addToken(PLUS); break;
            case ';': addToken(SEMICOLON); break;
            case '*': addToken(STAR); break;
            case '^': addToken(CARET); break;
            case '/': addToken(SLASH); break;
            case ':': addToken(COLON); break;
            case '#': addToken(HASH); break;
            case '$': addToken(DOLLAR); break;
            case '=': addToken(match('>') ? EQUAL_GREATER : match('<') ? EQUAL_LESS : EQUAL); break;
            case '<': addToken(match('>') ? NOT_EQUAL_LESS_GREATER : match('=') ? LESS_EQUAL : LESS); break;
            case '>': addToken(match('<') ? NOT_EQUAL_GREATER_LESS : match('=') ? GREATER_EQUAL : GREATER); break;
            case ' ':
            case '\r':
            case '\t':
            case '\n':
            case '\0':
                // Ignore whitespace and end of line signs.
                break;
                
            case '"': string(); break;
                
            default:
                if (isdigit(currentChar)){
                    number();
                }
                else if (isalpha(currentChar)){
                    identifier();
                }
                else{
                    char *error = malloc(sizeof(char) * 8);
                    
                    if (error == NULL){
                        printf("scanCurrentLine: malloc failed!!\n");
                        break;
                    }
                    
                    strncpy(error, " at '", 5);
                    strncpy(error+5, &currentChar, 1);
                    strncpy(error+6, "'\0", 2);
                    
                    report(currentLineNumber, error, "Unexpected character.");
                    
                    free(error);
                }
        }
    }
}

//Abstract Syntax Tree Section

/* Create an Expression of type binary */
Expression_t* make_binaryExp (Expression_t *left, Token_t *operator, Expression_t *right)
{
    if (DEBUG_QNICE)
        printf("make_binaryExp\n");
    
    if (left == NULL || operator == NULL || right == NULL){
        freeExpression(left);
        freeExpression(right);
        return NULL;
    }
    
    Expression_t *a = (Expression_t*) malloc(sizeof(Expression_t));
    
    if (a == NULL){
        printf("make_binaryExp: malloc failed!!\n");
        freeExpression(left);
        freeExpression(right);
        return NULL;
    }
    
    a->tag = BINARY_EXP;
    a->op.binaryExp.left = left;
    a->op.binaryExp.operator = tokenDup(operator);
    a->op.binaryExp.right = right;
    a->lineNumber = currentLineNumber;
    
    return a->op.binaryExp.operator == NULL ? NULL : a;
}

/* Create an Expression of type unary */
Expression_t* make_unaryExp (Token_t *operator, Expression_t *operand)
{
    if (DEBUG_QNICE)
        printf("make_unaryExp\n");
    
    if (operator == NULL || operand == NULL){
        freeExpression(operand);
        return NULL;
    }
    
    Expression_t *a = (Expression_t*) malloc(sizeof(Expression_t));
    
    if (a == NULL){
        printf("make_unaryExp: malloc failed!!\n");
        freeExpression(operand);
        return NULL;
    }
    
    a->tag = UNARY_EXP;
    a->op.unaryExp.operator = tokenDup(operator);
    a->op.unaryExp.operand = operand;
    a->lineNumber = currentLineNumber;
    
    return a->op.unaryExp.operator == NULL ? NULL : a;
}

/* Create an Expression of type grouping */
Expression_t* make_groupingExp (Expression_t *operand)
{
    if (DEBUG_QNICE)
        printf("make_groupingExp\n");
    
    if (operand == NULL)
        return NULL;
    
    Expression_t *a = (Expression_t*) malloc(sizeof(Expression_t));
    
    if (a == NULL){
        printf("make_groupingExp: malloc failed!!\n");
        freeExpression(operand);
        return NULL;
    }
    
    a->tag = GROUPING_EXP;
    a->op.groupingExp = operand;
    a->lineNumber = currentLineNumber;
    
    return a;
}

/* Create an Expression of type number */
Expression_t* make_numberExp (int numberExp)
{
    if (DEBUG_QNICE)
        printf("make_numberExp\n");
    
    Expression_t *a = (Expression_t*) malloc(sizeof(Expression_t));
    
    if (a == NULL){
        printf("make_numberExp: malloc failed!!\n");
        return NULL;
    }
    
    a->tag = NUMBER_EXP;
    a->op.numberExp = numberExp;
    a->lineNumber = currentLineNumber;
    
    return a;
}

/* Create an Expression of type string */
Expression_t* make_stringExp (char *stringExp)
{
    if (DEBUG_QNICE)
        printf("make_stringExp\n");
    
    if (stringExp == NULL)
        return NULL;
    
    Expression_t *a = (Expression_t*) malloc(sizeof(Expression_t));
    
    if (a == NULL){
        printf("make_stringExp: malloc failed!!\n");
        return NULL;
    }
    
    a->tag = STRING_EXP;
    a->op.stringExp = strdup(stringExp);
    a->lineNumber = currentLineNumber;
    
    return a->op.stringExp == NULL ? NULL : a;
}

/* Create an Expression of type indentifier */
Expression_t* make_identifierExp (Token_t *name)
{
    if (DEBUG_QNICE)
        printf("make_identifierExp\n");
    
    if (name == NULL)
        return NULL;
    
    Expression_t *a = (Expression_t*) malloc(sizeof(Expression_t));
    
    if (a == NULL){
        printf("make_identifierExp: malloc failed!!\n");
        return NULL;
    }
    
    a->tag = IDENTIFIER_EXP;
    a->op.identifierExp = tokenDup(name);
    a->lineNumber = currentLineNumber;
    
    return a->op.identifierExp == NULL ? NULL : a;
}

/* Print the created expression syntax tree. The function is only for testing. */
//https://lambda.uta.edu/cse5317/notes/node26.html
void printExpressionAst(Expression_t *exp)
{
    switch (exp->tag){
        case NUMBER_EXP:
            printf(" %d", exp->op.numberExp);
            break;
            
        case STRING_EXP:
            printf(" %s", exp->op.stringExp);
            break;
            
        case BINARY_EXP:
            printf(" (%s", TOKEN_TYPE_STRING[exp->op.binaryExp.operator->type]);
            printExpressionAst(exp->op.binaryExp.left);
            printExpressionAst(exp->op.binaryExp.right);
            printf(")");
            break;
            
        case UNARY_EXP:
            printf(" (%s", TOKEN_TYPE_STRING[exp->op.unaryExp.operator->type]);
            printExpressionAst(exp->op.unaryExp.operand);
            printf(")");
            break;
            
        case GROUPING_EXP:
            printf(" (group");
            printExpressionAst(exp->op.groupingExp);
            printf(")");
            break;
            
        case IDENTIFIER_EXP:
            printf(" (identifier %s)", exp->op.identifierExp->lexeme);
            break;
    }
}

//Parser Section

/* Return the next Token  */
Token_t* advanceToken(void)
{
    //Are there further tokens, then increase the index
    if (scannerTokenIndex != parserTokenIndex)
        parserTokenIndex++;
    
    return Tokens[parserTokenIndex-1];
}

/* Return true if the TokenType of the current Token is equal type */
bool checkTokenType(TokenType_t type)
{
    //There is no token anymore. A check is not possible
    if (scannerTokenIndex == parserTokenIndex)
        return false;
    
    return Tokens[parserTokenIndex]->type == type;
}

/* Return true if the TokenType of the current Token is equal to one value of types */
bool matchTokens(TokenType_t types[], int n, bool freeTokenMemory)
{
    for (int i = 0; i < n; i++){
        if (checkTokenType(types[i])){
            if (freeTokenMemory)
                freeSpecificToken(advanceToken(), parserTokenIndex-1);
            else
                advanceToken();
            return true;
        }
    }
    return false;
}

/* Return the current Token if the TokenType is equal type.
 If not then report a parser error */
Token_t* consume(TokenType_t type, char *message)
{
    if (checkTokenType(type))
        return advanceToken();
    
    reportParserError(message, Tokens[parserTokenIndex]);
    return NULL;
}

/* Corresponding function to production Primary of cfg.
 Return created expression
 If there is an unexpected Token then report a parser error */
Expression_t* primary(void)
{
    if (DEBUG_QNICE)
        printf("primary\n");
    
    if (matchTokens((TokenType_t[]) {NUMBER}, 1, false)){
        
        Expression_t *numberExp = make_numberExp(atoi(Tokens[parserTokenIndex-1]->lexeme));
        freeSpecificToken(Tokens[parserTokenIndex-1], parserTokenIndex-1);
        return numberExp;
    }
    
    if (matchTokens((TokenType_t[]) {STRING}, 1, false)){
        
        Expression_t *stringExp = make_stringExp(Tokens[parserTokenIndex-1]->lexeme);
        freeSpecificToken(Tokens[parserTokenIndex-1], parserTokenIndex-1);
        return stringExp;
    }
    
    if (matchTokens((TokenType_t[]) {IDENTIFIER}, 1, false)){
        
        Expression_t *identifierExp = make_identifierExp(Tokens[parserTokenIndex-1]);
        freeSpecificToken(Tokens[parserTokenIndex-1], parserTokenIndex-1);
        return identifierExp;
    }
    
    reportParserError("Expected expression.", Tokens[parserTokenIndex]);
    return NULL;
}

/* Corresponding function to production Grouping of cfg.
 Return created expression */
Expression_t* grouping(void)
{
    if (DEBUG_QNICE)
        printf("grouping\n");
    
    if (matchTokens((TokenType_t[]) {LEFT_PAREN}, 1, true)){
        Expression_t *expr = orExpression();
        
        //Syntax Error
        if (consume(RIGHT_PAREN, "Expected ')' after expression.") == NULL){
            return NULL;
        }
        
        freeSpecificToken(Tokens[parserTokenIndex-1], parserTokenIndex-1);
        return make_groupingExp(expr);
    }
    return primary();
}

/* Corresponding function to production Unary of cfg.
 Return created expression */
Expression_t* unary(void)
{
    if (DEBUG_QNICE)
        printf("unary\n");
    
    if (matchTokens((TokenType_t[]) {PLUS, MINUS, NOT}, 3, false)){
        
        Token_t *operator = Tokens[parserTokenIndex-1];
        int operatorTokenIndex = parserTokenIndex-1;
        
        Expression_t *unaryExpr = make_unaryExp(operator, grouping());
        freeSpecificToken(operator, operatorTokenIndex);
        return unaryExpr;
    }
    return grouping();
}

/* Corresponding function to production Multiplication of cfg.
 Return created expression */
Expression_t* multiplication(void)
{
    if (DEBUG_QNICE)
        printf("multiplication\n");
    
    Expression_t *expr = unary();
    
    while (matchTokens((TokenType_t[]) {STAR, SLASH}, 2, false)){
        Token_t *operator = Tokens[parserTokenIndex-1];
        int operatorTokenIndex = parserTokenIndex-1;
        
        expr = make_binaryExp(expr, operator, unary());
        freeSpecificToken(operator, operatorTokenIndex);
    }
    return expr;
}

/* Corresponding function to production Addition of cfg.
 Return created expression */
Expression_t* addition(void)
{
    if (DEBUG_QNICE)
        printf("addition\n");
    
    Expression_t *expr = multiplication();
    
    while (matchTokens((TokenType_t[]) {PLUS, MINUS}, 2, false)){
        Token_t *operator = Tokens[parserTokenIndex-1];
        int operatorTokenIndex = parserTokenIndex-1;
        
        expr = make_binaryExp(expr, operator, multiplication());
        freeSpecificToken(operator, operatorTokenIndex);
    }
    return expr;
}

/* Corresponding function to production Comparison of cfg.
 Return created expression */
Expression_t* comparison(void)
{
    if (DEBUG_QNICE)
        printf("comparison\n");
    
    Expression_t *expr = addition();
    
    while (matchTokens((TokenType_t[]) {EQUAL, GREATER, LESS, NOT_EQUAL_LESS_GREATER, NOT_EQUAL_GREATER_LESS, GREATER_EQUAL, EQUAL_GREATER, LESS_EQUAL, EQUAL_LESS}, 9, false)){
        Token_t *operator = Tokens[parserTokenIndex-1];
        int operatorTokenIndex = parserTokenIndex-1;
        
        expr = make_binaryExp(expr, operator, addition());
        freeSpecificToken(operator, operatorTokenIndex);
    }
    return expr;
}

/* Corresponding function to production AndExpression of cfg.
 Return created expression */
Expression_t* andExpression(void)
{
    if (DEBUG_QNICE)
        printf("andExpression\n");
    
    Expression_t *expr = comparison();
    
    while (matchTokens((TokenType_t[]) { AND }, 1, false)){
        Token_t *operator = Tokens[parserTokenIndex-1];
        int operatorTokenIndex = parserTokenIndex-1;
        
        expr = make_binaryExp(expr, operator, comparison());
        freeSpecificToken(operator, operatorTokenIndex);
    }
    return expr;
}

/* Corresponding function to production OrExpression of cfg.
 Return created expression */
Expression_t* orExpression(void)
{
    if (DEBUG_QNICE)
        printf("orExpression\n");
    
    Expression_t *expr = andExpression();
    
    while (matchTokens((TokenType_t[]) { OR }, 1, false)){
        Token_t *operator = Tokens[parserTokenIndex-1];
        int operatorTokenIndex = parserTokenIndex-1;
        
        expr = make_binaryExp(expr, operator, andExpression());
        freeSpecificToken(operator, operatorTokenIndex);
    }
    return expr;
}

/* Corresponding function to production NextStmt of cfg.
 Return created Statement */
Statement_t* nextStmt(void)
{
    Expression_t *identifier = NULL;
    
    if (matchTokens((TokenType_t[]){ IDENTIFIER }, 1, false)){
        
        identifier = make_identifierExp(Tokens[parserTokenIndex-1]);
        freeSpecificToken(Tokens[parserTokenIndex-1], parserTokenIndex-1);
    }
    
    Statement_t *a = (Statement_t*) malloc(sizeof(Statement_t));
    
    if (a == NULL){
        printf("make_NextStmt: malloc failed!!\n");
        freeExpression(identifier);
        return NULL;
    }
    
    a->tag = NEXT_STMT;
    a->value.expression = identifier;
    a->lineNumber = currentLineNumber;
    
    return a;
}

/* Corresponding function to production ForStmt of cfg.
 Return created Statement */
Statement_t* forStmt(void)
{
    Statement_t *initializer = varDecl();
    
    if (initializer == NULL)
        return NULL;
    
    switch (initializer->value.varDeclStmt.initializer->tag){
        case STRING_EXP:
            reportParserError("Type mismatch. Expected number.", initializer->value.varDeclStmt.name);
            freeStatement(initializer);
            return NULL;
            
        case IDENTIFIER_EXP:
            if (initializer->value.varDeclStmt.initializer-> op.identifierExp->lexeme[strlen(initializer->value.varDeclStmt.initializer->op.identifierExp->lexeme) - 1] == '$'){
                reportParserError("Type mismatch. Expected number.", initializer->value.varDeclStmt.initializer-> op.identifierExp);
                freeStatement(initializer);
                return NULL;
            }
            break;
        default:
            break;
    }
    
    if (consume(TO, "Expected 'TO' after for clauses.") == NULL){
        freeStatement(initializer);
        return NULL;
    }
    
    
    freeSpecificToken(Tokens[parserTokenIndex-1], parserTokenIndex-1);
    
    Expression_t *loopEnd = orExpression();
    
    Expression_t *increment = NULL;
    
    if (matchTokens((TokenType_t[]){ STEP }, 1, true))
        increment = orExpression();
    else
        increment = make_numberExp(1);
    
    if (initializer == NULL || loopEnd == NULL || increment == NULL){
        freeStatement(initializer);
        freeExpression(loopEnd);
        freeExpression(increment);
        return NULL;
    }
    
    
    Statement_t *a = (Statement_t*) malloc(sizeof(Statement_t));
    
    if (a == NULL){
        printf("make_ForStmt: malloc failed!!\n");
        freeStatement(initializer);
        freeExpression(loopEnd);
        freeExpression(increment);
        return NULL;
    }
    
    a->tag = FOR_STMT;
    a->value.forStmt.initializer = initializer;
    a->value.forStmt.loopEnd = loopEnd;
    a->value.forStmt.increment = increment;
    a->lineNumber = currentLineNumber;
    
    return a;
}

/* Corresponding function to production GotoStmt of cfg.
 Return created Statement */
Statement_t* gotoStmt(void)
{
    Token_t *linenumber = consume(NUMBER, "Expected linenumber after 'GOTO'.");
    
    if (linenumber == NULL)
        return NULL;
    
    Expression_t *linenumberExp = make_numberExp(atoi(linenumber->lexeme));
    
    if (linenumberExp == NULL)
        return NULL;
    
    Statement_t *a = (Statement_t*) malloc(sizeof(Statement_t));
    
    if (a == NULL){
        printf("make_NextStmt: malloc failed!!\n");
        return NULL;
    }
    
    a->tag = GOTO_STMT;
    a->value.expression = linenumberExp;
    a->lineNumber = currentLineNumber;
    
    freeSpecificToken(linenumber, parserTokenIndex-1);
    
    return a;
}

/* Corresponding function to production PrintStmt of cfg.
 Return created Statement */
Statement_t* printStmt(Expression_t *exp)
{
    if (exp == NULL)
        return NULL;
    
    Statement_t *a = (Statement_t*) malloc(sizeof(Statement_t));
    
    if (a == NULL){
        printf("make_printStmt: malloc failed!!\n");
        return NULL;
    }
    
    a->tag = PRINT_STMT;
    a->value.expression = exp;
    a->lineNumber = currentLineNumber;
    
    return a;
}

/* Corresponding function to production IfStmt of cfg.
 Return created Statement */
Statement_t* ifStmt(void)
{
    Expression_t *exp = orExpression();
    
    if (exp == NULL)
        return NULL;
    
    //TODO Verarbeitung von weiteren Statements die per Doppelpunkt angehaengt sind.
    if (matchTokens((TokenType_t[]) { THEN }, 1, false) ||
        matchTokens((TokenType_t[]) { GOTO }, 1, false)){
        
        //The GOTO is needed for correct parsing of the statement
        if (parserTokenIndex > 0 && Tokens[parserTokenIndex-1]->type == GOTO)
            parserTokenIndex -= 1;
        
        //TODO Im Online-Interpreter sind auch Declarations im Then moeglich.
        Statement_t *thenBranch = statement();
        
        if (exp == NULL || thenBranch == NULL)
            return NULL;
        
        Statement_t *a = (Statement_t*) malloc(sizeof(Statement_t));
        
        if (a == NULL){
            printf("make_ifStmt: malloc failed!!\n");
            freeStatement(thenBranch);
            freeExpression(exp);
            return NULL;
        }
        
        a->tag = IF_STMT;
        a->value.ifStmt.expression = exp;
        a->value.ifStmt.thenBranch = thenBranch;
        a->lineNumber = currentLineNumber;
        
        return a;
    }
    
    reportParserError("Expected 'THEN' or 'GOTO' after if condition.", Tokens[parserTokenIndex]);
    freeExpression(exp);
    
    return NULL;
}

/* Creates a statement to exp and return this statement */
Statement_t* expressionStmt(Expression_t *exp)
{
    if (exp == NULL)
        return NULL;
    
    Statement_t *a = (Statement_t*) malloc(sizeof(Statement_t));
    
    if (a == NULL){
        printf("make_expressionStmt: malloc failed!!\n");
        return NULL;
    }
    
    a->tag = EXPRESSION_STMT;
    a->value.expression = exp;
    a->lineNumber = currentLineNumber;
    
    return a;
}

/* Corresponding function to production Statement of cfg.
 Return created Statement */
Statement_t* statement(void)
{
    if (matchTokens((TokenType_t[]){ FOR }, 1, true))
        return forStmt();
    
    if (matchTokens((TokenType_t[]){ IF }, 1, true))
        return ifStmt();
    
    if (matchTokens((TokenType_t[]){ PRINT }, 1, true))
        return printStmt(orExpression());
    
    if (matchTokens((TokenType_t[]){ NEXT }, 1, true))
        return nextStmt();
    
    if (matchTokens((TokenType_t[]){ GOTO }, 1, true))
        return gotoStmt();
    
    return expressionStmt(orExpression());
}

/* Corresponding function to production VarDeclStmt of cfg.
 Return created Statement */
Statement_t* varDecl(void)
{
    if (DEBUG_QNICE)
        printf("varDecl\n");
    
    Token_t *name = consume(IDENTIFIER, "Expected identifier.");
    
    int identifierTokenIndex = parserTokenIndex-1;
    
    if (name != NULL &&
        consume(EQUAL, "Expect equal after variable name.") != NULL){
        
        freeSpecificToken(Tokens[parserTokenIndex-1], parserTokenIndex-1);
        
        Expression_t *initializer = orExpression();
        
        if (name == NULL || initializer == NULL)
            return NULL;
        
        Statement_t *varDecl = (Statement_t*) malloc(sizeof(Statement_t));
        
        if (varDecl == NULL){
            printf("make_varDecl: malloc failed!!\n");
            return NULL;
        }
        
        varDecl->tag = VAR_DECL_STMT;
        varDecl->value.varDeclStmt.name = tokenDup(name);
        varDecl->value.varDeclStmt.initializer = initializer;
        varDecl->lineNumber = currentLineNumber;
        
        if (varDecl->value.varDeclStmt.name == NULL ||
            varDecl->value.varDeclStmt.initializer == NULL){
            
            freeStatement(varDecl);
            freeSpecificToken(name, identifierTokenIndex);
            return NULL;
        }
        
        //TODO Unterscheidung zwischen Integer und Real -> S. 18
        bool isStringName = name->lexeme[strlen(name->lexeme) - 1] == '$';
        
        switch (varDecl->value.varDeclStmt.initializer->tag){
            case STRING_EXP:
                if (!isStringName){
                    reportParserError("Type mismatch. Expected identifier with $ at the end.", name);
                    freeStatement(varDecl);
                    freeSpecificToken(name, identifierTokenIndex);
                    return NULL;
                }
                break;
                
            case IDENTIFIER_EXP:
                if (varDecl->value.varDeclStmt.initializer->op.identifierExp->lexeme[strlen(varDecl->value.varDeclStmt.initializer->op.identifierExp->lexeme) - 1] == '$'){
                    if (!isStringName){
                        reportParserError("Type mismatch. Expected initializer without $ at the end.", varDecl->value.varDeclStmt.initializer->op.identifierExp);
                        freeStatement(varDecl);
                        freeSpecificToken(name, identifierTokenIndex);
                        return NULL;
                    }
                }
                else{
                    if (isStringName){
                        reportParserError("Type mismatch. Expected initializer with $ at the end.", varDecl->value.varDeclStmt.initializer->op.identifierExp);
                        freeStatement(varDecl);
                        freeSpecificToken(name, identifierTokenIndex);
                        return NULL;
                    }
                }
                break;
            case NUMBER_EXP:
                if (isStringName){
                    reportParserError("Type mismatch. Expected identifier without $ at the end.", name);
                    freeStatement(varDecl);
                    freeSpecificToken(name, identifierTokenIndex);
                    return NULL;
                }
                break;
            case BINARY_EXP:
            case UNARY_EXP:
            case GROUPING_EXP:
                //Have to be checked during the interpretation.
                break;
            default:
                reportParserError("Invalid initializer tag for a declaration.", name);
                freeStatement(varDecl);
                freeSpecificToken(name, identifierTokenIndex);
                return NULL;
        }
        
        freeSpecificToken(name, identifierTokenIndex);
        
        return varDecl;
    }
    return NULL;
}

Statement_t* declaration(void)
{
    if (DEBUG_QNICE)
        printf("declaration\n");
    
    //TODO Optionales LET
    if (matchTokens((TokenType_t[]){ LET }, 1, true))
        return varDecl();
    
    return statement();
}

/* Corresponding function to production Line of cfg.
 Return created Statement */
Statement_t* line(void)
{
    if (DEBUG_QNICE)
        printf("line\n");
    
    Token_t *lineNumberToken = consume(NUMBER, "Expected a line number at the beginning of the line.");
    
    if (lineNumberToken != NULL){
        
        currentLineNumber = atoi(lineNumberToken->lexeme);
        
        freeSpecificToken(lineNumberToken, parserTokenIndex-1);
        
        if (matchTokens((TokenType_t[]){ REM }, 1, true)){
            isComment = true;
            
            return NULL;
        }
        
        return declaration();
    }
    return NULL;
}

/* Corresponding function to production Program of cfg.
 Parse all created tokens and create statements */
void parse(void)
{
    program = (Statement_t**) malloc(statement_max_count * sizeof(Statement_t*));
    
    if (program == NULL){
        printf("Parse: malloc failed!!!\n");
        return;
    }
    
    while(scannerTokenIndex != parserTokenIndex){
        isComment = false;
        
        Statement_t *stmt = line();
        
        if (stmt == NULL){
            if (isComment)
                continue;
            
            break;
        }
        
        program[stmtIndex] = stmt;
        
        stmtIndex++;
        
        if (stmtIndex == statement_max_count){
            statement_max_count += STATEMENT_INCREMENT;
            program = (Statement_t**) realloc(program, statement_max_count * sizeof(Statement_t*));
            
            if (program == NULL){
                printf("Parse: realloc failed!!!\n");
                return;
            }
        }
    }
    freeTokens();
    
    //Sort statements in order of their linenumber
    qsort(program, stmtIndex, sizeof(Statement_t *), compareLineNumberOfStatements);
}

//Interpreter Section

/* Execute the operator  on the two ExpressionResults of type string
   Returns the result as ExpressionResult 
   The function includes an semantic check. */
ExpResult_t* interpretBinaryStringExpression(ExpResult_t *left, ExpResult_t *right, Token_t *operatorToken, int currentLineNumber)
{
    if (DEBUG_QNICE)
        printf("interpretBinaryStringExpression\n");
    
    ExpResult_t *result = (ExpResult_t*) malloc(sizeof(ExpResult_t));
    
    if (result == NULL){
        printf("interpretBinaryStringExpression: malloc failed!!\n");
        return NULL;
    }
    
    result->tag = NUMBER_RESULT;
    
    int lenLeft = 0;
    int lenRight = 0;
    
    switch(operatorToken->type){
        case EQUAL:
            if (strcmp(left->value.stringResult, right->value.stringResult) == 0){
                result->value.numberResult = 1;
            }
            else{
                result->value.numberResult = 0;
            }
            break;
            
        case GREATER:
            if (strcmp(left->value.stringResult, right->value.stringResult) > 0){
                result->value.numberResult = 1;
            }
            else{
                result->value.numberResult = 0;
            }
            break;
            
        case LESS:
            if (strcmp(left->value.stringResult, right->value.stringResult) < 0){
                result->value.numberResult = 1;
            }
            else{
                result->value.numberResult = 0;
            }
            break;
            
        case NOT_EQUAL_LESS_GREATER:
        case NOT_EQUAL_GREATER_LESS:
            if (strcmp(left->value.stringResult, right->value.stringResult) != 0){
                result->value.numberResult = 1;
            }
            else{
                result->value.numberResult = 0;
            }
            break;
            
        case GREATER_EQUAL:
        case EQUAL_GREATER:
            if (strcmp(left->value.stringResult, right->value.stringResult) >= 0){
                result->value.numberResult = 1;
            }
            else{
                result->value.numberResult = 0;
            }
            break;
            
        case LESS_EQUAL:
        case EQUAL_LESS:
            if (strcmp(left->value.stringResult, right->value.stringResult) <= 0){
                result->value.numberResult = 1;
            }
            else{
                result->value.numberResult = 0;
            }
            break;
            
        case PLUS:
            if (DEBUG_QNICE)
                printf("interpretBinaryStringExpression: operatorToken->type PLUS\n");
            
            result->tag = STRING_RESULT;
            
            lenLeft = (int) strlen(left->value.stringResult);
            lenRight = (int) strlen(right->value.stringResult);
            
            result->value.stringResult = NULL;
            
            if (lenLeft + lenRight < 3)
                break;
            
            if (lenLeft == 0 || lenRight == 0)
                result->value.stringResult = malloc((lenLeft + lenRight + 1) * sizeof(char));
            else
                result->value.stringResult = malloc((lenLeft + lenRight - 1) * sizeof(char));
            
            if (result->value.stringResult == NULL){
                printf("interpretBinaryStringExpression: malloc failed!!!\n");
                return NULL;
            }
            
            if (lenLeft >= 3)
                strncpy(result->value.stringResult, left->value.stringResult, lenLeft - 1);
            else{
                strncpy(result->value.stringResult, "\"", 1);
                lenLeft = 2;
            }
            
            
            if (lenRight >= 3)
                strncpy(result->value.stringResult + lenLeft - 1, right->value.stringResult + 1, lenRight - 1);
            else{
                strncpy(result->value.stringResult + lenLeft - 1, "\"", 1);
                lenRight = 2;
            }
            
            strncpy(result->value.stringResult + lenLeft + lenRight - 2, "\0", 1);
            break;
            
        default:
            reportRuntimeError(currentLineNumber, TOKEN_TYPE_STRING[operatorToken->type], "Type mismatch. Invalid operands.");
            freeExpressionResult(result);
            return NULL;
    }
    return result;
}

/* Execute the operator  on the two ExpressionResults of type number
 Returns the result as ExpressionResult 
 The function includes an semantic check. */
ExpResult_t* interpretBinaryNumberExpression(ExpResult_t *left, ExpResult_t *right, Token_t *operatorToken, int currentLineNumber)
{
    ExpResult_t *result = (ExpResult_t*) malloc(sizeof(ExpResult_t));
    
    if (result == NULL){
        printf("interpretBinaryNumberExpression: malloc failed!!!\n");
        return NULL;
    }
    
    result->tag = NUMBER_RESULT;
    
    switch(operatorToken->type){
        case EQUAL:
            result->value.numberResult = left->value.numberResult == right->value.numberResult;
            break;
            
        case GREATER:
            result->value.numberResult = left->value.numberResult > right->value.numberResult;
            break;
            
        case LESS:
            result->value.numberResult = left->value.numberResult < right->value.numberResult;
            break;
            
        case NOT_EQUAL_LESS_GREATER:
        case NOT_EQUAL_GREATER_LESS:
            result->value.numberResult = left->value.numberResult != right->value.numberResult;
            break;
            
        case GREATER_EQUAL:
        case EQUAL_GREATER:
            result->value.numberResult = left->value.numberResult >= right->value.numberResult;
            break;
            
        case LESS_EQUAL:
        case EQUAL_LESS:
            result->value.numberResult = left->value.numberResult <= right->value.numberResult;
            break;
            
        case MINUS:
            result->value.numberResult = left->value.numberResult - right->value.numberResult;
            break;
            
        case PLUS:
            result->value.numberResult = left->value.numberResult + right->value.numberResult;
            break;
            
        case SLASH:
            result->value.numberResult = left->value.numberResult / right->value.numberResult;
            break;
            
        case STAR:
            result->value.numberResult = left->value.numberResult * right->value.numberResult;
            break;
            
        case AND:
            result->value.numberResult = left->value.numberResult && right->value.numberResult;
            break;
            
        case OR:
            result->value.numberResult = left->value.numberResult || right->value.numberResult;
            break;
            
        default:
            reportRuntimeError(currentLineNumber, TOKEN_TYPE_STRING[operatorToken->type], "Type mismatch. Invalid operands.");
            freeExpressionResult(result);
            return NULL;
    }
    return result;
}

/* Interpretation of an Expression
   Returns an ExpressionResult */
ExpResult_t* interpretExpression(Expression_t *exp)
{
    if (DEBUG_QNICE)
        printf("interpretExpression\n");
    
    ExpResult_t *result = NULL;
    ExpResult_t *left;
    ExpResult_t *right;
    Nlist_t *variableResult = NULL;
    
    switch (exp->tag){
        case NUMBER_EXP:
            if (DEBUG_QNICE)
                printf("NUMBER_EXP\n");
            
            result = (ExpResult_t*) malloc(sizeof(ExpResult_t));
            
            if (result == NULL){
                printf("interpretExpression: malloc failed!!!\n");
                return NULL;
            }
            
            result->tag = NUMBER_RESULT;
            result->value.numberResult = exp->op.numberExp;
            
            return result;
            
        case STRING_EXP:
            if (DEBUG_QNICE)
                printf("STRING_EXP\n");
            
            result = (ExpResult_t*) malloc(sizeof(ExpResult_t));
            
            if (result == NULL){
                printf("interpretExpression: malloc failed!!!\n");
                return NULL;
            }
            
            result->tag = STRING_RESULT;
            result->value.stringResult = strdup(exp->op.stringExp);
            
            return result->value.stringResult == NULL ? NULL : result;
            
        case BINARY_EXP:
            if (DEBUG_QNICE)
                printf("BINARY_EXP\n");
            
            left = interpretExpression(exp->op.binaryExp.left);
            right = interpretExpression(exp->op.binaryExp.right);
            
            if (DEBUG_QNICE)
                printf("BINARY_EXP: interpret left and right\n");
            
            if (left == NULL || right == NULL){
                if (DEBUG_QNICE)
                    printf("BINARY_EXP: left or right == NULL!!\n");
                
                freeExpressionResult(left);
                freeExpressionResult(right);
                return NULL;
            }
            
            if (left->tag == NUMBER_RESULT && right->tag == NUMBER_RESULT){
                if (DEBUG_QNICE)
                    printf("BINARY_EXP: left and right are NUMBER_RESULT\n");
                
                result = interpretBinaryNumberExpression(left, right, exp->op.binaryExp.operator, exp->lineNumber);
            }
            else if (left->tag == STRING_RESULT && right->tag == STRING_RESULT){
                if (DEBUG_QNICE)
                    printf("BINARY_EXP: left and right are STRING_RESULT\n");
                
                result = interpretBinaryStringExpression(left, right, exp->op.binaryExp.operator, exp->lineNumber);
            }
            else{
                if (DEBUG_QNICE)
                    printf("BINARY_EXP: Runtime Error\n");
                
                reportRuntimeError(exp->lineNumber, TOKEN_TYPE_STRING[exp->op.binaryExp.operator->type], "Type mismatch. Operands must be two numbers or two strings.");
                result = NULL;
            }
            
            if (DEBUG_QNICE)
                printf("BINARY_EXP: Cleaning\n");
            
            freeExpressionResult(left);
            freeExpressionResult(right);
            
            return result;
            
        case UNARY_EXP:
            if (DEBUG_QNICE)
                printf("UNARY_EXP\n");
            
            right = interpretExpression(exp->op.unaryExp.operand);
            
            //Only for numbers possible
            if (right->tag != NUMBER_RESULT){
                reportRuntimeError(exp->op.unaryExp.operand->lineNumber, TOKEN_TYPE_STRING[exp->op.unaryExp.operator->type], "Type mismatch. Operand must be a number.");
                
                freeExpressionResult(right);
                return NULL;
            }
            
            result = (ExpResult_t*) malloc(sizeof(ExpResult_t));
            
            if (result == NULL){
                printf("interpretExpression: malloc failed!!!\n");
                freeExpressionResult(right);
                return NULL;
            }
            
            result->tag = NUMBER_RESULT;
            
            switch (exp->op.unaryExp.operator->type){
                case MINUS:
                    if (right->tag == NUMBER_RESULT){
                        result->value.numberResult = - right->value.numberResult;
                    }
                    break;
                    
                case PLUS:
                    if (right->tag == NUMBER_RESULT){
                        result->value.numberResult = + right->value.numberResult;
                    }
                    break;
                    
                case NOT:
                    if (right->tag == NUMBER_RESULT){
                        if (right->value.numberResult == 0){
                            result->value.numberResult = 1;
                        }
                        else{
                            result->value.numberResult = 0;
                        }
                    }
                    break;
                    
                default:
                    freeExpressionResult(result);
                    break;
            }
            
            freeExpressionResult(right);
            
            return result;
            
        case GROUPING_EXP:
            if (DEBUG_QNICE)
                printf("GROUPING_EXP\n");
            
            return interpretExpression(exp->op.groupingExp);
            
        case IDENTIFIER_EXP:
            if (DEBUG_QNICE)
                printf("IDENTIFIER_EXP\n");
            
            variableResult = lookup(exp->op.identifierExp->lexeme);
            
            if (DEBUG_QNICE)
                printf("IDENTIFIER_EXP: lookup finished\n");
            
            if (variableResult != NULL){
                if (DEBUG_QNICE)
                    printf("IDENTIFIER_EXP: variableResult != NULL\n");
                
                return expResultDub(variableResult->expResult);
            }
            else{
                //Used identifier is undeclared
                result = (ExpResult_t*) malloc(sizeof(ExpResult_t));
                
                if (result == NULL){
                    printf("interpretExpression: malloc failed!!!\n");
                    return NULL;
                }
                
                //Is string Identifier?
                if (exp->op.identifierExp->lexeme[strlen(exp->op.identifierExp->lexeme) - 1] == '$'){
                    result->tag = STRING_RESULT;
                    result->value.stringResult = strdup("");
                }
                else{
                    result->tag = NUMBER_RESULT;
                    result->value.numberResult = 0;
                }
                return result;
            }
            break;
    }
    return NULL;
}

/* Interpretation of an goto statement */
void interpretGotoStatement(Statement_t *statement)
{
    //Looking for linenumber of the GOTO
    for (int i = 0; i < stmtIndex; i++){
        if (program[i]->lineNumber == statement->value.expression->op.numberExp){
            interpretedStmtIndex = i - 1;
            return;
        }
    }
    
    reportRuntimeError(statement->lineNumber, "GOTO", "Undefined linenumber.");
}

/* Interpretation of an for statement */
void interpretForStatement(Statement_t *forStatement)
{
    interpretStatement(forStatement->value.forStmt.initializer);
    
    Nlist_t *loopIndex = lookup(forStatement->value.forStmt.initializer->value.varDeclStmt.name->lexeme);
    
    if (loopIndex == NULL)
        return;
    
    if (loopIndex->expResult->tag == STRING_RESULT){
        reportRuntimeError(forStatement->lineNumber, forStatement->value.forStmt.initializer->value.varDeclStmt.name->lexeme, "Type mismatch. Expected number.");
        return;
    }
    
    ExpResult_t *result = interpretExpression(forStatement->value.forStmt.loopEnd);
    
    if (result->tag != NUMBER_RESULT){
        reportRuntimeError(forStatement->lineNumber, "after TO", "Type mismatch. Expected number.");
        freeExpressionResult(result);
        return;
    }
    
    int loopEnd = result->value.numberResult;
    freeExpressionResult(result);
    
    if (hadRuntimeError)
        return;
    
    result = interpretExpression(forStatement->value.forStmt.increment);
    
    if (result->tag != NUMBER_RESULT){
        reportRuntimeError(forStatement->lineNumber, "after STEP", "Type mismatch. Expected number.");
        freeExpressionResult(result);
        return;
    }
    
    int increment = result->value.numberResult;
    freeExpressionResult(result);
    
    if (hadRuntimeError)
        return;
    
    int j = interpretedStmtIndex + 1;
    
    //It exists only one statement (the for statement)
    if (j == stmtIndex)
        return;
    
    int tmpInterpretedStmtIndex = 0;
    bool foundCorrespondingNext = false;
    
    while (loopIndex->expResult->value.numberResult <= loopEnd){
        foundCorrespondingNext = false;
        
        //Interpret first statement after FOR
        j = interpretedStmtIndex + 1;
        
        //Until a NEXT is read interpret every statement
        while (program[j]->tag != NEXT_STMT){
            
            //There is a inner loop. The statements of the inner loop are interpreted recursive.
            if (program[j]->tag == FOR_STMT){
                tmpInterpretedStmtIndex = interpretedStmtIndex;
                interpretedStmtIndex = j;
            }
            
            interpretStatement(program[j]);
            
            if (program[j]->tag == FOR_STMT){
                //Last statement belongs to inner loop. There is no statement left and no NEXT found for current loop.
                if (interpretedStmtIndex == stmtIndex)
                    return;
                
                //Continue with the last statement (NEXT) of the inner loop
                j = interpretedStmtIndex;
                interpretedStmtIndex = tmpInterpretedStmtIndex;
                
                //Check if the NEXT corresponds to current loop.
                if (program[j]->tag == NEXT_STMT &&
                    program[j]->value.expression != NULL &&
                    strcmp(forStatement->value.forStmt.initializer->value.varDeclStmt.name->lexeme, program[j]->value.expression->op.identifierExp->lexeme) == 0){
                    
                    //Identifier of NEXT corresponds to identifier of FOR
                    foundCorrespondingNext = true;
                    break;
                }
            }
            
            j++;
            
            //Found no NEXT
            if (j == stmtIndex){
                interpretedStmtIndex = j;
                return;
            }
        }
        
        //Found NEXT
        if(!foundCorrespondingNext && program[j]->value.expression != NULL &&
           strcmp(forStatement->value.forStmt.initializer->value.varDeclStmt.name->lexeme, program[j]->value.expression->op.identifierExp->lexeme) != 0)
            break;
            
        loopIndex->expResult->value.numberResult += increment;
    }
    
    //Continue with the current statement to check if the NEXT corresponds to a outer loop.
    interpretedStmtIndex = j;
}

/* Interpretation of an if statement */
void interpretIfStatement(Statement_t *ifStatement)
{
    ExpResult_t *condition = interpretExpression(ifStatement->value.ifStmt.expression);
    
    //If the condition is not an empty string or greater 0 execute the thenBranch
    if ((condition->tag == STRING_RESULT && strcmp(condition->value.stringResult, "\"\"") != 0) ||
        (condition->tag == NUMBER_RESULT && condition->value.numberResult > 0)){
        
        interpretStatement(ifStatement->value.ifStmt.thenBranch);
    }
    freeExpressionResult(condition);
}

/* Interpretation of an variable declaration statement */
void interpretVarDeclStatement(Statement_t *statement)
{
    ExpResult_t *result = interpretExpression(statement->value.varDeclStmt.initializer);
    
    if (result == NULL)
        return;
    
    //TODO Unterscheidung zwischen Integer und Real -> S. 18
    bool isStringName = statement->value.varDeclStmt.name->lexeme[strlen(statement->value.varDeclStmt.name->lexeme) - 1] == '$';
    
    switch (result->tag){
        case STRING_RESULT:
            if (!isStringName){
                reportRuntimeError(statement->lineNumber, statement->value.varDeclStmt.name->lexeme, "Type mismatch. Expected identifier with $ at the end.");
                return;
            }
            break;
            
        case NUMBER_RESULT:
            if (isStringName){
                reportRuntimeError(statement->lineNumber, statement->value.varDeclStmt.name->lexeme, "Type mismatch. Expected identifier without $ at the end.");
                return;
            }
            break;
            
        default:
            reportRuntimeError(statement->lineNumber, statement->value.varDeclStmt.name->lexeme, "Invalid initializer tag for a declaration.");
            return;
    }
    
    install(statement->value.varDeclStmt.name->lexeme, result);
    
    freeExpressionResult(result);
}

/* Print the interpreted result of a print statement */
void printExpressionResult(ExpResult_t *result)
{
    if (DEBUG_QNICE)
        printf("Print Expression\n");
    
    if (result != NULL){
        if (result->tag == NUMBER_RESULT){
            if (DEBUG_QNICE)
                printf("Print Number\n");
            
            printf("> %d", result->value.numberResult);
        }
        else if (result->tag == STRING_RESULT){
            if (DEBUG_QNICE)
                printf("Print String\n");
            
            if (result->value.stringResult != NULL)
                printf("> %s", result->value.stringResult);
            else
                printf(">");
        }
        
        freeExpressionResult(result);
    }
    
    printf("\n");
    
    if (DEBUG_QNICE)
        printf("Finished Print Expression\n");
}

/* Interpretation of an statement */
void interpretStatement(Statement_t *statement)
{
    if (DEBUG_QNICE)
        printf("Interpret..\n");
    
    switch (statement->tag){
        case EXPRESSION_STMT:
            if (DEBUG_QNICE)
                printf("EXPRESSION_STMT\n");
            //Is in no case necessary at the moment.
            //freeExpressionResult(interpretExpression(statement->value.expression));
            break;
            
        case PRINT_STMT:
            if (DEBUG_QNICE)
                printf("PRINT_STMT\n");
            
            printExpressionResult(interpretExpression(statement->value.expression));
            break;
            
        case VAR_DECL_STMT:
            if (DEBUG_QNICE)
                printf("VAR_DECL_STMT\n");
            
            interpretVarDeclStatement(statement);
            break;
            
        case IF_STMT:
            interpretIfStatement(statement);
            break;
            
        case FOR_STMT:
            interpretForStatement(statement);
            break;
            
        case NEXT_STMT:
            reportRuntimeError(statement->lineNumber, "NEXT", "NEXT without FOR.");
            break;
            
        case GOTO_STMT:
            interpretGotoStatement(statement);
            break;
    }
}

/* Interpretation of the table of statements */
void interpret(void)
{
    if (DEBUG_QNICE)
        printf("Start interpreting\n");
    
    for (interpretedStmtIndex = 0; interpretedStmtIndex < stmtIndex; interpretedStmtIndex++){
        interpretStatement(program[interpretedStmtIndex]);
        
        //TODO Muss in dem Fall noch Speicher freigegeben werden?
        if (hadRuntimeError || hadError)
            break;
    }
    
    if (DEBUG_QNICE)
        printf("Interpreting finished\n");
    
    freeStatements();
}

/* Parse and intpret the whole input */
void interpretInput(void)
{
    //Parse the grammar top down.
    
    //TODO Bei Fehlern nicht das komplette Programm verlassen.
    if (hadError)
        return;
    
    if (DEBUG_QNICE)
        printf("Start parsing\n");
    
    parse();
    
    if (DEBUG_QNICE)
        printf("Parsing finished\n");
    
    if (hadError || program == NULL){
        if (program != NULL)
            freeStatements();
        
        return;
    }
    
    //printExpression(programm);
    interpret();
    
    //printf("ScannerTokenIndex: %d\n", scannerTokenIndex);
    //printf("StatementIndex: %d\n", stmtIndex);
    
    if (hadRuntimeError)
        return;
    
    scannerTokenIndex = 0;
    parserTokenIndex = 0;
    stmtIndex = 0;
    statement_max_count = 50;
    token_max_count = 50;
    interpretedStmtIndex = 0;
    
    Tokens = (Token_t**) malloc(token_max_count * sizeof(Token_t*));
    
    if (Tokens == NULL){
        printf("main: malloc failed!!!\n");
        hadError = true;
        return;
    }
    
    scanAndInterpretInput();
}

/* Read current input line and create tokens */
void scanAndInterpretInput(void)
{
    currentInputRow = malloc(sizeof(char) * ROW_LENGTH);
    
    while (fgets(currentInputRow, ROW_LENGTH, stdin) != NULL){
        
        if (strncmp(currentInputRow+strlen(currentInputRow) - 1, "\n", 1) != 0){
            printf("\nThe current line has more than %d characters! It was not possible to process this line:\n", ROW_LENGTH);
            printf("%s\n", currentInputRow);
            
            //Read the rest of the line
            while (strncmp(currentInputRow+strlen(currentInputRow) - 1, "\n", 1) != 0)
                if (fgets(currentInputRow, ROW_LENGTH, stdin) == NULL)
                    break;
            continue;
        }
        
        if (strncmp(TOKEN_TYPE_STRING[RUN], currentInputRow, strlen(currentInputRow) - 1) == 0){
            free(currentInputRow);
            currentInputRow = NULL;
            
            interpretInput();
            break;
        }
        
        if (strncmp(PRINTTOKENS, currentInputRow, strlen(currentInputRow) - 1) == 0){
            free(currentInputRow);
            currentInputRow = NULL;
            
            printTokens();
            break;
        }
        
        //Wenn ein EXIT eingegeben wird, dann wird die Eingabe verlassen
        if (strncmp(EXIT, currentInputRow, strlen(currentInputRow) - 1) == 0){
            printf("Thank you for using the APPLESOFT BASIC Interpreter!\n");
            break;
        }
            
        scanCurrentLine();
        
        if (hadError)
            break;
    }
    
    free(currentInputRow);
    currentInputRow = NULL;
    
    if (!hadError)
        freeTokens();
}

int main(int argc, char** argv)
{
    printf("\nAPPLESOFT BASIC Interpreter - Version 0.4 (Christoph Beinert, November 2017)\n");
    
    for (int i = 0; i < ROW_LENGTH; i++){
        putchar('-');
    }
    
    putchar('\n');
    
    Tokens = (Token_t**) malloc(token_max_count * sizeof(Token_t*));
    
    if (Tokens == NULL){
        printf("main: malloc failed!!!\n");
        return 65;
    }
    
    scanAndInterpretInput();
    
    for (int i = 0; i < HASHSIZE; i++){
        freeHashTabEntry(hashtab[i]);
    }
    
    if (hadRuntimeError)
        return(70);
    
    if (hadError)
        return(65);
    
    return (EXIT_SUCCESS);
}

/* Create a duplicate of a Token */
Token_t* tokenDup(Token_t *token)
{
    if (DEBUG_QNICE)
        printf("tokenDup\n");
    
    Token_t *newToken = (Token_t*) malloc(sizeof(Token_t));
    
    if (newToken == NULL){
        printf("copyToken: malloc failed!!\n");
        return NULL;
    }
    
    newToken->type = token->type;
    newToken->lexeme = NULL;
    
    if (token->lexeme == NULL)
        return newToken;
    
    newToken->lexeme = strdup(token->lexeme);
    
    return newToken->lexeme == NULL ? NULL : newToken;
}

/* Deallocation of a symbol table entry */
void freeHashTabEntry(Nlist_t *np)
{
    if (np == NULL)
        return;
    
    free(np->name);
    freeExpressionResult(np->expResult);
    np->next = NULL;
    free(np);
    np = NULL;
}

/* Deallocation of a ExpressionResult */
void freeExpressionResult(ExpResult_t *result)
{
    if (result == NULL)
        return;
    
    switch(result->tag){
        case STRING_RESULT:
            free(result->value.stringResult);
            result->value.stringResult = NULL;
            break;
            
        case NUMBER_RESULT:
            break;
    }
    
    free(result);
    result = NULL;
}

/* Deallocation of a Expression */
void freeExpression(Expression_t *exp)
{
    //The expression of NEXT could be NULL
    if (exp == NULL)
        return;
    
    switch(exp->tag){
        case STRING_EXP:
            free(exp->op.stringExp);
            exp->op.stringExp = NULL;
            break;
            
        case BINARY_EXP:
            freeExpression(exp->op.binaryExp.left);
            freeToken(exp->op.binaryExp.operator);
            freeExpression(exp->op.binaryExp.right);
            break;
            
        case UNARY_EXP:
            freeToken(exp->op.unaryExp.operator);
            freeExpression(exp->op.unaryExp.operand);
            break;
            
        case GROUPING_EXP:
            freeExpression(exp->op.groupingExp);
            break;
            
        case IDENTIFIER_EXP:
            freeToken(exp->op.identifierExp);
            break;
            
        case NUMBER_EXP:
            break;
    }
    
    free(exp);
    exp = NULL;
}

/* Deallocation of a Statement */
void freeStatement(Statement_t *stmt)
{
    switch(stmt->tag){
        case EXPRESSION_STMT:
        case PRINT_STMT:
        case NEXT_STMT:
        case GOTO_STMT:
            freeExpression(stmt->value.expression);
            break;
            
        case VAR_DECL_STMT:
            freeToken(stmt->value.varDeclStmt.name);
            freeExpression(stmt->value.varDeclStmt.initializer);
            break;
            
        case IF_STMT:
            freeExpression(stmt->value.ifStmt.expression);
            freeStatement(stmt->value.ifStmt.thenBranch);
            break;
            
        case FOR_STMT:
            freeStatement(stmt->value.forStmt.initializer);
            freeExpression(stmt->value.forStmt.loopEnd);
            freeExpression(stmt->value.forStmt.increment);
            break;
    }
    
    free(stmt);
    stmt = NULL;
}

/* Deallocation of all statements in the statement table */
void freeStatements(void)
{
    for (int i = 0; i < stmtIndex; i++)
        freeStatement(program[i]);
    
    free(program);
    program = NULL;
}

/* Deallocation of a Token */
void freeToken(Token_t *token)
{
    if (DEBUG_QNICE)
        printf("freeToken\n");
    
    if (token == NULL)
        return;
    
    free(token->lexeme);
    token->lexeme = NULL;
    free(token);
    token = NULL;
}

/* Deallocation of a specific Token in the token table */
void freeSpecificToken(Token_t *token, int tokenIndex)
{
#ifndef __QNICE__
    freeToken(token);
    Tokens[tokenIndex] = NULL;
#endif
}

/* Deallocation of all tokens in the token table */
void freeTokens(void)
{
    if (DEBUG_QNICE)
        printf("freeTokens\n");
    
    for (int i = 0; i < scannerTokenIndex; i++){
        freeToken(Tokens[i]);
        Tokens[i] = NULL;
    }
    
    free(Tokens);
    Tokens = NULL;
}

/* Show an error message */
void reportParserError(char *exceptionMessage, Token_t *exceptionToken)
{
    if (exceptionToken != NULL && exceptionMessage != NULL){
        
        int len;
        
        if (exceptionToken->lexeme == NULL)
            len = (int) strlen(TOKEN_TYPE_STRING[exceptionToken->type]);
        else
            len = (int) strlen(exceptionToken->lexeme);
        
        char *str = malloc(sizeof(char) * (len + 7));
        
        if (str == NULL){
            printf("parserError: malloc failed!!!\n");
            return;
        }
        
        strncpy(str, " at '", 5);
        
        if (exceptionToken->lexeme == NULL)
            strncpy(str+5, TOKEN_TYPE_STRING[exceptionToken->type], len);
        else
            strncpy(str+5, exceptionToken->lexeme, len);
        
        strncpy(str+5+len, "'\0", 2);
        
        report(currentLineNumber, str, exceptionMessage);

        free(str);
    }
}

/* Show an error message and set hadRuntimeError to true */
void reportRuntimeError(int line, const char *where, const char *message)
{
    hadRuntimeError = true;
    report(line, where, message);
}

/* Show an error message and set hadError to true */
void report (int line, const char *where, const char *message)
{
    fprintf(stderr, "[line %d] Error %s : %s\n", line, where, message);
    hadError = true;
}

/* Show an error message */
void error(int line, const char *message)
{
    report(line, "", message);
}

/* Compare the line numbers of two statements.
 This is necessary for the ordering of statements. */
int compareLineNumberOfStatements( const void *a, const void *b)
{
    Statement_t *stmt_a = *(Statement_t**) a;
    Statement_t *stmt_b = *(Statement_t**) b;
    
    if (stmt_a->lineNumber == stmt_b->lineNumber)
        return 0;
    else if (stmt_a->lineNumber < stmt_b->lineNumber)
        return -1;
    else
        return 1;
}

//https://stackoverflow.com/questions/4384359/quick-way-to-implement-dictionary-in-c
/* hash: form hash value for string s */
unsigned hash(char *s)
{
    unsigned hashval;
    
    for (hashval = 0; *s != '\0'; s++)
        hashval = *s + 31 * hashval;
    
    return hashval % HASHSIZE;
}

/* Look for a variable in the symbol table */
Nlist_t *lookup(char *s)
{
    Nlist_t *np;
    
    for (np = hashtab[hash(s)]; np != NULL; np = np->next)
        if (strcmp(s, np->name) == 0)
            return np; /* found */
    
    return NULL; /* not found */
}

#ifdef __QNICE__
char *strdup(char *s) /* make a duplicate of s */
{
    if (DEBUG_QNICE)
        printf("strdup\n");
    
    char *p;
    p = (char *) malloc(strlen(s)+1); /* +1 for ’\0’ */
    
    if (p == NULL){
        printf("strdup: malloc failed!!\n");
        return NULL;
    }
    
    strcpy(p, s);
    
    p[strlen(s)] = '\0';
    return p;
}
#endif

/* Create a duplicate of an ExpressionResult */
ExpResult_t* expResultDub(ExpResult_t *result)
{
    if (DEBUG_QNICE)
        printf("expResultDub\n");
    
    ExpResult_t *r = (ExpResult_t*) malloc(sizeof(ExpResult_t));
    
    if (r == NULL){
        printf("expResultDub: malloc failed!!\n");
        return NULL;
    }
    
    r->tag = result->tag;
    
    switch (result->tag){
        case NUMBER_RESULT:
            if (DEBUG_QNICE)
                printf("expResult result->tag: NUMBER_RESULT\n");
            
            r->value.numberResult = result->value.numberResult;
            break;
            
        case STRING_RESULT:
            if (DEBUG_QNICE)
                printf("expResult result->tag: STRING_RESULT\n");
            
            r->value.stringResult = strdup(result->value.stringResult);
            
            if (r->value.stringResult == NULL)
                return NULL;
            break;
            
        default:
            if (DEBUG_QNICE)
                printf("expResult result->tag: default\n");
            return NULL;
    }
    if (DEBUG_QNICE)
        printf("expResult end\n");
    
    return r;
}

/* Add a variable to the symbol table */
/* install: put (name, expResult) in hashtab */
Nlist_t *install(char *name, ExpResult_t *expResult)
{
    Nlist_t *np;
    unsigned hashval;
    
    if ((np = lookup(name)) == NULL){ /* not found */
        np = (Nlist_t *) malloc(sizeof(*np));
        
        if (np == NULL || (np->name = strdup(name)) == NULL)
            return NULL;
        
        hashval = hash(name);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    } else /* already there */
        freeExpressionResult(np->expResult); /*free previous defn */
    
    if ((np->expResult = expResultDub(expResult)) == NULL)
        return NULL;
    return np;
}
