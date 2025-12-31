#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <cctype>

// We need to declare these types early since they reference each other
struct Value;
struct Object;
struct ClassInfo;
struct FunctionInfo;
struct MethodInfo;

class Tokenizer;
class Parser;
class Interpreter;

// All the different kinds of tokens our lexer can produce
enum class TokenType {
    INT_LITERAL,
    IDENTIFIER,
    
    // Language keywords
    CLASS,
    INT,
    VOID,
    IF,
    ELSE,
    WHILE,
    RETURN,
    PRINT,
    
    // Math and comparison stuff
    PLUS,
    MINUS,
    STAR,
    SLASH,
    ASSIGN,
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    AND,
    OR,
    NOT,
    
    // Punctuation
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    SEMICOLON,
    COMMA,
    DOT,
    
    END_OF_FILE,
    INVALID
};

// Holds info about a single token we've scanned
struct Token {
    TokenType type;
    std::string text;
    int line;
    int column;
    
    Token(TokenType t = TokenType::INVALID, const std::string& txt = "", int l = 0, int c = 0)
        : type(t), text(txt), line(l), column(c) {}
};

// Every kind of thing we can have in our syntax tree
enum class NodeType {
    PROGRAM,
    
    CLASS_DECL,
    FIELD_DECL,
    METHOD_DECL,
    FUNCTION_DECL,
    VAR_DECL,
    PARAMETER,
    
    BLOCK,
    IF_STMT,
    WHILE_STMT,
    RETURN_STMT,
    EXPR_STMT,
    PRINT_STMT,
    ASSIGN_STMT,
    FIELD_ASSIGN_STMT,
    
    INT_LITERAL,
    IDENTIFIER,
    BINARY_OP,
    UNARY_OP,
    CALL_EXPR,
    METHOD_CALL,
    FIELD_ACCESS,
    THIS_EXPR
};

// Base class for all AST nodes - everything inherits from this
struct ASTNode {
    NodeType type;
    virtual ~ASTNode() = default;
    ASTNode(NodeType t) : type(t) {}
};

using ASTPtr = std::shared_ptr<ASTNode>;

// Just a number like 42 or 0
struct IntLiteralNode : ASTNode {
    int value;
    IntLiteralNode(int v) : ASTNode(NodeType::INT_LITERAL), value(v) {}
};

// A variable name like "counter" or "x"
struct IdentifierNode : ASTNode {
    std::string name;
    IdentifierNode(const std::string& n) : ASTNode(NodeType::IDENTIFIER), name(n) {}
};

// The "this" keyword when you're inside a method
struct ThisExprNode : ASTNode {
    ThisExprNode() : ASTNode(NodeType::THIS_EXPR) {}
};

// Something like "a + b" or "x == y"
struct BinaryOpNode : ASTNode {
    std::string op;
    ASTPtr left;
    ASTPtr right;
    BinaryOpNode(const std::string& o, ASTPtr l, ASTPtr r) 
        : ASTNode(NodeType::BINARY_OP), op(o), left(l), right(r) {}
};

// Something like "-x" or "!done"
struct UnaryOpNode : ASTNode {
    std::string op;
    ASTPtr operand;
    UnaryOpNode(const std::string& o, ASTPtr opnd) 
        : ASTNode(NodeType::UNARY_OP), op(o), operand(opnd) {}
};

// Accessing a field like "point.x"
struct FieldAccessNode : ASTNode {
    ASTPtr object;
    std::string fieldName;
    FieldAccessNode(ASTPtr obj, const std::string& field) 
        : ASTNode(NodeType::FIELD_ACCESS), object(obj), fieldName(field) {}
};

// Calling a regular function like "factorial(5)"
struct CallExprNode : ASTNode {
    std::string funcName;
    std::vector<ASTPtr> arguments;
    CallExprNode(const std::string& name, std::vector<ASTPtr> args) 
        : ASTNode(NodeType::CALL_EXPR), funcName(name), arguments(std::move(args)) {}
};

// Calling a method on an object like "counter.increment()"
struct MethodCallNode : ASTNode {
    ASTPtr object;
    std::string methodName;
    std::vector<ASTPtr> arguments;
    MethodCallNode(ASTPtr obj, const std::string& method, std::vector<ASTPtr> args) 
        : ASTNode(NodeType::METHOD_CALL), object(obj), methodName(method), arguments(std::move(args)) {}
};

// A bunch of statements wrapped in curly braces
struct BlockNode : ASTNode {
    std::vector<ASTPtr> statements;
    BlockNode() : ASTNode(NodeType::BLOCK) {}
};

// Declaring a variable like "int x = 5;"
struct VarDeclNode : ASTNode {
    std::string typeName;
    std::string varName;
    ASTPtr initializer;
    VarDeclNode(const std::string& type, const std::string& name, ASTPtr init = nullptr) 
        : ASTNode(NodeType::VAR_DECL), typeName(type), varName(name), initializer(init) {}
};

// Assigning to a variable like "x = 10;"
struct AssignStmtNode : ASTNode {
    std::string varName;
    ASTPtr value;
    AssignStmtNode(const std::string& name, ASTPtr val) 
        : ASTNode(NodeType::ASSIGN_STMT), varName(name), value(val) {}
};

// Assigning to a field like "this.x = 10;"
struct FieldAssignStmtNode : ASTNode {
    ASTPtr object;
    std::string fieldName;
    ASTPtr value;
    FieldAssignStmtNode(ASTPtr obj, const std::string& field, ASTPtr val) 
        : ASTNode(NodeType::FIELD_ASSIGN_STMT), object(obj), fieldName(field), value(val) {}
};

// Your basic if statement with optional else
struct IfStmtNode : ASTNode {
    ASTPtr condition;
    ASTPtr thenBranch;
    ASTPtr elseBranch;
    IfStmtNode(ASTPtr cond, ASTPtr thenB, ASTPtr elseB = nullptr) 
        : ASTNode(NodeType::IF_STMT), condition(cond), thenBranch(thenB), elseBranch(elseB) {}
};

// While loop - keeps going until condition is false
struct WhileStmtNode : ASTNode {
    ASTPtr condition;
    ASTPtr body;
    WhileStmtNode(ASTPtr cond, ASTPtr b) 
        : ASTNode(NodeType::WHILE_STMT), condition(cond), body(b) {}
};

// Return statement - can have a value or not
struct ReturnStmtNode : ASTNode {
    ASTPtr value;
    ReturnStmtNode(ASTPtr val = nullptr) : ASTNode(NodeType::RETURN_STMT), value(val) {}
};

// Just an expression used as a statement
struct ExprStmtNode : ASTNode {
    ASTPtr expr;
    ExprStmtNode(ASTPtr e) : ASTNode(NodeType::EXPR_STMT), expr(e) {}
};

// Our print statement for debugging output
struct PrintStmtNode : ASTNode {
    ASTPtr expr;
    PrintStmtNode(ASTPtr e) : ASTNode(NodeType::PRINT_STMT), expr(e) {}
};

// A parameter in a function or method definition
struct ParameterNode : ASTNode {
    std::string typeName;
    std::string paramName;
    ParameterNode(const std::string& type, const std::string& name) 
        : ASTNode(NodeType::PARAMETER), typeName(type), paramName(name) {}
};

// A field inside a class
struct FieldDeclNode : ASTNode {
    std::string typeName;
    std::string fieldName;
    FieldDeclNode(const std::string& type, const std::string& name) 
        : ASTNode(NodeType::FIELD_DECL), typeName(type), fieldName(name) {}
};

// A method inside a class
struct MethodDeclNode : ASTNode {
    std::string returnType;
    std::string methodName;
    std::vector<std::shared_ptr<ParameterNode>> parameters;
    std::shared_ptr<BlockNode> body;
    MethodDeclNode(const std::string& ret, const std::string& name) 
        : ASTNode(NodeType::METHOD_DECL), returnType(ret), methodName(name) {}
};

// A whole class definition with fields and methods
struct ClassDeclNode : ASTNode {
    std::string className;
    std::vector<std::shared_ptr<FieldDeclNode>> fields;
    std::vector<std::shared_ptr<MethodDeclNode>> methods;
    ClassDeclNode(const std::string& name) : ASTNode(NodeType::CLASS_DECL), className(name) {}
};

// A standalone function (not inside a class)
struct FunctionDeclNode : ASTNode {
    std::string returnType;
    std::string funcName;
    std::vector<std::shared_ptr<ParameterNode>> parameters;
    std::shared_ptr<BlockNode> body;
    FunctionDeclNode(const std::string& ret, const std::string& name) 
        : ASTNode(NodeType::FUNCTION_DECL), returnType(ret), funcName(name) {}
};

// The root of our AST - holds all classes and functions
struct ProgramNode : ASTNode {
    std::vector<std::shared_ptr<ClassDeclNode>> classes;
    std::vector<std::shared_ptr<FunctionDeclNode>> functions;
    ProgramNode() : ASTNode(NodeType::PROGRAM) {}
};

// An object instance at runtime - just a class name and its field values
struct Object {
    std::string className;
    std::map<std::string, int> fields;
    
    Object(const std::string& clsName) : className(clsName) {}
};

using ObjectPtr = std::shared_ptr<Object>;

// Runtime values can be void, an int, or an object reference
struct Value {
    enum class Type { VOID, INT, OBJECT };
    Type type;
    int intValue;
    ObjectPtr objectValue;
    
    Value() : type(Type::VOID), intValue(0), objectValue(nullptr) {}
    Value(int v) : type(Type::INT), intValue(v), objectValue(nullptr) {}
    Value(ObjectPtr obj) : type(Type::OBJECT), intValue(0), objectValue(obj) {}
    
    bool isVoid() const { return type == Type::VOID; }
    bool isInt() const { return type == Type::INT; }
    bool isObject() const { return type == Type::OBJECT; }
    
    int asInt() const {
        if (type != Type::INT) throw std::runtime_error("Value is not an integer");
        return intValue;
    }
    
    ObjectPtr asObject() const {
        if (type != Type::OBJECT) throw std::runtime_error("Value is not an object");
        return objectValue;
    }
};

// Stores info about a field we've parsed
struct FieldInfo {
    std::string typeName;
    std::string fieldName;
};

// Stores info about a method we've parsed
struct MethodInfo {
    std::string returnType;
    std::string methodName;
    std::vector<std::pair<std::string, std::string>> parameters;
    std::shared_ptr<BlockNode> body;
};

// Everything we need to know about a class
struct ClassInfo {
    std::string className;
    std::vector<FieldInfo> fields;
    std::map<std::string, MethodInfo> methods;
};

// Everything we need to know about a function
struct FunctionInfo {
    std::string returnType;
    std::string funcName;
    std::vector<std::pair<std::string, std::string>> parameters;
    std::shared_ptr<BlockNode> body;
};

// The tokenizer breaks source code into tokens
// It handles whitespace, comments, numbers, identifiers, and operators
class Tokenizer {
private:
    std::string source;
    size_t pos;
    int line;
    int column;
    
    char current() const {
        return pos < source.size() ? source[pos] : '\0';
    }
    
    char peek(int offset = 1) const {
        size_t idx = pos + offset;
        return idx < source.size() ? source[idx] : '\0';
    }
    
    void advance() {
        if (current() == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        pos++;
    }
    
    void skipWhitespace() {
        while (std::isspace(current())) {
            advance();
        }
    }
    
    void skipLineComment() {
        while (current() != '\n' && current() != '\0') {
            advance();
        }
    }
    
    void skipBlockComment() {
        advance();
        advance();
        while (!(current() == '*' && peek() == '/') && current() != '\0') {
            advance();
        }
        if (current() != '\0') {
            advance();
            advance();
        }
    }
    
    Token readNumber() {
        int startCol = column;
        std::string num;
        while (std::isdigit(current())) {
            num += current();
            advance();
        }
        return Token(TokenType::INT_LITERAL, num, line, startCol);
    }
    
    Token readIdentifierOrKeyword() {
        int startCol = column;
        std::string id;
        while (std::isalnum(current()) || current() == '_') {
            id += current();
            advance();
        }
        
        // Check if it's a keyword instead of a regular identifier
        if (id == "class") return Token(TokenType::CLASS, id, line, startCol);
        if (id == "int") return Token(TokenType::INT, id, line, startCol);
        if (id == "void") return Token(TokenType::VOID, id, line, startCol);
        if (id == "if") return Token(TokenType::IF, id, line, startCol);
        if (id == "else") return Token(TokenType::ELSE, id, line, startCol);
        if (id == "while") return Token(TokenType::WHILE, id, line, startCol);
        if (id == "return") return Token(TokenType::RETURN, id, line, startCol);
        if (id == "print") return Token(TokenType::PRINT, id, line, startCol);
        
        return Token(TokenType::IDENTIFIER, id, line, startCol);
    }

public:
    Tokenizer(const std::string& src) : source(src), pos(0), line(1), column(1) {}
    
    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        
        while (current() != '\0') {
            skipWhitespace();
            if (current() == '\0') break;
            
            int startLine = line;
            int startCol = column;
            
            // Handle comments - just skip over them
            if (current() == '/' && peek() == '/') {
                skipLineComment();
                continue;
            }
            if (current() == '/' && peek() == '*') {
                skipBlockComment();
                continue;
            }
            
            if (std::isdigit(current())) {
                tokens.push_back(readNumber());
                continue;
            }
            
            if (std::isalpha(current()) || current() == '_') {
                tokens.push_back(readIdentifierOrKeyword());
                continue;
            }
            
            // Two character operators need to be checked first
            if (current() == '=' && peek() == '=') {
                advance(); advance();
                tokens.push_back(Token(TokenType::EQ, "==", startLine, startCol));
                continue;
            }
            if (current() == '!' && peek() == '=') {
                advance(); advance();
                tokens.push_back(Token(TokenType::NE, "!=", startLine, startCol));
                continue;
            }
            if (current() == '<' && peek() == '=') {
                advance(); advance();
                tokens.push_back(Token(TokenType::LE, "<=", startLine, startCol));
                continue;
            }
            if (current() == '>' && peek() == '=') {
                advance(); advance();
                tokens.push_back(Token(TokenType::GE, ">=", startLine, startCol));
                continue;
            }
            if (current() == '&' && peek() == '&') {
                advance(); advance();
                tokens.push_back(Token(TokenType::AND, "&&", startLine, startCol));
                continue;
            }
            if (current() == '|' && peek() == '|') {
                advance(); advance();
                tokens.push_back(Token(TokenType::OR, "||", startLine, startCol));
                continue;
            }
            
            // Single character tokens
            char c = current();
            advance();
            switch (c) {
                case '+': tokens.push_back(Token(TokenType::PLUS, "+", startLine, startCol)); break;
                case '-': tokens.push_back(Token(TokenType::MINUS, "-", startLine, startCol)); break;
                case '*': tokens.push_back(Token(TokenType::STAR, "*", startLine, startCol)); break;
                case '/': tokens.push_back(Token(TokenType::SLASH, "/", startLine, startCol)); break;
                case '=': tokens.push_back(Token(TokenType::ASSIGN, "=", startLine, startCol)); break;
                case '<': tokens.push_back(Token(TokenType::LT, "<", startLine, startCol)); break;
                case '>': tokens.push_back(Token(TokenType::GT, ">", startLine, startCol)); break;
                case '!': tokens.push_back(Token(TokenType::NOT, "!", startLine, startCol)); break;
                case '(': tokens.push_back(Token(TokenType::LPAREN, "(", startLine, startCol)); break;
                case ')': tokens.push_back(Token(TokenType::RPAREN, ")", startLine, startCol)); break;
                case '{': tokens.push_back(Token(TokenType::LBRACE, "{", startLine, startCol)); break;
                case '}': tokens.push_back(Token(TokenType::RBRACE, "}", startLine, startCol)); break;
                case ';': tokens.push_back(Token(TokenType::SEMICOLON, ";", startLine, startCol)); break;
                case ',': tokens.push_back(Token(TokenType::COMMA, ",", startLine, startCol)); break;
                case '.': tokens.push_back(Token(TokenType::DOT, ".", startLine, startCol)); break;
                default:
                    throw std::runtime_error("Unexpected character: " + std::string(1, c));
            }
        }
        
        tokens.push_back(Token(TokenType::END_OF_FILE, "", line, column));
        return tokens;
    }
};

// The parser turns a stream of tokens into an AST
// It uses recursive descent with precedence climbing for expressions
class Parser {
private:
    std::vector<Token> tokens;
    size_t pos;
    
    Token current() const {
        return pos < tokens.size() ? tokens[pos] : Token(TokenType::END_OF_FILE);
    }
    
    Token peek(int offset = 1) const {
        size_t idx = pos + offset;
        return idx < tokens.size() ? tokens[idx] : Token(TokenType::END_OF_FILE);
    }
    
    Token advance() {
        Token t = current();
        if (pos < tokens.size()) pos++;
        return t;
    }
    
    bool check(TokenType type) const {
        return current().type == type;
    }
    
    bool match(TokenType type) {
        if (check(type)) {
            advance();
            return true;
        }
        return false;
    }
    
    Token expect(TokenType type, const std::string& msg) {
        if (!check(type)) {
            throw std::runtime_error("Parse error at line " + std::to_string(current().line) + 
                                    ": " + msg + ", got '" + current().text + "'");
        }
        return advance();
    }
    
    bool isTypeName() const {
        return check(TokenType::INT) || check(TokenType::VOID) || check(TokenType::IDENTIFIER);
    }
    
    std::string parseTypeName() {
        if (check(TokenType::INT)) {
            advance();
            return "int";
        }
        if (check(TokenType::VOID)) {
            advance();
            return "void";
        }
        if (check(TokenType::IDENTIFIER)) {
            return advance().text;
        }
        throw std::runtime_error("Expected type name");
    }
    
    // Expression parsing - each function handles one precedence level
    // Lower precedence operators are higher in the call chain
    ASTPtr parseExpression() {
        return parseOr();
    }
    
    ASTPtr parseOr() {
        ASTPtr left = parseAnd();
        while (match(TokenType::OR)) {
            ASTPtr right = parseAnd();
            left = std::make_shared<BinaryOpNode>("||", left, right);
        }
        return left;
    }
    
    ASTPtr parseAnd() {
        ASTPtr left = parseEquality();
        while (match(TokenType::AND)) {
            ASTPtr right = parseEquality();
            left = std::make_shared<BinaryOpNode>("&&", left, right);
        }
        return left;
    }
    
    ASTPtr parseEquality() {
        ASTPtr left = parseComparison();
        while (check(TokenType::EQ) || check(TokenType::NE)) {
            std::string op = advance().text;
            ASTPtr right = parseComparison();
            left = std::make_shared<BinaryOpNode>(op, left, right);
        }
        return left;
    }
    
    ASTPtr parseComparison() {
        ASTPtr left = parseAdditive();
        while (check(TokenType::LT) || check(TokenType::GT) || 
               check(TokenType::LE) || check(TokenType::GE)) {
            std::string op = advance().text;
            ASTPtr right = parseAdditive();
            left = std::make_shared<BinaryOpNode>(op, left, right);
        }
        return left;
    }
    
    ASTPtr parseAdditive() {
        ASTPtr left = parseMultiplicative();
        while (check(TokenType::PLUS) || check(TokenType::MINUS)) {
            std::string op = advance().text;
            ASTPtr right = parseMultiplicative();
            left = std::make_shared<BinaryOpNode>(op, left, right);
        }
        return left;
    }
    
    ASTPtr parseMultiplicative() {
        ASTPtr left = parseUnary();
        while (check(TokenType::STAR) || check(TokenType::SLASH)) {
            std::string op = advance().text;
            ASTPtr right = parseUnary();
            left = std::make_shared<BinaryOpNode>(op, left, right);
        }
        return left;
    }
    
    ASTPtr parseUnary() {
        if (check(TokenType::MINUS) || check(TokenType::NOT)) {
            std::string op = advance().text;
            ASTPtr operand = parseUnary();
            return std::make_shared<UnaryOpNode>(op, operand);
        }
        return parsePostfix();
    }
    
    // Handles dots for field access and method calls
    ASTPtr parsePostfix() {
        ASTPtr expr = parsePrimary();
        
        while (true) {
            if (match(TokenType::DOT)) {
                std::string memberName = expect(TokenType::IDENTIFIER, "Expected member name").text;
                
                // Is it a method call or just field access?
                if (match(TokenType::LPAREN)) {
                    std::vector<ASTPtr> args;
                    if (!check(TokenType::RPAREN)) {
                        do {
                            args.push_back(parseExpression());
                        } while (match(TokenType::COMMA));
                    }
                    expect(TokenType::RPAREN, "Expected ')'");
                    expr = std::make_shared<MethodCallNode>(expr, memberName, std::move(args));
                } else {
                    expr = std::make_shared<FieldAccessNode>(expr, memberName);
                }
            } else {
                break;
            }
        }
        
        return expr;
    }
    
    // The base case for expressions - literals, variables, function calls
    ASTPtr parsePrimary() {
        if (check(TokenType::INT_LITERAL)) {
            int value = std::stoi(advance().text);
            return std::make_shared<IntLiteralNode>(value);
        }
        
        if (match(TokenType::LPAREN)) {
            ASTPtr expr = parseExpression();
            expect(TokenType::RPAREN, "Expected ')'");
            return expr;
        }
        
        if (check(TokenType::IDENTIFIER)) {
            std::string name = advance().text;
            
            if (name == "this") {
                return std::make_shared<ThisExprNode>();
            }
            
            // Function call if followed by parenthesis
            if (match(TokenType::LPAREN)) {
                std::vector<ASTPtr> args;
                if (!check(TokenType::RPAREN)) {
                    do {
                        args.push_back(parseExpression());
                    } while (match(TokenType::COMMA));
                }
                expect(TokenType::RPAREN, "Expected ')'");
                return std::make_shared<CallExprNode>(name, std::move(args));
            }
            
            return std::make_shared<IdentifierNode>(name);
        }
        
        throw std::runtime_error("Unexpected token in expression: " + current().text);
    }
    
    // Parse different kinds of statements
    ASTPtr parseStatement() {
        if (check(TokenType::LBRACE)) {
            return parseBlock();
        }
        
        if (match(TokenType::IF)) {
            expect(TokenType::LPAREN, "Expected '(' after 'if'");
            ASTPtr condition = parseExpression();
            expect(TokenType::RPAREN, "Expected ')' after condition");
            ASTPtr thenBranch = parseStatement();
            ASTPtr elseBranch = nullptr;
            if (match(TokenType::ELSE)) {
                elseBranch = parseStatement();
            }
            return std::make_shared<IfStmtNode>(condition, thenBranch, elseBranch);
        }
        
        if (match(TokenType::WHILE)) {
            expect(TokenType::LPAREN, "Expected '(' after 'while'");
            ASTPtr condition = parseExpression();
            expect(TokenType::RPAREN, "Expected ')' after condition");
            ASTPtr body = parseStatement();
            return std::make_shared<WhileStmtNode>(condition, body);
        }
        
        if (match(TokenType::RETURN)) {
            ASTPtr value = nullptr;
            if (!check(TokenType::SEMICOLON)) {
                value = parseExpression();
            }
            expect(TokenType::SEMICOLON, "Expected ';' after return");
            return std::make_shared<ReturnStmtNode>(value);
        }
        
        if (match(TokenType::PRINT)) {
            expect(TokenType::LPAREN, "Expected '(' after 'print'");
            ASTPtr expr = parseExpression();
            expect(TokenType::RPAREN, "Expected ')' after expression");
            expect(TokenType::SEMICOLON, "Expected ';' after print statement");
            return std::make_shared<PrintStmtNode>(expr);
        }
        
        // Variable declaration looks like: type name = value;
        // Need to look ahead to distinguish from expression statements
        if (isTypeName() && peek().type == TokenType::IDENTIFIER && 
            (peek(2).type == TokenType::SEMICOLON || peek(2).type == TokenType::ASSIGN)) {
            std::string typeName = parseTypeName();
            std::string varName = expect(TokenType::IDENTIFIER, "Expected variable name").text;
            ASTPtr init = nullptr;
            if (match(TokenType::ASSIGN)) {
                init = parseExpression();
            }
            expect(TokenType::SEMICOLON, "Expected ';' after variable declaration");
            return std::make_shared<VarDeclNode>(typeName, varName, init);
        }
        
        // If it's not any of the above, it's probably an expression or assignment
        ASTPtr expr = parseExpression();
        
        if (match(TokenType::ASSIGN)) {
            ASTPtr value = parseExpression();
            expect(TokenType::SEMICOLON, "Expected ';' after assignment");
            
            if (auto id = std::dynamic_pointer_cast<IdentifierNode>(expr)) {
                return std::make_shared<AssignStmtNode>(id->name, value);
            }
            if (auto field = std::dynamic_pointer_cast<FieldAccessNode>(expr)) {
                return std::make_shared<FieldAssignStmtNode>(field->object, field->fieldName, value);
            }
            throw std::runtime_error("Invalid assignment target");
        }
        
        expect(TokenType::SEMICOLON, "Expected ';' after expression");
        return std::make_shared<ExprStmtNode>(expr);
    }
    
    std::shared_ptr<BlockNode> parseBlock() {
        expect(TokenType::LBRACE, "Expected '{'");
        auto block = std::make_shared<BlockNode>();
        while (!check(TokenType::RBRACE) && !check(TokenType::END_OF_FILE)) {
            block->statements.push_back(parseStatement());
        }
        expect(TokenType::RBRACE, "Expected '}'");
        return block;
    }
    
    std::vector<std::shared_ptr<ParameterNode>> parseParameters() {
        std::vector<std::shared_ptr<ParameterNode>> params;
        expect(TokenType::LPAREN, "Expected '('");
        if (!check(TokenType::RPAREN)) {
            do {
                std::string typeName = parseTypeName();
                std::string paramName = expect(TokenType::IDENTIFIER, "Expected parameter name").text;
                params.push_back(std::make_shared<ParameterNode>(typeName, paramName));
            } while (match(TokenType::COMMA));
        }
        expect(TokenType::RPAREN, "Expected ')'");
        return params;
    }
    
    std::shared_ptr<ClassDeclNode> parseClassDecl() {
        expect(TokenType::CLASS, "Expected 'class'");
        std::string className = expect(TokenType::IDENTIFIER, "Expected class name").text;
        expect(TokenType::LBRACE, "Expected '{'");
        
        auto classDecl = std::make_shared<ClassDeclNode>(className);
        
        while (!check(TokenType::RBRACE) && !check(TokenType::END_OF_FILE)) {
            std::string typeName = parseTypeName();
            std::string memberName = expect(TokenType::IDENTIFIER, "Expected member name").text;
            
            // If there's a paren, it's a method; otherwise it's a field
            if (check(TokenType::LPAREN)) {
                auto method = std::make_shared<MethodDeclNode>(typeName, memberName);
                auto params = parseParameters();
                for (auto& p : params) {
                    method->parameters.push_back(p);
                }
                method->body = parseBlock();
                classDecl->methods.push_back(method);
            }
            else {
                expect(TokenType::SEMICOLON, "Expected ';' after field declaration");
                classDecl->fields.push_back(std::make_shared<FieldDeclNode>(typeName, memberName));
            }
        }
        
        expect(TokenType::RBRACE, "Expected '}'");
        return classDecl;
    }
    
    std::shared_ptr<FunctionDeclNode> parseFunctionDecl() {
        std::string returnType = parseTypeName();
        std::string funcName = expect(TokenType::IDENTIFIER, "Expected function name").text;
        
        auto func = std::make_shared<FunctionDeclNode>(returnType, funcName);
        auto params = parseParameters();
        for (auto& p : params) {
            func->parameters.push_back(p);
        }
        func->body = parseBlock();
        
        return func;
    }

public:
    Parser(const std::vector<Token>& toks) : tokens(toks), pos(0) {}
    
    std::shared_ptr<ProgramNode> parse() {
        auto program = std::make_shared<ProgramNode>();
        
        while (!check(TokenType::END_OF_FILE)) {
            if (check(TokenType::CLASS)) {
                program->classes.push_back(parseClassDecl());
            } else {
                program->functions.push_back(parseFunctionDecl());
            }
        }
        
        return program;
    }
};

// We use exceptions to handle return statements
// This makes it easy to unwind the call stack
class ReturnException : public std::exception {
public:
    Value value;
    ReturnException(const Value& v) : value(v) {}
};

// Holds variable bindings for the current scope
// Each scope has a pointer to its parent so we can look up the chain
class Environment {
private:
    std::map<std::string, Value> variables;
    std::shared_ptr<Environment> parent;

public:
    Environment(std::shared_ptr<Environment> p = nullptr) : parent(p) {}
    
    void define(const std::string& name, const Value& value) {
        variables[name] = value;
    }
    
    Value get(const std::string& name) const {
        auto it = variables.find(name);
        if (it != variables.end()) {
            return it->second;
        }
        if (parent) {
            return parent->get(name);
        }
        throw std::runtime_error("Undefined variable: " + name);
    }
    
    void set(const std::string& name, const Value& value) {
        auto it = variables.find(name);
        if (it != variables.end()) {
            it->second = value;
            return;
        }
        if (parent) {
            parent->set(name, value);
            return;
        }
        throw std::runtime_error("Undefined variable: " + name);
    }
    
    bool has(const std::string& name) const {
        if (variables.count(name)) return true;
        if (parent) return parent->has(name);
        return false;
    }
};

// The interpreter walks the AST and executes it
class Interpreter {
private:
    std::map<std::string, ClassInfo> classTable;
    std::map<std::string, FunctionInfo> functionTable;
    std::shared_ptr<Environment> globalEnv;
    std::shared_ptr<Environment> currentEnv;
    
    // First pass: collect all class definitions
    void registerClasses(const std::shared_ptr<ProgramNode>& program) {
        for (auto& classDecl : program->classes) {
            ClassInfo info;
            info.className = classDecl->className;
            
            for (auto& field : classDecl->fields) {
                FieldInfo fi;
                fi.typeName = field->typeName;
                fi.fieldName = field->fieldName;
                info.fields.push_back(fi);
            }
            
            for (auto& method : classDecl->methods) {
                MethodInfo mi;
                mi.returnType = method->returnType;
                mi.methodName = method->methodName;
                mi.body = method->body;
                for (auto& param : method->parameters) {
                    mi.parameters.push_back({param->typeName, param->paramName});
                }
                info.methods[method->methodName] = mi;
            }
            
            classTable[classDecl->className] = info;
        }
    }
    
    // First pass: collect all function definitions
    void registerFunctions(const std::shared_ptr<ProgramNode>& program) {
        for (auto& func : program->functions) {
            FunctionInfo info;
            info.returnType = func->returnType;
            info.funcName = func->funcName;
            info.body = func->body;
            for (auto& param : func->parameters) {
                info.parameters.push_back({param->typeName, param->paramName});
            }
            functionTable[func->funcName] = info;
        }
    }
    
    // Create a new object instance with all fields initialized to zero
    ObjectPtr createObject(const std::string& className) {
        if (classTable.find(className) == classTable.end()) {
            throw std::runtime_error("Unknown class: " + className);
        }
        
        auto& classInfo = classTable[className];
        auto obj = std::make_shared<Object>(className);
        
        for (auto& field : classInfo.fields) {
            obj->fields[field.fieldName] = 0;
        }
        
        return obj;
    }
    
    // Evaluate an expression and return its value
    Value evaluate(const ASTPtr& node) {
        if (!node) return Value();
        
        switch (node->type) {
            case NodeType::INT_LITERAL: {
                auto n = std::static_pointer_cast<IntLiteralNode>(node);
                return Value(n->value);
            }
            
            case NodeType::IDENTIFIER: {
                auto n = std::static_pointer_cast<IdentifierNode>(node);
                return currentEnv->get(n->name);
            }
            
            case NodeType::THIS_EXPR: {
                return currentEnv->get("this");
            }
            
            case NodeType::BINARY_OP: {
                auto n = std::static_pointer_cast<BinaryOpNode>(node);
                
                // Short circuit for logical operators - don't evaluate right side if we don't need to
                if (n->op == "&&") {
                    Value left = evaluate(n->left);
                    if (left.asInt() == 0) return Value(0);
                    return Value(evaluate(n->right).asInt() != 0 ? 1 : 0);
                }
                if (n->op == "||") {
                    Value left = evaluate(n->left);
                    if (left.asInt() != 0) return Value(1);
                    return Value(evaluate(n->right).asInt() != 0 ? 1 : 0);
                }
                
                Value left = evaluate(n->left);
                Value right = evaluate(n->right);
                
                int l = left.asInt();
                int r = right.asInt();
                
                if (n->op == "+") return Value(l + r);
                if (n->op == "-") return Value(l - r);
                if (n->op == "*") return Value(l * r);
                if (n->op == "/") {
                    if (r == 0) throw std::runtime_error("Division by zero");
                    return Value(l / r);
                }
                if (n->op == "==") return Value(l == r ? 1 : 0);
                if (n->op == "!=") return Value(l != r ? 1 : 0);
                if (n->op == "<") return Value(l < r ? 1 : 0);
                if (n->op == ">") return Value(l > r ? 1 : 0);
                if (n->op == "<=") return Value(l <= r ? 1 : 0);
                if (n->op == ">=") return Value(l >= r ? 1 : 0);
                
                throw std::runtime_error("Unknown operator: " + n->op);
            }
            
            case NodeType::UNARY_OP: {
                auto n = std::static_pointer_cast<UnaryOpNode>(node);
                Value operand = evaluate(n->operand);
                
                if (n->op == "-") return Value(-operand.asInt());
                if (n->op == "!") return Value(operand.asInt() == 0 ? 1 : 0);
                
                throw std::runtime_error("Unknown unary operator: " + n->op);
            }
            
            case NodeType::FIELD_ACCESS: {
                auto n = std::static_pointer_cast<FieldAccessNode>(node);
                Value objVal = evaluate(n->object);
                ObjectPtr obj = objVal.asObject();
                
                auto it = obj->fields.find(n->fieldName);
                if (it == obj->fields.end()) {
                    throw std::runtime_error("Unknown field: " + n->fieldName);
                }
                return Value(it->second);
            }
            
            case NodeType::CALL_EXPR: {
                auto n = std::static_pointer_cast<CallExprNode>(node);
                return callFunction(n->funcName, n->arguments);
            }
            
            case NodeType::METHOD_CALL: {
                auto n = std::static_pointer_cast<MethodCallNode>(node);
                Value objVal = evaluate(n->object);
                ObjectPtr obj = objVal.asObject();
                return callMethod(obj, n->methodName, n->arguments);
            }
            
            default:
                throw std::runtime_error("Cannot evaluate node type");
        }
    }
    
    // Execute a statement (doesn't return a value, just does stuff)
    void execute(const ASTPtr& node) {
        if (!node) return;
        
        switch (node->type) {
            case NodeType::BLOCK: {
                auto n = std::static_pointer_cast<BlockNode>(node);
                auto prevEnv = currentEnv;
                currentEnv = std::make_shared<Environment>(prevEnv);
                for (auto& stmt : n->statements) {
                    execute(stmt);
                }
                currentEnv = prevEnv;
                break;
            }
            
            case NodeType::VAR_DECL: {
                auto n = std::static_pointer_cast<VarDeclNode>(node);
                Value value;
                
                if (n->initializer) {
                    value = evaluate(n->initializer);
                } else if (n->typeName == "int") {
                    value = Value(0);
                } else {
                    // It's a class type, so make a new object
                    value = Value(createObject(n->typeName));
                }
                
                currentEnv->define(n->varName, value);
                break;
            }
            
            case NodeType::ASSIGN_STMT: {
                auto n = std::static_pointer_cast<AssignStmtNode>(node);
                Value value = evaluate(n->value);
                currentEnv->set(n->varName, value);
                break;
            }
            
            case NodeType::FIELD_ASSIGN_STMT: {
                auto n = std::static_pointer_cast<FieldAssignStmtNode>(node);
                Value objVal = evaluate(n->object);
                ObjectPtr obj = objVal.asObject();
                Value value = evaluate(n->value);
                
                if (obj->fields.find(n->fieldName) == obj->fields.end()) {
                    throw std::runtime_error("Unknown field: " + n->fieldName);
                }
                obj->fields[n->fieldName] = value.asInt();
                break;
            }
            
            case NodeType::IF_STMT: {
                auto n = std::static_pointer_cast<IfStmtNode>(node);
                Value cond = evaluate(n->condition);
                if (cond.asInt() != 0) {
                    execute(n->thenBranch);
                } else if (n->elseBranch) {
                    execute(n->elseBranch);
                }
                break;
            }
            
            case NodeType::WHILE_STMT: {
                auto n = std::static_pointer_cast<WhileStmtNode>(node);
                while (evaluate(n->condition).asInt() != 0) {
                    execute(n->body);
                }
                break;
            }
            
            case NodeType::RETURN_STMT: {
                auto n = std::static_pointer_cast<ReturnStmtNode>(node);
                Value value;
                if (n->value) {
                    value = evaluate(n->value);
                }
                throw ReturnException(value);
            }
            
            case NodeType::EXPR_STMT: {
                auto n = std::static_pointer_cast<ExprStmtNode>(node);
                evaluate(n->expr);
                break;
            }
            
            case NodeType::PRINT_STMT: {
                auto n = std::static_pointer_cast<PrintStmtNode>(node);
                Value value = evaluate(n->expr);
                if (value.isInt()) {
                    std::cout << value.asInt() << std::endl;
                } else if (value.isObject()) {
                    std::cout << "<object:" << value.asObject()->className << ">" << std::endl;
                } else {
                    std::cout << "<void>" << std::endl;
                }
                break;
            }
            
            default:
                throw std::runtime_error("Cannot execute node type");
        }
    }
    
    // Call a regular function with the given arguments
    Value callFunction(const std::string& name, const std::vector<ASTPtr>& args) {
        if (functionTable.find(name) == functionTable.end()) {
            throw std::runtime_error("Unknown function: " + name);
        }
        
        auto& func = functionTable[name];
        
        if (args.size() != func.parameters.size()) {
            throw std::runtime_error("Wrong number of arguments for function: " + name);
        }
        
        std::vector<Value> argValues;
        for (auto& arg : args) {
            argValues.push_back(evaluate(arg));
        }
        
        auto prevEnv = currentEnv;
        currentEnv = std::make_shared<Environment>(globalEnv);
        
        for (size_t i = 0; i < func.parameters.size(); i++) {
            currentEnv->define(func.parameters[i].second, argValues[i]);
        }
        
        Value result;
        try {
            execute(func.body);
        } catch (ReturnException& e) {
            result = e.value;
        }
        
        currentEnv = prevEnv;
        return result;
    }
    
    // Call a method on an object - similar to callFunction but with "this" bound
    Value callMethod(ObjectPtr obj, const std::string& methodName, const std::vector<ASTPtr>& args) {
        if (classTable.find(obj->className) == classTable.end()) {
            throw std::runtime_error("Unknown class: " + obj->className);
        }
        
        auto& classInfo = classTable[obj->className];
        
        if (classInfo.methods.find(methodName) == classInfo.methods.end()) {
            throw std::runtime_error("Unknown method: " + methodName);
        }
        
        auto& method = classInfo.methods[methodName];
        
        if (args.size() != method.parameters.size()) {
            throw std::runtime_error("Wrong number of arguments for method: " + methodName);
        }
        
        std::vector<Value> argValues;
        for (auto& arg : args) {
            argValues.push_back(evaluate(arg));
        }
        
        auto prevEnv = currentEnv;
        currentEnv = std::make_shared<Environment>(globalEnv);
        
        // This is the key difference from functions - we bind "this"
        currentEnv->define("this", Value(obj));
        
        for (size_t i = 0; i < method.parameters.size(); i++) {
            currentEnv->define(method.parameters[i].second, argValues[i]);
        }
        
        Value result;
        try {
            execute(method.body);
        } catch (ReturnException& e) {
            result = e.value;
        }
        
        currentEnv = prevEnv;
        return result;
    }

public:
    Interpreter() {
        globalEnv = std::make_shared<Environment>();
        currentEnv = globalEnv;
    }
    
    void run(const std::shared_ptr<ProgramNode>& program) {
        registerClasses(program);
        registerFunctions(program);
        
        if (functionTable.find("main") == functionTable.end()) {
            throw std::runtime_error("No main function found");
        }
        
        callFunction("main", {});
    }
};

int main() {
    // A simple test program that exercises the language features
    std::string source = R"(
        class Counter {
            int value;
            
            void reset() {
                this.value = 0;
            }
            
            void increment() {
                this.value = this.value + 1;
            }
            
            void add(int amount) {
                this.value = this.value + amount;
            }
            
            int getValue() {
                return this.value;
            }
        }
        
        class Point {
            int x;
            int y;
            
            void setCoords(int newX, int newY) {
                this.x = newX;
                this.y = newY;
            }
            
            int distanceSquared() {
                return this.x * this.x + this.y * this.y;
            }
        }
        
        int factorial(int n) {
            if (n <= 1) {
                return 1;
            }
            return n * factorial(n - 1);
        }
        
        int square(int x) {
            return x * x;
        }
        
        int main() {
            print(100);
            
            Counter c;
            c.reset();
            c.increment();
            c.increment();
            c.increment();
            print(c.getValue());
            
            c.add(7);
            print(c.getValue());
            
            Point p;
            p.setCoords(3, 4);
            print(p.x);
            print(p.y);
            print(p.distanceSquared());
            
            p.x = 5;
            p.y = 12;
            print(p.distanceSquared());
            
            print(factorial(5));
            print(square(7));
            
            int sum;
            sum = 0;
            int i;
            i = 1;
            while (i <= 5) {
                sum = sum + i;
                i = i + 1;
            }
            print(sum);
            
            int x;
            x = 42;
            if (x > 40) {
                print(1);
            } else {
                print(0);
            }
            
            print(3 < 5);
            print(5 < 3);
            print(5 == 5);
            print(5 != 5);
            
            print(10 + 5);
            print(10 - 3);
            print(6 * 7);
            print(100 / 4);
            
            print(-5 + 10);
            print(!0);
            print(!1);
            
            print(1 && 1);
            print(1 && 0);
            print(0 || 1);
            print(0 || 0);
            
            Counter c1;
            Counter c2;
            c1.reset();
            c2.reset();
            c1.add(100);
            c2.add(200);
            print(c1.getValue());
            print(c2.getValue());
            
            print(999);
            
            return 0;
        }
    )";
    
    try {
        Tokenizer tokenizer(source);
        std::vector<Token> tokens = tokenizer.tokenize();
        
        Parser parser(tokens);
        std::shared_ptr<ProgramNode> program = parser.parse();
        
        Interpreter interpreter;
        interpreter.run(program);
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}