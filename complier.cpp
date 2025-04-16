#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <stdexcept>
#include <functional>
#include <fstream>
#include <sstream>
#include <cctype>
#include <algorithm>
#include <map>

struct ASTNode
{
    std::string type;
    std::string value;
    std::vector<ASTNode *> children;
    ASTNode(std::string t, std::string v = "") : type(t), value(v) {}
};

struct Instruction
{
    std::string op;
    std::string arg1;
    std::string arg2;
    std::string result;
    Instruction(std::string o, std::string a1, std::string a2, std::string r) : op(o), arg1(a1), arg2(a2), result(r) {}
};

enum TokenType
{
    KEYWORD,
    IDENTIFIER,
    NUMBER,
    OPERATOR,
    PUNCTUATION,
    STRING_LITERAL,
    EOF_TOKEN
};

struct Token
{
    TokenType type;
    std::string value;
};

std::vector<Token> tokenize(const std::string &input)
{
    std::vector<Token> tokens;
    size_t i = 0;
    while (i < input.length())
    {
        char c = input[i];
        if (std::isspace(c) || c == '\n')
        {
            i++;
            continue;
        }
        if (c == '"')
        {
            size_t start = i;
            i++;
            while (i < input.length() && input[i] != '"')
                i++;
            if (i == input.length())
                throw std::runtime_error("Unterminated string literal");
            std::string str = input.substr(start + 1, i - start - 1);
            tokens.push_back({STRING_LITERAL, str});
            i++;
        }
        else if (std::isalpha(c))
        {
            std::string word;
            while (i < input.length() && std::isalnum(input[i]))
                word += input[i++];
            if (word == "int" || word == "print" || word == "if" || word == "else" || word == "while")
                tokens.push_back({KEYWORD, word});
            else
                tokens.push_back({IDENTIFIER, word});
        }
        else if (std::isdigit(c))
        {
            std::string num;
            while (i < input.length() && std::isdigit(input[i]))
                num += input[i++];
            tokens.push_back({NUMBER, num});
        }
        else if (c == '+' || c == '-' || c == '*' || c == '/' || c == '=')
        {
            if (i + 1 < input.length() && input[i + 1] == '=')
            {
                tokens.push_back({OPERATOR, std::string(1, c) + "="});
                i += 2;
            }
            else
            {
                tokens.push_back({OPERATOR, std::string(1, c)});
                i++;
            }
        }
        else if (c == '<' || c == '>')
        {
            if (i + 1 < input.length() && input[i + 1] == '=')
            {
                tokens.push_back({OPERATOR, std::string(1, c) + "="});
                i += 2;
            }
            else
            {
                tokens.push_back({OPERATOR, std::string(1, c)});
                i++;
            }
        }
        else if (c == ';' || c == '(' || c == ')' || c == '{' || c == '}' || c == '[' || c == ']')
        {
            tokens.push_back({PUNCTUATION, std::string(1, c)});
            i++;
        }
        else
        {
            throw std::runtime_error("Invalid character: " + std::string(1, c));
        }
    }
    tokens.push_back({EOF_TOKEN, ""});
    return tokens;
}

struct Production
{
    std::string lhs;
    std::vector<std::string> rhs;
    Production(std::string l, std::vector<std::string> r) : lhs(l), rhs(r) {}
};

struct State
{
    Production prod;
    size_t dot;
    size_t start;
    State(Production p, size_t d, size_t s) : prod(p), dot(d), start(s) {}
    bool operator==(const State &other) const
    {
        return prod.lhs == other.prod.lhs && prod.rhs == other.prod.rhs && dot == other.dot && start == other.start;
    }
};

class EarleyParser
{
private:
    std::vector<Production> grammar = {
        {"Program", {"StatementList"}},
        {"StatementList", {"Statement"}},
        {"StatementList", {"Statement", "StatementList"}},
        {"Statement", {"Declaration"}},
        {"Statement", {"Assignment"}},
        {"Statement", {"PrintStatement"}},
        {"Statement", {"IfStatement"}},
        {"Statement", {"WhileStatement"}},
        {"Statement", {"Block"}},
        {"Block", {"{", "StatementList", "}"}},
        {"Declaration", {"int", "IDENTIFIER", ";"}},
        {"Declaration", {"int", "IDENTIFIER", "=", "Expression", ";"}},
        {"Declaration", {"int", "IDENTIFIER", "ArrayDims", ";"}},
        {"ArrayDims", {"[", "NUMBER", "]"}},
        {"ArrayDims", {"[", "NUMBER", "]", "ArrayDims"}},
        {"Assignment", {"LValue", "=", "Expression", ";"}},
        {"LValue", {"IDENTIFIER"}},
        {"LValue", {"IDENTIFIER", "ArrayAccess"}},
        {"ArrayAccess", {"[", "Expression", "]"}},
        {"ArrayAccess", {"[", "Expression", "]", "ArrayAccess"}},
        {"PrintStatement", {"print", "(", "Expression", ")", ";"}},
        {"PrintStatement", {"print", "(", "STRING_LITERAL", ")", ";"}},
        {"IfStatement", {"if", "(", "Expression", ")", "Block"}},
        {"IfStatement", {"if", "(", "Expression", ")", "Block", "else", "Block"}},
        {"WhileStatement", {"while", "(", "Expression", ")", "Block"}},
        {"Expression", {"Term"}},
        {"Expression", {"Expression", "OPERATOR", "Term"}},
        {"Term", {"Factor"}},
        {"Term", {"Term", "OPERATOR", "Factor"}},
        {"Factor", {"NUMBER"}},
        {"Factor", {"LValue"}},
        {"Factor", {"(", "Expression", ")"}}};

    bool isNonTerminal(const std::string &symbol)
    {
        return std::any_of(grammar.begin(), grammar.end(), [&symbol](const auto &prod)
                           { return prod.lhs == symbol; });
    }

    std::string tokenTypeToString(TokenType type) const
    {
        switch (type)
        {
        case KEYWORD:
            return "KEYWORD";
        case IDENTIFIER:
            return "IDENTIFIER";
        case NUMBER:
            return "NUMBER";
        case OPERATOR:
            return "OPERATOR";
        case PUNCTUATION:
            return "PUNCTUATION";
        case STRING_LITERAL:
            return "STRING_LITERAL";
        case EOF_TOKEN:
            return "EOF_TOKEN";
        default:
            return "";
        }
    }

    void predict(std::vector<State> &chart, size_t i, const std::string &nonTerminal)
    {
        for (const auto &prod : grammar)
        {
            if (prod.lhs == nonTerminal)
            {
                State newState(prod, 0, i);
                if (std::find(chart.begin(), chart.end(), newState) == chart.end())
                    chart.push_back(newState);
            }
        }
    }

    void scan(std::vector<std::vector<State>> &chart, size_t i, const Token &token)
    {
        for (const auto &state : chart[i])
        {
            if (state.dot < state.prod.rhs.size())
            {
                std::string next = state.prod.rhs[state.dot];
                std::string tokenTypeStr = tokenTypeToString(token.type);
                if (next == tokenTypeStr || next == token.value)
                {
                    State newState(state.prod, state.dot + 1, state.start);
                    if (std::find(chart[i + 1].begin(), chart[i + 1].end(), newState) == chart[i + 1].end())
                        chart[i + 1].push_back(newState);
                }
            }
        }
    }

    void complete(std::vector<std::vector<State>> &chart, size_t i)
    {
        size_t lastSize = 0;
        while (lastSize != chart[i].size())
        {
            lastSize = chart[i].size();
            for (size_t j = 0; j < chart[i].size(); j++)
            {
                auto state = chart[i][j];
                if (state.dot == state.prod.rhs.size())
                {
                    for (auto &prev : chart[state.start])
                    {
                        if (prev.dot < prev.prod.rhs.size() && prev.prod.rhs[prev.dot] == state.prod.lhs)
                        {
                            State newState(prev.prod, prev.dot + 1, prev.start);
                            if (std::find(chart[i].begin(), chart[i].end(), newState) == chart[i].end())
                                chart[i].push_back(newState);
                        }
                    }
                }
            }
        }
    }

    std::vector<std::vector<State>> parse(const std::vector<Token> &tokens)
    {
        std::vector<std::vector<State>> chart(tokens.size() + 1);
        chart[0].push_back({{"Program", {"StatementList"}}, 0, 0});
        for (size_t i = 0; i <= tokens.size(); i++)
        {
            size_t lastSize = 0;
            while (lastSize != chart[i].size())
            {
                lastSize = chart[i].size();
                for (size_t j = 0; j < chart[i].size(); j++)
                {
                    auto s = chart[i][j];
                    if (s.dot < s.prod.rhs.size())
                    {
                        std::string next = s.prod.rhs[s.dot];
                        if (isNonTerminal(next))
                            predict(chart[i], i, next);
                        else if (i < tokens.size())
                            scan(chart, i, tokens[i]);
                    }
                    else
                        complete(chart, i);
                }
            }
        }
        State accept({{"Program", {"StatementList"}}, 1, 0});
        if (std::find(chart[tokens.size()].begin(), chart[tokens.size()].end(), accept) == chart[tokens.size()].end())
        {
            size_t max_i = 0;
            for (size_t i = 0; i <= tokens.size(); ++i)
                if (!chart[i].empty())
                    max_i = i;
            std::string error_token = (max_i < tokens.size()) ? tokens[max_i].value : "EOF";
            std::string error_type = (max_i < tokens.size()) ? tokenTypeToString(tokens[max_i].type) : "EOF_TOKEN";
            throw std::runtime_error("Syntax error at token " + std::to_string(max_i) +
                                     ", near '" + error_token + "' (type: " + error_type + ")");
        }
        return chart;
    }

    std::vector<ASTNode *> buildArrayDims(const std::vector<Token> &tokens, size_t start, size_t end)
    {
        std::vector<ASTNode *> dims;
        size_t current = start;
        while (current < end && tokens[current].value == "[")
        {
            size_t numStart = current + 1;
            if (tokens[numStart].type != NUMBER)
                throw std::runtime_error("Expected NUMBER in array dimension");
            auto *dimNode = new ASTNode("NUMBER", tokens[numStart].value);
            dims.push_back(dimNode);
            if (tokens[numStart + 1].value != "]")
                throw std::runtime_error("Expected ']'");
            current = numStart + 2;
        }
        return dims;
    }

    ASTNode *buildLValue(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        if (tokens[start].type != IDENTIFIER)
            throw std::runtime_error("LValue must start with IDENTIFIER");
        std::string var = tokens[start].value;
        size_t current = start + 1;
        std::vector<ASTNode *> indices;
        while (current < end && tokens[current].value == "[")
        {
            size_t exprStart = current + 1;
            size_t depth = 1;
            size_t exprEnd = exprStart;
            while (exprEnd < end && depth > 0)
            {
                if (tokens[exprEnd].value == "[")
                    depth++;
                else if (tokens[exprEnd].value == "]")
                    depth--;
                exprEnd++;
            }
            if (depth != 0)
                throw std::runtime_error("Mismatched brackets");
            exprEnd--;
            auto *expr = buildExpression(chart, tokens, exprStart, exprEnd);
            indices.push_back(expr);
            current = exprEnd + 1;
        }
        if (indices.empty())
            return new ASTNode("IDENTIFIER", var);
        else
        {
            auto *arrayAccess = new ASTNode("ArrayAccess", var);
            for (auto *idx : indices)
                arrayAccess->children.push_back(idx);
            return arrayAccess;
        }
    }

    ASTNode *buildFactor(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (const auto &state : chart[end])
        {
            if (state.prod.lhs == "Factor" && state.dot == state.prod.rhs.size() && state.start == start)
            {
                if (state.prod.rhs[0] == "NUMBER")
                    return new ASTNode("NUMBER", tokens[start].value);
                else if (state.prod.rhs[0] == "LValue")
                    return buildLValue(chart, tokens, start, end);
                else if (state.prod.rhs[0] == "(")
                    return buildExpression(chart, tokens, start + 1, end - 1);
            }
        }
        throw std::runtime_error("Invalid factor from " + std::to_string(start) + " to " + std::to_string(end));
    }

    ASTNode *buildTerm(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (size_t i = end - 1; i >= start; i--)
        {
            for (const auto &state : chart[end])
            {
                if (state.prod.lhs == "Term" && state.dot == state.prod.rhs.size() && state.start == start)
                {
                    if (state.prod.rhs.size() == 1)
                        return buildFactor(chart, tokens, start, end);
                    else if (state.prod.rhs.size() == 3 && tokens[i].type == OPERATOR)
                    {
                        auto *node = new ASTNode("BinaryExpression", tokens[i].value);
                        node->children.push_back(buildTerm(chart, tokens, start, i));
                        node->children.push_back(buildFactor(chart, tokens, i + 1, end));
                        return node;
                    }
                }
            }
            if (i == start)
                break;
        }
        return buildFactor(chart, tokens, start, end);
    }

    ASTNode *buildExpression(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (size_t i = end - 1; i >= start; i--)
        {
            for (const auto &state : chart[end])
            {
                if (state.prod.lhs == "Expression" && state.dot == state.prod.rhs.size() && state.start == start)
                {
                    if (state.prod.rhs.size() == 1)
                        return buildTerm(chart, tokens, start, end);
                    else if (state.prod.rhs.size() == 3 && tokens[i].type == OPERATOR)
                    {
                        auto *node = new ASTNode("BinaryExpression", tokens[i].value);
                        node->children.push_back(buildExpression(chart, tokens, start, i));
                        node->children.push_back(buildTerm(chart, tokens, i + 1, end));
                        return node;
                    }
                }
            }
            if (i == start)
                break;
        }
        return buildTerm(chart, tokens, start, end);
    }

    ASTNode *buildBlock(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (const auto &state : chart[end])
        {
            if (state.prod.lhs == "Block" && state.dot == state.prod.rhs.size() && state.start == start)
            {
                if (tokens[start].value != "{" || tokens[end - 1].value != "}")
                    throw std::runtime_error("Invalid block syntax");
                return buildStatementList(chart, tokens, start + 1, end - 1);
            }
        }
        throw std::runtime_error("Invalid block from " + std::to_string(start) + " to " + std::to_string(end));
    }

    ASTNode *buildDeclaration(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        bool isArray = false;
        for (size_t i = start + 2; i < end; i++)
        {
            if (tokens[i].value == "[")
            {
                isArray = true;
                break;
            }
        }
        if (isArray)
        {
            if (tokens[start].type != KEYWORD || tokens[start].value != "int" || tokens[start + 1].type != IDENTIFIER)
                throw std::runtime_error("Invalid array declaration syntax");
            std::string varName = tokens[start + 1].value;
            auto *arrayDecl = new ASTNode("ArrayDeclaration", varName);
            size_t i = start + 2;
            while (i < end && i + 2 < end)
            {
                if (tokens[i].value == "[" && tokens[i + 1].type == NUMBER && tokens[i + 2].value == "]")
                {
                    arrayDecl->children.push_back(new ASTNode("NUMBER", tokens[i + 1].value));
                    i += 3;
                }
                else
                    break;
            }
            return arrayDecl;
        }
        if (tokens[start].type == KEYWORD && tokens[start + 1].type == IDENTIFIER)
        {
            if (end - start == 3 && tokens[end - 1].value == ";")
            {
                auto *decl = new ASTNode("Declaration");
                decl->children.push_back(new ASTNode("IDENTIFIER", tokens[start + 1].value));
                return decl;
            }
            else if (end - start >= 5 && tokens[start + 2].value == "=" && tokens[end - 1].value == ";")
            {
                auto *decl = new ASTNode("Declaration");
                decl->children.push_back(new ASTNode("IDENTIFIER", tokens[start + 1].value));
                decl->children.push_back(buildExpression(chart, tokens, start + 3, end - 1));
                return decl;
            }
        }
        throw std::runtime_error("Invalid declaration syntax");
    }

    ASTNode *buildAssignment(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (size_t k = start + 1; k < end; k++)
        {
            for (const auto &state : chart[k])
            {
                if (state.prod.lhs == "LValue" && state.dot == state.prod.rhs.size() && state.start == start)
                {
                    if (k < end && tokens[k].value == "=")
                    {
                        for (size_t m = k + 1; m < end; m++)
                        {
                            for (const auto &state2 : chart[m])
                            {
                                if (state2.prod.lhs == "Expression" && state2.dot == state2.prod.rhs.size() && state2.start == k + 1)
                                {
                                    if (m < end && tokens[m].value == ";")
                                    {
                                        auto *assign = new ASTNode("Assignment");
                                        assign->children.push_back(buildLValue(chart, tokens, start, k));
                                        assign->children.push_back(buildExpression(chart, tokens, k + 1, m));
                                        return assign;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        throw std::runtime_error("Invalid assignment from " + std::to_string(start) + " to " + std::to_string(end));
    }

    ASTNode *buildPrintStatement(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (const auto &state : chart[end])
        {
            if (state.prod.lhs == "PrintStatement" && state.dot == state.prod.rhs.size() && state.start == start)
            {
                auto *print = new ASTNode("PrintStatement");
                if (state.prod.rhs == std::vector<std::string>{"print", "(", "STRING_LITERAL", ")", ";"})
                    print->children.push_back(new ASTNode("STRING_LITERAL", tokens[start + 2].value));
                else if (state.prod.rhs == std::vector<std::string>{"print", "(", "Expression", ")", ";"})
                    print->children.push_back(buildExpression(chart, tokens, start + 2, end - 2));
                return print;
            }
        }
        throw std::runtime_error("Invalid print statement from " + std::to_string(start) + " to " + std::to_string(end));
    }

    ASTNode *buildIfStatement(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (const auto &state : chart[end])
        {
            if (state.prod.lhs == "IfStatement" && state.dot == state.prod.rhs.size() && state.start == start)
            {
                if (state.prod.rhs.size() == 5)
                {
                    size_t exprStart = start + 1;
                    size_t parenDepth = 1;
                    size_t exprEnd = exprStart + 1;
                    while (exprEnd < end && parenDepth > 0)
                    {
                        if (tokens[exprEnd].value == "(")
                            parenDepth++;
                        else if (tokens[exprEnd].value == ")")
                            parenDepth--;
                        exprEnd++;
                    }
                    if (parenDepth != 0)
                        throw std::runtime_error("Mismatched parentheses in if");
                    auto *ifNode = new ASTNode("IfStatement");
                    ifNode->children.push_back(buildExpression(chart, tokens, exprStart + 1, exprEnd - 1));
                    size_t blockStart = exprEnd;
                    size_t blockEnd = end;
                    ifNode->children.push_back(buildBlock(chart, tokens, blockStart, blockEnd));
                    return ifNode;
                }
                else if (state.prod.rhs.size() == 7)
                {
                    size_t exprStart = start + 1;
                    size_t parenDepth = 1;
                    size_t exprEnd = exprStart + 1;
                    while (exprEnd < end && parenDepth > 0)
                    {
                        if (tokens[exprEnd].value == "(")
                            parenDepth++;
                        else if (tokens[exprEnd].value == ")")
                            parenDepth--;
                        exprEnd++;
                    }
                    if (parenDepth != 0)
                        throw std::runtime_error("Mismatched parentheses in if");
                    size_t elsePos = 0;
                    for (size_t i = exprEnd; i < end; i++)
                    {
                        if (tokens[i].type == KEYWORD && tokens[i].value == "else")
                        {
                            elsePos = i;
                            break;
                        }
                    }
                    if (elsePos == 0)
                        throw std::runtime_error("Cannot find else keyword");
                    auto *ifNode = new ASTNode("IfElseStatement");
                    ifNode->children.push_back(buildExpression(chart, tokens, exprStart + 1, exprEnd - 1));
                    size_t ifBlockStart = exprEnd;
                    size_t ifBlockEnd = elsePos;
                    ifNode->children.push_back(buildBlock(chart, tokens, ifBlockStart, ifBlockEnd));
                    size_t elseBlockStart = elsePos + 1;
                    size_t elseBlockEnd = end;
                    ifNode->children.push_back(buildBlock(chart, tokens, elseBlockStart, elseBlockEnd));
                    return ifNode;
                }
            }
        }
        throw std::runtime_error("Invalid if statement from " + std::to_string(start) + " to " + std::to_string(end));
    }

    ASTNode *buildWhileStatement(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (const auto &state : chart[end])
        {
            if (state.prod.lhs == "WhileStatement" && state.dot == state.prod.rhs.size() && state.start == start)
            {
                auto *whileNode = new ASTNode("WhileStatement");
                size_t exprStart = start + 1;
                size_t parenDepth = 1;
                size_t exprEnd = exprStart + 1;
                while (exprEnd < end && parenDepth > 0)
                {
                    if (tokens[exprEnd].value == "(")
                        parenDepth++;
                    else if (tokens[exprEnd].value == ")")
                        parenDepth--;
                    exprEnd++;
                }
                if (parenDepth != 0)
                    throw std::runtime_error("Mismatched parentheses in while");
                whileNode->children.push_back(buildExpression(chart, tokens, exprStart + 1, exprEnd - 1));
                size_t blockStart = exprEnd;
                size_t blockEnd = end;
                whileNode->children.push_back(buildBlock(chart, tokens, blockStart, blockEnd));
                return whileNode;
            }
        }
        throw std::runtime_error("Invalid while statement from " + std::to_string(start) + " to " + std::to_string(end));
    }

    ASTNode *buildStatement(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        for (const auto &state : chart[end])
        {
            if (state.prod.lhs == "Statement" && state.dot == state.prod.rhs.size() && state.start == start)
            {
                if (state.prod.rhs.size() == 1)
                {
                    std::string stmtType = state.prod.rhs[0];
                    if (stmtType == "Declaration")
                        return buildDeclaration(chart, tokens, start, end);
                    else if (stmtType == "Assignment")
                        return buildAssignment(chart, tokens, start, end);
                    else if (stmtType == "PrintStatement")
                        return buildPrintStatement(chart, tokens, start, end);
                    else if (stmtType == "IfStatement")
                        return buildIfStatement(chart, tokens, start, end);
                    else if (stmtType == "WhileStatement")
                        return buildWhileStatement(chart, tokens, start, end);
                    else if (stmtType == "Block")
                        return buildBlock(chart, tokens, start, end);
                }
            }
        }
        std::string tokenContext;
        for (size_t i = start; i < std::min(start + 5, tokens.size()); i++)
            tokenContext += tokens[i].value + " ";
        throw std::runtime_error("Invalid statement from " + std::to_string(start) +
                                 " to " + std::to_string(end) +
                                 ". Token context: " + tokenContext);
    }

    ASTNode *buildStatementList(const std::vector<std::vector<State>> &chart, const std::vector<Token> &tokens, size_t start, size_t end)
    {
        auto *root = new ASTNode("StatementList");
        size_t currentPos = start;
        if (start >= end)
            return root;
        if (tokens[start].value == "{" && tokens[end - 1].value == "}")
        {
            start++;
            end--;
        }
        currentPos = start;
        while (currentPos < end)
        {
            bool found = false;
            for (size_t i = currentPos + 1; i <= end; ++i)
            {
                if (i < end && tokens[i - 1].value == ";" &&
                    (i >= end || tokens[i].type != PUNCTUATION || tokens[i].value != ")"))
                {
                    for (const auto &state : chart[i])
                    {
                        if (state.prod.lhs == "Statement" && state.dot == state.prod.rhs.size() && state.start == currentPos)
                        {
                            root->children.push_back(buildStatement(chart, tokens, currentPos, i));
                            currentPos = i;
                            found = true;
                            break;
                        }
                    }
                    if (found)
                        break;
                }
                else if (tokens[currentPos].value == "{")
                {
                    int braceCount = 1;
                    size_t blockEnd = currentPos + 1;
                    while (blockEnd < end && braceCount > 0)
                    {
                        if (tokens[blockEnd].value == "{")
                            braceCount++;
                        else if (tokens[blockEnd].value == "}")
                            braceCount--;
                        blockEnd++;
                    }
                    if (braceCount == 0)
                    {
                        for (const auto &state : chart[blockEnd])
                        {
                            if (state.prod.lhs == "Statement" && state.dot == state.prod.rhs.size() && state.start == currentPos)
                            {
                                root->children.push_back(buildStatement(chart, tokens, currentPos, blockEnd));
                                currentPos = blockEnd;
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                }
            }
            if (!found)
            {
                for (size_t i = end; i > currentPos; --i)
                {
                    for (const auto &state : chart[i])
                    {
                        if (state.prod.lhs == "Statement" && state.dot == state.prod.rhs.size() && state.start == currentPos)
                        {
                            root->children.push_back(buildStatement(chart, tokens, currentPos, i));
                            currentPos = i;
                            found = true;
                            break;
                        }
                    }
                    if (found)
                        break;
                }
            }
            if (!found)
            {
                std::string context;
                for (size_t j = currentPos; j < std::min(currentPos + 10, tokens.size()); j++)
                    context += tokens[j].value + " ";
                throw std::runtime_error("No valid statement found at position " +
                                         std::to_string(currentPos) + " to " +
                                         std::to_string(end) +
                                         ". Context: " + context);
            }
        }
        return root;
    }

public:
    ASTNode *parse_input(const std::string &input)
    {
        auto tokens = tokenize(input);
        std::cout << "Tokenization successful, tokens: ";
        for (const auto &token : tokens)
            std::cout << token.value << " ";
        std::cout << "\n";
        if (!tokens.empty() && tokens.back().type == EOF_TOKEN)
            tokens.pop_back();
        auto chart = parse(tokens);
        std::cout << "\nChart built, size: " << chart.size() << "\n";
        ASTNode *ast = buildStatementList(chart, tokens, 0, tokens.size());
        std::cout << "\nAST built, statements: " << ast->children.size() << "\n";
        return ast;
    }
};

std::map<std::string, std::vector<int>> semantic_analyze(ASTNode *ast)
{
    std::cout << "Starting semantic analysis\n";
    std::map<std::string, std::vector<int>> declared_vars;
    std::function<void(ASTNode *)> collect_declarations = [&](ASTNode *node)
    {
        if (node->type == "StatementList")
        {
            for (auto *child : node->children)
            {
                std::cout << "Processing: " << child->type << "\n";
                if (child->type == "Declaration")
                {
                    if (!child->children.empty() && child->children[0]->type == "IDENTIFIER")
                        declared_vars[child->children[0]->value] = {};
                }
                else if (child->type == "ArrayDeclaration")
                {
                    std::vector<int> dims;
                    for (auto *dimNode : child->children)
                        dims.push_back(std::stoi(dimNode->value));
                    declared_vars[child->value] = dims;
                }
                else if (child->type == "IfStatement" || child->type == "IfElseStatement")
                {
                    collect_declarations(child->children[1]);
                    if (child->type == "IfElseStatement")
                        collect_declarations(child->children[2]);
                }
                else if (child->type == "WhileStatement")
                {
                    collect_declarations(child->children[1]);
                }
            }
        }
    };
    collect_declarations(ast);
    std::cout << "\nDeclarations collected\n";
    struct CheckExpr
    {
        const std::map<std::string, std::vector<int>> &declared_vars;
        CheckExpr(const std::map<std::string, std::vector<int>> &vars) : declared_vars(vars) {}
        void operator()(ASTNode *node, const CheckExpr &self) const
        {
            if (node->type == "NUMBER")
                return;
            if (node->type == "IDENTIFIER")
            {
                std::string var = node->value;
                if (declared_vars.find(var) == declared_vars.end())
                    throw std::runtime_error("Undeclared variable: " + var);
                if (!declared_vars.at(var).empty())
                    throw std::runtime_error("Variable " + var + " is an array, cannot use directly");
            }
            else if (node->type == "ArrayAccess")
            {
                std::string var = node->value;
                if (declared_vars.find(var) == declared_vars.end())
                    throw std::runtime_error("Undeclared array: " + var);
                const auto &dims = declared_vars.at(var);
                if (dims.empty())
                    throw std::runtime_error("Variable " + var + " is not an array");
                if (node->children.size() != dims.size())
                    throw std::runtime_error("Dimension mismatch for array " + var);
                for (auto *idx : node->children)
                    self(idx, self);
            }
            else if (node->type == "BinaryExpression")
            {
                self(node->children[0], self);
                self(node->children[1], self);
            }
        }
    };
    CheckExpr check_expr(declared_vars);
    std::function<void(ASTNode *)> check_usage = [&](ASTNode *node)
    {
        if (node->type == "StatementList")
        {
            for (auto *child : node->children)
            {
                if (child->type == "Assignment")
                {
                    ASTNode *lvalue = child->children[0];
                    if (lvalue->type == "IDENTIFIER")
                    {
                        std::string var = lvalue->value;
                        if (declared_vars.find(var) == declared_vars.end())
                            throw std::runtime_error("Undeclared variable: " + var);
                        if (!declared_vars.at(var).empty())
                            throw std::runtime_error("Variable " + var + " is an array, cannot assign directly");
                        check_expr(child->children[1], check_expr);
                    }
                    else if (lvalue->type == "ArrayAccess")
                    {
                        std::string var = lvalue->value;
                        if (declared_vars.find(var) == declared_vars.end())
                            throw std::runtime_error("Undeclared array: " + var);
                        const auto &dims = declared_vars.at(var);
                        if (dims.empty())
                            throw std::runtime_error("Variable " + var + " is not an array");
                        if (lvalue->children.size() != dims.size())
                            throw std::runtime_error("Dimension mismatch for array " + var);
                        for (auto *idx : lvalue->children)
                            check_expr(idx, check_expr);
                        check_expr(child->children[1], check_expr);
                    }
                }
                else if (child->type == "PrintStatement")
                {
                    if (child->children[0]->type != "STRING_LITERAL")
                        check_expr(child->children[0], check_expr);
                }
                else if (child->type == "IfStatement" || child->type == "IfElseStatement")
                {
                    check_expr(child->children[0], check_expr);
                    check_usage(child->children[1]);
                    if (child->type == "IfElseStatement")
                        check_usage(child->children[2]);
                }
                else if (child->type == "WhileStatement")
                {
                    check_expr(child->children[0], check_expr);
                    check_usage(child->children[1]);
                }
            }
        }
    };
    check_usage(ast);
    return declared_vars;
}

std::vector<Instruction> generate_intermediate_code(ASTNode *ast, const std::map<std::string, std::vector<int>> &declared_vars)
{
    std::cout << "Generating intermediate code\n";
    std::vector<Instruction> code;
    int temp_count = 0;
    int label_count = 0;
    auto gen_temp = [&temp_count]()
    { return "t" + std::to_string(temp_count++); };
    auto gen_label = [&label_count]()
    { return "L" + std::to_string(label_count++); };
    struct GenExpr
    {
        std::vector<Instruction> &code;
        std::function<std::string()> gen_temp;
        const std::map<std::string, std::vector<int>> &declared_vars;
        GenExpr(std::vector<Instruction> &c, std::function<std::string()> gt, const std::map<std::string, std::vector<int>> &dv)
            : code(c), gen_temp(gt), declared_vars(dv) {}
        std::string operator()(ASTNode *node, const GenExpr &self) const
        {
            if (node->type == "NUMBER")
                return node->value;
            if (node->type == "IDENTIFIER")
                return node->value;
            if (node->type == "ArrayAccess")
            {
                std::string var = node->value;
                const auto &dims = declared_vars.at(var);
                if (dims.size() != node->children.size())
                    throw std::runtime_error("Dimension mismatch for array " + var);
                std::string index = self(node->children[0], self);
                for (size_t k = 1; k < node->children.size(); k++)
                {
                    std::string dim = std::to_string(dims[k]);
                    std::string temp1 = gen_temp();
                    code.emplace_back("*", index, dim, temp1);
                    std::string ik = self(node->children[k], self);
                    std::string temp2 = gen_temp();
                    code.emplace_back("+", temp1, ik, temp2);
                    index = temp2;
                }
                std::string result = gen_temp();
                code.emplace_back("load_array", var, index, result);
                return result;
            }
            if (node->type == "BinaryExpression")
            {
                std::string left = self(node->children[0], self);
                std::string right = self(node->children[1], self);
                std::string temp = gen_temp();
                code.emplace_back(node->value, left, right, temp);
                return temp;
            }
            throw std::runtime_error("Unknown expression type: " + node->type);
        }
    };
    GenExpr gen_expr(code, gen_temp, declared_vars);
    std::function<void(ASTNode *)> generate = [&](ASTNode *node)
    {
        if (node->type == "StatementList")
        {
            for (auto *child : node->children)
            {
                if (child->type == "Declaration")
                {
                    if (child->children.size() > 1)
                    {
                        std::string result = gen_expr(child->children[1], gen_expr);
                        code.emplace_back("=", result, "", child->children[0]->value);
                    }
                }
                else if (child->type == "Assignment")
                {
                    ASTNode *lvalue = child->children[0];
                    std::string value = gen_expr(child->children[1], gen_expr);
                    if (lvalue->type == "IDENTIFIER")
                        code.emplace_back("=", value, "", lvalue->value);
                    else if (lvalue->type == "ArrayAccess")
                    {
                        std::string var = lvalue->value;
                        const auto &dims = declared_vars.at(var);
                        if (dims.size() != lvalue->children.size())
                            throw std::runtime_error("Dimension mismatch for array " + var);
                        std::string index = gen_expr(lvalue->children[0], gen_expr);
                        for (size_t k = 1; k < lvalue->children.size(); k++)
                        {
                            std::string dim = std::to_string(dims[k]);
                            std::string temp1 = gen_temp();
                            code.emplace_back("*", index, dim, temp1);
                            std::string ik = gen_expr(lvalue->children[k], gen_expr);
                            std::string temp2 = gen_temp();
                            code.emplace_back("+", temp1, ik, temp2);
                            index = temp2;
                        }
                        code.emplace_back("store_array", var, index, value);
                    }
                }
                else if (child->type == "PrintStatement")
                {
                    if (child->children[0]->type == "STRING_LITERAL")
                        code.emplace_back("print_str", child->children[0]->value, "", "");
                    else
                    {
                        std::string result = gen_expr(child->children[0], gen_expr);
                        code.emplace_back("print", result, "", "");
                    }
                }
                else if (child->type == "IfStatement")
                {
                    std::string cond = gen_expr(child->children[0], gen_expr);
                    std::string end_label = gen_label();
                    code.emplace_back("ifz", cond, "", end_label);
                    generate(child->children[1]);
                    code.emplace_back("label", "", "", end_label);
                }
                else if (child->type == "IfElseStatement")
                {
                    std::string cond = gen_expr(child->children[0], gen_expr);
                    std::string else_label = gen_label();
                    std::string end_label = gen_label();
                    code.emplace_back("ifz", cond, "", else_label);
                    generate(child->children[1]);
                    code.emplace_back("goto", "", "", end_label);
                    code.emplace_back("label", "", "", else_label);
                    generate(child->children[2]);
                    code.emplace_back("label", "", "", end_label);
                }
                else if (child->type == "WhileStatement")
                {
                    std::string start_label = gen_label();
                    std::string end_label = gen_label();
                    code.emplace_back("label", "", "", start_label);
                    std::string cond = gen_expr(child->children[0], gen_expr);
                    code.emplace_back("ifz", cond, "", end_label);
                    generate(child->children[1]);
                    code.emplace_back("goto", "", "", start_label);
                    code.emplace_back("label", "", "", end_label);
                }
            }
        }
    };
    generate(ast);
    return code;
}

std::string read_file(const std::string &filename)
{
    std::ifstream file(filename);
    if (!file.is_open())
    {
        std::cerr << "Error: Could not open the file " << filename << std::endl;
        return "";
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

void delete_ast(ASTNode *node)
{
    for (auto *child : node->children)
        delete_ast(child);
    delete node;
}

int main()
{
    try
    {
        std::string input = read_file("input.txt");
        std::cout << "Input read successfully\n\n";
        EarleyParser parser;
        ASTNode *ast = parser.parse_input(input);
        std::cout << "Parsing successful\n\n";
        auto vars = semantic_analyze(ast);
        std::cout << "Semantic analysis successful\n\n";
        std::cout << "Declared vars: ";
        for (const auto &var : vars)
        {
            std::cout << var.first;
            if (!var.second.empty())
            {
                std::cout << "[";
                for (size_t i = 0; i < var.second.size(); i++)
                {
                    std::cout << var.second[i];
                    if (i < var.second.size() - 1)
                        std::cout << "][";
                }
                std::cout << "]";
            }
            std::cout << " ";
        }
        std::cout << "\n\n";
        auto code = generate_intermediate_code(ast, vars);
        std::cout << "Code generation successful\n\n";
        for (const auto &instr : code)
            std::cout << instr.op << " " << instr.arg1 << " " << instr.arg2 << " " << instr.result << "\n";
        delete_ast(ast);
    }
    catch (const std::exception &e)
    {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
    catch (...)
    {
        std::cerr << "Unknown error occurred\n";
        return 1;
    }
    return 0;
}