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
#include <memory>
#include <format>
#include <ranges>

class CompilerException : public std::exception {
public:
    explicit CompilerException(const std::string& message) : msg(message) {}
    const char* what() const noexcept override { return msg.c_str(); }
private:
    std::string msg;
};

struct ASTNode {
    std::string type;
    std::string value;
    std::vector<std::unique_ptr<ASTNode>> children;
    ASTNode(const std::string& t, const std::string& v = "") : type(t), value(v) {}
};

struct Instruction {
    std::string op;
    std::string arg1;
    std::string arg2;
    std::string result;
    Instruction(const std::string& o, const std::string& a1, const std::string& a2, const std::string& r)
        : op(o), arg1(a1), arg2(a2), result(r) {}
};

enum class TokenType {
    KEYWORD,
    IDENTIFIER,
    NUMBER,
    OPERATOR,
    PUNCTUATION,
    STRING_LITERAL,
    EOF_TOKEN
};

struct Token {
    TokenType type;
    std::string value;
};

Token getStringLiteral(const std::string& input, size_t& i) {
    size_t start = i;
    i += 1;
    while (i < input.length() && input[i] != '"')
        i += 1;
    if (i == input.length())
        throw CompilerException("Unterminated string literal");
    std::string str = input.substr(start + 1, i - start - 1);
    i += 1;
    return {TokenType::STRING_LITERAL, str};
}

Token getIdentifierOrKeyword(const std::string& input, size_t& i) {
    std::string word;
    while (i < input.length() && std::isalnum(input[i]))
        word += input[i++];
    if (word == "int" || word == "print" || word == "if" || word == "else" || word == "while")
        return {TokenType::KEYWORD, word};
    return {TokenType::IDENTIFIER, word};
}

Token getNumber(const std::string& input, size_t& i) {
    std::string num;
    while (i < input.length() && std::isdigit(input[i]))
        num += input[i++];
    return {TokenType::NUMBER, num};
}

Token getOperator(const std::string& input, size_t& i) {
    char c = input[i];
    if (i + 1 < input.length() && input[i + 1] == '=' && 
        (c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>')) {
        std::string op = std::string(1, c) + "=";
        i += 2;
        return {TokenType::OPERATOR, op};
    }
    i += 1;
    return {TokenType::OPERATOR, std::string(1, c)};
}

Token getPunctuation(const std::string& input, size_t& i) {
    char c = input[i];
    i += 1;
    return {TokenType::PUNCTUATION, std::string(1, c)};
}

std::vector<Token> tokenize(const std::string& input) {
    std::vector<Token> tokens;
    size_t i = 0;
    while (i < input.length()) {
        char c = input[i];
        if (std::isspace(c) || c == '\n') {
            i += 1;
            continue;
        }
        if (c == '"')
            tokens.emplace_back(getStringLiteral(input, i));
        else if (std::isalpha(c))
            tokens.emplace_back(getIdentifierOrKeyword(input, i));
        else if (std::isdigit(c))
            tokens.emplace_back(getNumber(input, i));
        else if (c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>')
            tokens.emplace_back(getOperator(input, i));
        else if (c == ';' || c == '(' || c == ')' || c == '{' || c == '}' || c == '[' || c == ']')
            tokens.emplace_back(getPunctuation(input, i));
        else
            throw CompilerException(std::format("Invalid character: '{}'", c));
    }
    tokens.emplace_back(TokenType::EOF_TOKEN, "");
    return tokens;
}

struct Production {
    std::string lhs;
    std::vector<std::string> rhs;
    Production(const std::string& l, const std::vector<std::string>& r) : lhs(l), rhs(r) {}
};

struct State {
    Production prod;
    size_t dot;
    size_t start;
    State(const Production& p, size_t d, size_t s) : prod(p), dot(d), start(s) {}
    bool operator==(const State& other) const {
        return prod.lhs == other.prod.lhs && prod.rhs == other.prod.rhs && dot == other.dot && start == other.start;
    }
};

class EarleyParser {
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
        {"Factor", {"(", "Expression", ")"}}
    };

    bool isNonTerminal(std::string_view symbol) const {
        return std::ranges::any_of(grammar, [symbol](const auto& prod) { return prod.lhs == symbol; });
    }

    std::string tokenTypeToString(TokenType type) const {
        using enum TokenType;
        switch (type) {
            case KEYWORD: return "KEYWORD";
            case IDENTIFIER: return "IDENTIFIER";
            case NUMBER: return "NUMBER";
            case OPERATOR: return "OPERATOR";
            case PUNCTUATION: return "PUNCTUATION";
            case STRING_LITERAL: return "STRING_LITERAL";
            case EOF_TOKEN: return "EOF_TOKEN";
            default: return "";
        }
    }

    void predict(std::vector<State>& chart, size_t i, std::string_view nonTerminal) {
        for (const auto& prod : grammar) {
            if (prod.lhs != nonTerminal)
                continue;
            State newState(prod, 0, i);
            if (std::ranges::find(chart, newState) == chart.end())
                chart.emplace_back(prod, 0, i);
        }
    }

    void scan(std::vector<std::vector<State>>& chart, size_t i, const Token& token) {
        std::string tokenTypeStr = tokenTypeToString(token.type);
        for (const auto& state : chart[i]) {
            if (state.dot >= state.prod.rhs.size())
                continue;
            if (std::string next = state.prod.rhs[state.dot]; next != tokenTypeStr && next != token.value)
                continue;
            State newState(state.prod, state.dot + 1, state.start);
            if (std::ranges::find(chart[i + 1], newState) == chart[i + 1].end())
                chart[i + 1].emplace_back(newState);
        }
    }

    void complete(std::vector<std::vector<State>>& chart, size_t i) {
        size_t lastSize;
        do {
            lastSize = chart[i].size();
            for (const auto& state : chart[i]) {
                if (state.dot != state.prod.rhs.size())
                    continue;
                for (auto& prev : chart[state.start]) {
                    if (prev.dot >= prev.prod.rhs.size() || prev.prod.rhs[prev.dot] != state.prod.lhs)
                        continue;
                    State newState(prev.prod, prev.dot + 1, prev.start);
                    if (std::ranges::find(chart[i], newState) == chart[i].end())
                        chart[i].emplace_back(newState);
                }
            }
        } while (lastSize != chart[i].size());
    }

    std::vector<std::vector<State>> parse(const std::vector<Token>& tokens) {
        std::vector<std::vector<State>> chart(tokens.size() + 1);
        chart[0].emplace_back(Production{"Program", {"StatementList"}}, 0, 0);
        for (size_t i = 0; i <= tokens.size(); i++) {
            size_t lastSize;
            do {
                lastSize = chart[i].size();
                for (const auto& s : chart[i]) {
                    if (s.dot >= s.prod.rhs.size()) {
                        complete(chart, i);
                        continue;
                    }
                    std::string next = s.prod.rhs[s.dot];
                    if (isNonTerminal(next))
                        predict(chart[i], i, next);
                    else if (i < tokens.size())
                        scan(chart, i, tokens[i]);
                }
            } while (lastSize != chart[i].size());
        }
        State accept(Production{{"Program", {"StatementList"}}, 1, 0});
        if (std::ranges::find(chart[tokens.size()], accept) == chart[tokens.size()].end()) {
            size_t max_i = 0;
            for (size_t i = 0; i <= tokens.size(); ++i)
                if (!chart[i].empty())
                    max_i = i;
            std::string error_token = (max_i < tokens.size()) ? tokens[max_i].value : "EOF";
            std::string error_type = (max_i < tokens.size()) ? tokenTypeToString(tokens[max_i].type) : "EOF_TOKEN";
            throw CompilerException(std::format("Syntax error at token {}, near '{}' (type: {})", max_i, error_token, error_type));
        }
        return chart;
    }

    std::vector<std::unique_ptr<ASTNode>> buildArrayDims(const std::vector<Token>& tokens, size_t start, size_t end) {
        std::vector<std::unique_ptr<ASTNode>> dims;
        for (size_t current = start; current < end && tokens[current].value == "["; current += 3) {
            size_t numStart = current + 1;
            if (tokens[numStart].type != TokenType::NUMBER)
                throw CompilerException("Expected NUMBER in array dimension");
            dims.emplace_back(std::make_unique<ASTNode>("NUMBER", tokens[numStart].value));
            if (tokens[numStart + 1].value != "]")
                throw CompilerException("Expected ']'");
        }
        return dims;
    }

    std::unique_ptr<ASTNode> buildLValue(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        if (tokens[start].type != TokenType::IDENTIFIER)
            throw CompilerException("LValue must start with IDENTIFIER");
        std::string var = tokens[start].value;
        size_t current = start + 1;
        std::vector<std::unique_ptr<ASTNode>> indices;
        while (current < end && tokens[current].value == "[") {
            size_t exprStart = current + 1;
            size_t depth = 1;
            size_t exprEnd = exprStart;
            while (exprEnd < end && depth > 0) {
                if (tokens[exprEnd].value == "[")
                    depth++;
                else if (tokens[exprEnd].value == "]")
                    depth--;
                exprEnd++;
            }
            if (depth != 0)
                throw CompilerException("Mismatched brackets");
            indices.push_back(buildExpression(chart, tokens, exprStart, exprEnd - 1));
            current = exprEnd + 1;
        }
        if (indices.empty())
            return std::make_unique<ASTNode>("IDENTIFIER", var);
        auto arrayAccess = std::make_unique<ASTNode>("ArrayAccess", var);
        for (auto& idx : indices)
            arrayAccess->children.push_back(std::move(idx));
        return arrayAccess;
    }

    std::unique_ptr<ASTNode> buildFactor(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (const auto& state : chart[end]) {
            if (state.prod.lhs != "Factor" || state.dot != state.prod.rhs.size() || state.start != start)
                continue;
            if (state.prod.rhs[0] == "NUMBER")
                return std::make_unique<ASTNode>("NUMBER", tokens[start].value);
            if (state.prod.rhs[0] == "LValue")
                return buildLValue(chart, tokens, start, end);
            if (state.prod.rhs[0] == "(")
                return buildExpression(chart, tokens, start + 1, end - 1);
        }
        throw CompilerException(std::format("Invalid factor from {} to {}", start, end));
    }

    std::unique_ptr<ASTNode> buildTerm(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (size_t i = end - 1; i >= start; i--) {
            for (const auto& state : chart[end]) {
                if (state.prod.lhs != "Term" || state.dot != state.prod.rhs.size() || state.start != start)
                    continue;
                if (state.prod.rhs.size() == 1)
                    return buildFactor(chart, tokens, start, end);
                if (state.prod.rhs.size() == 3 && tokens[i].type == TokenType::OPERATOR) {
                    auto node = std::make_unique<ASTNode>("BinaryExpression", tokens[i].value);
                    node->children.push_back(buildTerm(chart, tokens, start, i));
                    node->children.push_back(buildFactor(chart, tokens, i + 1, end));
                    return node;
                }
            }
            if (i == start)
                return buildFactor(chart, tokens, start, end);
        }
        return buildFactor(chart, tokens, start, end);
    }

    std::unique_ptr<ASTNode> buildExpression(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (size_t i = end - 1; i >= start; i--) {
            for (const auto& state : chart[end]) {
                if (state.prod.lhs != "Expression" || state.dot != state.prod.rhs.size() || state.start != start)
                    continue;
                if (state.prod.rhs.size() == 1)
                    return buildTerm(chart, tokens, start, end);
                if (state.prod.rhs.size() == 3 && tokens[i].type == TokenType::OPERATOR) {
                    auto node = std::make_unique<ASTNode>("BinaryExpression", tokens[i].value);
                    node->children.push_back(buildExpression(chart, tokens, start, i));
                    node->children.push_back(buildTerm(chart, tokens, i + 1, end));
                    return node;
                }
            }
            if (i == start)
                return buildTerm(chart, tokens, start, end);
        }
        return buildTerm(chart, tokens, start, end);
    }

    std::unique_ptr<ASTNode> buildBlock(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (const auto& state : chart[end]) {
            if (state.prod.lhs == "Block" && state.dot == state.prod.rhs.size() && state.start == start) {
                if (tokens[start].value != "{" || tokens[end - 1].value != "}")
                    throw CompilerException("Invalid block syntax");
                return buildStatementList(chart, tokens, start + 1, end - 1);
            }
        }
        throw CompilerException(std::format("Invalid block from {} to {}", start, end));
    }

    std::unique_ptr<ASTNode> buildDeclaration(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        if (tokens[start].type != TokenType::KEYWORD || tokens[start].value != "int" || tokens[start + 1].type != TokenType::IDENTIFIER)
            throw CompilerException("Invalid declaration syntax");
        auto decl = std::make_unique<ASTNode>("Declaration");
        decl->children.emplace_back(std::make_unique<ASTNode>("IDENTIFIER", tokens[start + 1].value));
        size_t i = start + 2;
        if (i < end && tokens[i].value == "[") {
            std::vector<int> dims;
            while (i < end && tokens[i].value == "[") {
                if (i + 2 >= end || tokens[i + 1].type != TokenType::NUMBER || tokens[i + 2].value != "]")
                    throw CompilerException("Invalid array dimension");
                dims.push_back(std::stoi(tokens[i + 1].value));
                i += 3;
            }
            decl->type = "ArrayDeclaration";
            decl->value = tokens[start + 1].value;
            for (int dim : dims)
                decl->children.emplace_back(std::make_unique<ASTNode>("NUMBER", std::to_string(dim)));
        } else if (i < end && tokens[i].value == "=") {
            size_t exprStart = i + 1;
            size_t exprEnd = end - 1;
            decl->children.push_back(buildExpression(chart, tokens, exprStart, exprEnd));
            i = exprEnd + 1;
        }
        if (i < end && tokens[i].value == ";")
            return decl;
        throw CompilerException("Invalid declaration syntax");
    }

    std::unique_ptr<ASTNode> buildAssignment(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (size_t k = start + 1; k < end; k++) {
            for (const auto& state : chart[k]) {
                if (state.prod.lhs != "LValue" || state.dot != state.prod.rhs.size() || state.start != start || tokens[k].value != "=")
                    continue;
                for (size_t m = k + 1; m < end; m++) {
                    for (const auto& state2 : chart[m]) {
                        if (state2.prod.lhs == "Expression" && state2.dot == state2.prod.rhs.size() && state2.start == k + 1 && tokens[m].value == ";") {
                            auto assign = std::make_unique<ASTNode>("Assignment");
                            assign->children.push_back(buildLValue(chart, tokens, start, k));
                            assign->children.push_back(buildExpression(chart, tokens, k + 1, m));
                            return assign;
                        }
                    }
                }
            }
        }
        throw CompilerException(std::format("Invalid assignment from {} to {}", start, end));
    }

    std::unique_ptr<ASTNode> buildPrintStatement(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (const auto& state : chart[end]) {
            if (state.prod.lhs != "PrintStatement" || state.dot != state.prod.rhs.size() || state.start != start)
                continue;
            auto print = std::make_unique<ASTNode>("PrintStatement");
            if (state.prod.rhs == std::vector<std::string>{"print", "(", "STRING_LITERAL", ")", ";"})
                print->children.emplace_back(std::make_unique<ASTNode>("STRING_LITERAL", tokens[start + 2].value));
            else if (state.prod.rhs == std::vector<std::string>{"print", "(", "Expression", ")", ";"})
                print->children.push_back(buildExpression(chart, tokens, start + 2, end - 2));
            return print;
        }
        throw CompilerException(std::format("Invalid print statement from {} to {}", start, end));
    }

    std::unique_ptr<ASTNode> buildIfStatement(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (const auto& state : chart[end]) {
            if (state.prod.lhs != "IfStatement" || state.dot != state.prod.rhs.size() || state.start != start)
                continue;
            size_t exprStart = start + 1;
            size_t parenDepth = 1;
            size_t exprEnd = exprStart + 1;
            while (exprEnd < end && parenDepth > 0) {
                if (tokens[exprEnd].value == "(")
                    parenDepth++;
                else if (tokens[exprEnd].value == ")")
                    parenDepth--;
                exprEnd++;
            }
            if (parenDepth != 0)
                throw CompilerException("Mismatched parentheses in if");
            if (state.prod.rhs.size() == 5) {
                auto ifNode = std::make_unique<ASTNode>("IfStatement");
                ifNode->children.push_back(buildExpression(chart, tokens, exprStart + 1, exprEnd - 1));
                ifNode->children.push_back(buildBlock(chart, tokens, exprEnd, end));
                return ifNode;
            }
            if (state.prod.rhs.size() == 7) {
                size_t elsePos = 0;
                for (size_t i = exprEnd; i < end && elsePos == 0; i++)
                    if (tokens[i].type == TokenType::KEYWORD && tokens[i].value == "else")
                        elsePos = i;
                if (elsePos == 0)
                    throw CompilerException("Cannot find else keyword");
                auto ifNode = std::make_unique<ASTNode>("IfElseStatement");
                ifNode->children.push_back(buildExpression(chart, tokens, exprStart + 1, exprEnd - 1));
                ifNode->children.push_back(buildBlock(chart, tokens, exprEnd, elsePos));
                ifNode->children.push_back(buildBlock(chart, tokens, elsePos + 1, end));
                return ifNode;
            }
        }
        throw CompilerException(std::format("Invalid if statement from {} to {}", start, end));
    }

    std::unique_ptr<ASTNode> buildWhileStatement(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (const auto& state : chart[end]) {
            if (state.prod.lhs != "WhileStatement" || state.dot != state.prod.rhs.size() || state.start != start)
                continue;
            size_t exprStart = start + 1;
            size_t parenDepth = 1;
            size_t exprEnd = exprStart + 1;
            while (exprEnd < end && parenDepth > 0) {
                if (tokens[exprEnd].value == "(")
                    parenDepth++;
                else if (tokens[exprEnd].value == ")")
                    parenDepth--;
                exprEnd++;
            }
            if (parenDepth != 0)
                throw CompilerException("Mismatched parentheses in while");
            auto whileNode = std::make_unique<ASTNode>("WhileStatement");
            whileNode->children.push_back(buildExpression(chart, tokens, exprStart + 1, exprEnd - 1));
            whileNode->children.push_back(buildBlock(chart, tokens, exprEnd, end));
            return whileNode;
        }
        throw CompilerException(std::format("Invalid while statement from {} to {}", start, end));
    }

    std::unique_ptr<ASTNode> buildStatement(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        for (const auto& state : chart[end]) {
            if (state.prod.lhs != "Statement" || state.dot != state.prod.rhs.size() || state.start != start)
                continue;
            if (state.prod.rhs.size() == 1) {
                std::string stmtType = state.prod.rhs[0];
                if (stmtType == "Declaration")
                    return buildDeclaration(chart, tokens, start, end);
                if (stmtType == "Assignment")
                    return buildAssignment(chart, tokens, start, end);
                if (stmtType == "PrintStatement")
                    return buildPrintStatement(chart, tokens, start, end);
                if (stmtType == "IfStatement")
                    return buildIfStatement(chart, tokens, start, end);
                if (stmtType == "WhileStatement")
                    return buildWhileStatement(chart, tokens, start, end);
                if (stmtType == "Block")
                    return buildBlock(chart, tokens, start, end);
            }
        }
        std::string tokenContext;
        for (size_t i = start; i < std::min(start + 5, tokens.size()); i++)
            tokenContext += tokens[i].value + " ";
        throw CompilerException(std::format("Invalid statement from {} to {}. Token context: {}", start, end, tokenContext));
    }

    std::unique_ptr<ASTNode> buildStatementList(const std::vector<std::vector<State>>& chart, const std::vector<Token>& tokens, size_t start, size_t end) {
        auto root = std::make_unique<ASTNode>("StatementList");
        if (start >= end)
            return root;
        if (tokens[start].value == "{" && tokens[end - 1].value == "}") {
            start++;
            end--;
        }
        size_t currentPos = start;
        while (currentPos < end) {
            bool found = false;
            for (size_t i = currentPos + 1; i <= end && !found; i++) {
                if (i < end && tokens(i - 1).value == ";") {
                    bool isPunctuation = (i < tokens.size() && tokens[i].type == TokenType::PUNCTUATION);
                    bool isClosingParen = isPunctuation && tokens[i].value == ")";
                    if (!isPunctuation || isClosingParen) {
                        for (const auto& state : chart[i]) {
                            if (state.prod.lhs == "Statement" && state.dot == state.prod.rhs.size() && state.start == currentPos) {
                                root->children.push_back(buildStatement(chart, tokens, currentPos, i));
                                currentPos = i;
                                found = true;
                                break;
                            }
                        }
                    }
                }
            }
            if (!found && tokens[currentPos].value == "{") {
                int braceCount = 1;
                size_t blockEnd = currentPos + 1;
                while (blockEnd < end && braceCount > 0) {
                    if (tokens[blockEnd].value == "{")
                        braceCount++;
                    else if (tokens[blockEnd].value == "}")
                        braceCount--;
                    blockEnd++;
                }
                if (braceCount == 0) {
                    for (const auto& state : chart[blockEnd]) {
                        if (state.prod.lhs == "Statement" && state.dot == state.prod.rhs.size() && state.start == currentPos) {
                            root->children.push_back(buildStatement(chart, tokens, currentPos, blockEnd));
                            currentPos = blockEnd;
                            found = true;
                            break;
                        }
                    }
                }
            }
            if (!found) {
                for (size_t i = end; i > currentPos && !found; i--) {
                    for (const auto& state : chart[i]) {
                        if (state.prod.lhs == "Statement" && state.dot == state.prod.rhs.size() && state.start == currentPos) {
                            root->children.push_back(buildStatement(chart, tokens, currentPos, i));
                            currentPos = i;
                            found = true;
                            break;
                        }
                    }
                }
            }
            if (!found) {
                std::string context;
                for (size_t j = currentPos; j < std::min(currentPos + 10, tokens.size()); j++)
                    context += tokens[j].value + " ";
                throw CompilerException(std::format("No valid statement found at position {} to {}. Context: {}", currentPos, end, context));
            }
        }
        return root;
    }

public:
    std::unique_ptr<ASTNode> parse_input(const std::string& input) {
        auto tokens = tokenize(input);
        std::cout << "Tokenization successful, tokens: ";
        for (const auto& token : tokens)
            std::cout << token.value << " ";
        std::cout << "\n";
        if (!tokens.empty() && tokens.back().type == TokenType::EOF_TOKEN)
            tokens.pop_back();
        auto chart = parse(tokens);
        std::cout << "\nChart built, size: " << chart.size() << "\n";
        std::unique_ptr<ASTNode> ast = buildStatementList(chart, tokens, 0, tokens.size());
        std::cout << "\nAST built, statements: " << ast->children.size() << "\n";
        return ast;
    }
};

void collectDeclarations(const ASTNode& node, std::map<std::string, std::vector<int>, std::less<>>& declared_vars) {
    if (node.type != "StatementList")
        return;
    for (const auto& child : node.children) {
        if (child->type == "Declaration") {
            if (!child->children.empty() && child->children[0]->type == "IDENTIFIER")
                declared_vars[child->children[0]->value] = {};
        } else if (child->type == "ArrayDeclaration") {
            std::vector<int> dims;
            for (const auto& dimNode : child->children)
                dims.push_back(std::stoi(dimNode->value));
            declared_vars[child->value] = dims;
        } else if (child->type == "IfStatement" || child->type == "IfElseStatement") {
            collectDeclarations(*child->children[1], declared_vars);
            if (child->type == "IfElseStatement")
                collectDeclarations(*child->children[2], declared_vars);
        } else if (child->type == "WhileStatement") {
            collectDeclarations(*child->children[1], declared_vars);
        }
    }
}

struct CheckExpr {
    const std::map<std::string, std::vector<int>, std::less<>>& declared_vars;
    explicit CheckExpr(const std::map<std::string, std::vector<int>, std::less<>>& vars) : declared_vars(vars) {}
    void operator()(const ASTNode& node) const {
        if (node.type == "NUMBER")
            return;
        if (node.type == "IDENTIFIER") {
            std::string var = node.value;
            if (!declared_vars.contains(var))
                throw CompilerException(std::format("Undeclared variable: {}", var));
            if (!declared_vars.at(var).empty())
                throw CompilerException(std::format("Variable {} is an array, cannot use directly", var));
        } else if (node.type == "ArrayAccess") {
            std::string var = node.value;
            if (!declared_vars.contains(var))
                throw CompilerException(std::format("Undeclared array: {}", var));
            const auto& dims = declared_vars.at(var);
            if (dims.empty())
                throw CompilerException(std::format("Variable {} is not an array", var));
            if (node.children.size() != dims.size())
                throw CompilerException(std::format("Dimension mismatch for array {}", var));
            for (const auto& idx : node.children)
                (*this)(*idx);
        } else if (node.type == "BinaryExpression") {
            (*this)(*node.children[0]);
            (*this)(*node.children[1]);
        }
    }
};

void checkUsage(const ASTNode& node, const CheckExpr& check_expr) {
    if (node.type != "StatementList")
        return;
    for (const auto& child : node.children) {
        if (child->type == "Assignment") {
            const ASTNode& lvalue = *child->children[0];
            if (lvalue.type == "IDENTIFIER") {
                std::string var = lvalue.value;
                if (!check_expr.declared_vars.contains(var))
                    throw CompilerException(std::format("Undeclared variable: {}", var));
                if (!check_expr.declared_vars.at(var).empty())
                    throw CompilerException(std::format("Variable {} is an array, cannot assign directly", var));
                check_expr(*child->children[1]);
            } else if (lvalue.type == "ArrayAccess") {
                std::string var = lvalue.value;
                if (!check_expr.declared_vars.contains(var))
                    throw CompilerException(std::format("Undeclared array: {}", var));
                const auto& dims = check_expr.declared_vars.at(var);
                if (dims.empty())
                    throw CompilerException(std::format("Variable {} is not an array", var));
                if (lvalue.children.size() != dims.size())
                    throw CompilerException(std::format("Dimension mismatch for array {}", var));
                for (const auto& idx : lvalue.children)
                    check_expr(*idx);
                check_expr(*child->children[1]);
            }
        } else if (child->type == "PrintStatement") {
            if (child->children[0]->type != "STRING_LITERAL")
                check_expr(*child->children[0]);
        } else if (child->type == "IfStatement" || child->type == "IfElseStatement") {
            check_expr(*child->children[0]);
            checkUsage(*child->children[1], check_expr);
            if (child->type == "IfElseStatement")
                checkUsage(*child->children[2], check_expr);
        } else if (child->type == "WhileStatement") {
            check_expr(*child->children[0]);
            checkUsage(*child->children[1], check_expr);
        }
    }
}

std::map<std::string, std::vector<int>, std::less<>> semantic_analyze(const ASTNode& ast) {
    std::cout << "Starting semantic analysis\n";
    std::map<std::string, std::vector<int>, std::less<>> declared_vars;
    collectDeclarations(ast, declared_vars);
    std::cout << "\nDeclarations collected\n";
    CheckExpr check_expr(declared_vars);
    checkUsage(ast, check_expr);
    return declared_vars;
}

struct GenExpr {
    std::vector<Instruction>& code;
    const std::function<std::string()>& gen_temp;
    const std::map<std::string, std::vector<int>, std::less<>>& declared_vars;
    GenExpr(std::vector<Instruction>& c, const std::function<std::string()>& gt, const std::map<std::string, std::vector<int>, std::less<>>& dv)
        : code(c), gen_temp(gt), declared_vars(dv) {}
    std::string operator()(const ASTNode& node) const {
        if (node.type == "NUMBER")
            return node.value;
        if (node.type == "IDENTIFIER")
            return node.value;
        if (node.type == "ArrayAccess") {
            std::string var = node.value;
            const auto& dims = declared_vars.at(var);
            if (dims.size() != node.children.size())
                throw CompilerException(std::format("Dimension mismatch for array {}", var));
            std::string index = (*this)(*node.children[0]);
            for (size_t k = 1; k < node.children.size(); k++) {
                std::string dim = std::to_string(dims[k]);
                std::string temp1 = gen_temp();
                code.emplace_back("*", index, dim, temp1);
                std::string ik = (*this)(*node.children[k]);
                std::string temp2 = gen_temp();
                code.emplace_back("+", temp1, ik, temp2);
                index = temp2;
            }
            std::string result = gen_temp();
            code.emplace_back("load_array", var, index, result);
            return result;
        }
        if (node.type == "BinaryExpression") {
            std::string left = (*this)(*node.children[0]);
            std::string right = (*this)(*node.children[1]);
            std::string temp = gen_temp();
            code.emplace_back(node.value, left, right, temp);
            return temp;
        }
        throw CompilerException(std::format("Unknown expression type: {}", node.type));
    }
};

void generateCode(const ASTNode& node, std::vector<Instruction>& code, int& temp_count, int& label_count, const std::map<std::string, std::vector<int>, std::less<>>& declared_vars) {
    if (node.type != "StatementList")
        return;
    auto gen_temp = [&temp_count]() { return "t" + std::to_string(temp_count++); };
    auto gen_label = [&label_count]() { return "L" + std::to_string(label_count++); };
    GenExpr gen_expr(code, gen_temp, declared_vars);
    for (const auto& child : node.children) {
        if (child->type == "Declaration" && child->children.size() > 1) {
            std::string result = gen_expr(*child->children[1]);
            code.emplace_back("=", result, "", child->children[0]->value);
        } else if (child->type == "Assignment") {
            const ASTNode& lvalue = *child->children[0];
            std::string value = gen_expr(*child->children[1]);
            if (lvalue.type == "IDENTIFIER")
                code.emplace_back("=", value, "", lvalue.value);
            else if (lvalue.type == "ArrayAccess") {
                std::string var = lvalue.value;
                const auto& dims = declared_vars.at(var);
                if (dims.size() != lvalue.children.size())
                    throw CompilerException(std::format("Dimension mismatch for array {}", var));
                std::string index = gen_expr(*lvalue.children[0]);
                for (size_t k = 1; k < lvalue.children.size(); k++) {
                    std::string dim = std::to_string(dims[k]);
                    std::string temp1 = gen_temp();
                    code.emplace_back("*", index, dim, temp1);
                    std::string ik = gen_expr(*lvalue.children[k]);
                    std::string temp2 = gen_temp();
                    code.emplace_back("+", temp1, ik, temp2);
                    index = temp2;
                }
                code.emplace_back("store_array", var, index, value);
            }
        } else if (child->type == "PrintStatement") {
            if (child->children[0]->type == "STRING_LITERAL")
                code.emplace_back("print_str", child->children[0]->value, "", "");
            else {
                std::string result = gen_expr(*child->children[0]);
                code.emplace_back("print", result, "", "");
            }
        } else if (child->type == "IfStatement") {
            std::string cond = gen_expr(*child->children[0]);
            std::string end_label = gen_label();
            code.emplace_back("ifz", cond, "", end_label);
            generateCode(*child->children[1], code, temp_count, label_count, declared_vars);
            code.emplace_back("label", "", "", end_label);
        } else if (child->type == "IfElseStatement") {
            std::string cond = gen_expr(*child->children[0]);
            std::string else_label = gen_label();
            std::string end_label = gen_label();
            code.emplace_back("ifz", cond, "", else_label);
            generateCode(*child->children[1], code, temp_count, label_count, declared_vars);
            code.emplace_back("goto", "", "", end_label);
            code.emplace_back("label", "", "", else_label);
            generateCode(*child->children[2], code, temp_count, label_count, declared_vars);
            code.emplace_back("label", "", "", end_label);
        } else if (child->type == "WhileStatement") {
            std::string start_label = gen_label();
            std::string end_label = gen_label();
            code.emplace_back("label", "", "", start_label);
            std::string cond = gen_expr(*child->children[0]);
            code.emplace_back("ifz", cond, "", end_label);
            generateCode(*child->children[1], code, temp_count, label_count, declared_vars);
            code.emplace_back("goto", "", "", start_label);
            code.emplace_back("label", "", "", end_label);
        }
    }
}

std::vector<Instruction> generate_intermediate_code(const ASTNode& ast, const std::map<std::string, std::vector<int>, std::less<>>& declared_vars) {
    std::cout << "Generating intermediate code\n";
    std::vector<Instruction> code;
    int temp_count = 0;
    int label_count = 0;
    generateCode(ast, code, temp_count, label_count, declared_vars);
    return code;
}

std::string read_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open the file " << filename << std::endl;
        return "";
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main() {
    try {
        std::string input = read_file("input.txt");
        std::cout << "Input read successfully\n\n";
        EarleyParser parser;
        auto ast = parser.parse_input(input);
        std::cout << "Parsing successful\n\n";
        auto vars = semantic_analyze(*ast);
        std::cout << "Semantic analysis successful\n\n";
        std::cout << "Declared vars: ";
        for (const auto& [var, dims] : vars) {
            std::cout << var;
            if (!dims.empty()) {
                std::cout << "[";
                for (size_t i = 0; i < dims.size(); i++) {
                    std::cout << dims[i];
                    if (i < dims.size() - 1)
                        std::cout << "][";
                }
                std::cout << "]";
            }
            std::cout << " ";
        }
        std::cout << "\n\n";
        auto code = generate_intermediate_code(*ast, vars);
        std::cout << "Code generation successful\n\n";
        for (const auto& instr : code)
            std::cout << instr.op << " " << instr.arg1 << " " << instr.arg2 << " " << instr.result << "\n";
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    } catch (...) {
        std::cerr << "Unknown error occurred\n";
        return 1;
    }
    return 0;
}
