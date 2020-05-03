"use strict";

const operations = {};

const Exception = function (name, Message) {
    const result = function (...args) {
        this.message = Message(...args);
        this.name = name;
    };
    result.prototype = new Error;
    return result;
};

Exception.prototype.toString = function() {
    return this.message.toString() + this.name;
}

const MissingOperationException = Exception(
    "MissingOperationException",
    index => "Missed operation at index " + (index + 1)
);

const InvalidAmountOfOperands = Exception(
    "InvalidAmountOfOperands",
    (symbol, index) => "Invalid amount of operands for operation " + symbol + " at index " + (index + 1)
);

const MissingCloseBracketException = Exception(
    "MissingCloseBracketException",
    index => "Missed close bracket at index " + (index + 1)
);

const UnexpectedSymbolException = Exception(
    "UnexpectedSymbolException",
    (index, symbol) => "Unexpected symbol : " + symbol + " at index " + (index + 1)
);

const UnexpectedOperationException = Exception(
    "UnexpectedOperationException",
    (index, token) => "Unexpected operation : " + token + " at index " + (index + 1)
);

const UnexpectedOperandException = Exception(
    "UnexpectedOperandException",
    (index, token) => "Unexpected operand : " + token + " at index " + (index + 1)
)

function AbstractOperation(...operands) {
    this.operands = operands;
}

AbstractOperation.prototype.evaluate = function (...values) {
    let oper = this.operands.map(op => op.evaluate(...values));
    return this.operation(...oper);
};

AbstractOperation.prototype.toString = function () {
    return this.operands.map(op => op.toString()).join(" ") + " " + this.tag;
};

AbstractOperation.prototype.diff = function (variable) {
    return this.derivative(variable, ...this.operands);
};

AbstractOperation.prototype.prefix = function () {
    return "(" + this.tag + " " + this.operands.map(operand => operand.prefix()).join(" ") + ")";
};

AbstractOperation.prototype.postfix = function () {
    return "(" + this.operands.map(operand => operand.postfix()).join(" ") + " " + this.tag + ")";
};

function createOperation(operation, derivative, operationSign) {
    let Operation = function (...operands) {
        AbstractOperation.call(this, ...operands);
    }

    Operation.prototype = Object.create(AbstractOperation.prototype);
    Operation.prototype.operation = operation;
    Operation.prototype.derivative = derivative;
    Operation.prototype.tag = operationSign;
    Operation.arity = operation.length;

    operations[operationSign] = Operation;
    return Operation;
}

function Const(value) {
    this.value = value;
}

Const.prototype.diff = () => Const.ZERO;
Const.prototype.evaluate = function () {
    return this.value;
}
Const.prototype.toString = function () {
    return this.value.toString();
}

Const.prototype.prefix = Const.prototype.toString;
Const.prototype.postfix = Const.prototype.toString;

Const.E = new Const(Math.E);
Const.ZERO = new Const(0);
Const.ONE = new Const(1);

const vars = ["x", "y", "z"];

function Variable(name) {
    this.name = name;
    this.index = vars.indexOf(name);
}

Variable.prototype.evaluate = function (...values) {
    return values[this.index]
};
Variable.prototype.diff = function (variable) {
    return variable === this.name ? Const.ONE : Const.ZERO
};
Variable.prototype.toString = function () {
    return this.name;
};

Variable.prototype.prefix = Variable.prototype.toString;
Variable.prototype.postfix = Variable.prototype.toString;

const Negate = createOperation(
    (value) => -value,
    (variable, expr) => new Negate(expr.diff(variable)),
    "negate"
);

const Add = createOperation(
    (x, y) => x + y,
    (variable, x, y) => new Add(x.diff(variable), y.diff(variable)),
    "+"
);

const Subtract = createOperation(
    (x, y) => x - y,
    (variable, x, y) => new Subtract(x.diff(variable), y.diff(variable)),
    "-"
);

const Multiply = createOperation(
    (x, y) => x * y,
    (variable, x, y) => new Add(new Multiply(x.diff(variable), y), new Multiply(x, y.diff(variable))),
    "*"
);

const Divide = createOperation(
    (x, y) => x / y,
    (variable, x, y) => new Divide(new Subtract(
        new Multiply(x.diff(variable), y), new Multiply(x, y.diff(variable))),
        new Multiply(y, y)
    ),
    "/"
);

const Log = createOperation(
    (x, y) => (Math.log(Math.abs(y)) / Math.log(Math.abs(x))),
    (variable, x, y) => new Divide(
        new Subtract(
            new Multiply(new Multiply(x, new Log(Const.E, x)), y.diff(variable)),
            new Multiply(y, new Multiply(x.diff(variable), new Log(Const.E, y)))
        ),
        new Multiply(
            x,
            new Multiply(y, new Multiply(new Log(Const.E, x), new Log(Const.E, x)))
        )
    ),
    "log"
);

const Power = createOperation(
    (x, y) => Math.pow(x, y),
    (variable, x, y) => new Multiply(
        new Power(x, new Subtract(y, Const.ONE)),
        new Add(
            new Multiply(y, x.diff(variable)),
            new Multiply(x, new Multiply(new Log(Const.E, x), y.diff(variable))))),
    "pow"
);

const expDerivative = (variable, ...opers) => {
    if (opers.length === 0) {
        return Const.ZERO;
    }

    return opers.reduce((acum, oper) => new Add(acum, new Power(Const.E, oper)), Const.ZERO);
};

const Sumexp = createOperation(
    (...opers) => opers.reduce((acum, operand) => acum + Math.pow(Math.E, operand), 0),
    (variable, ...opers) => expDerivative(variable, ...opers).diff(variable),
    "sumexp"
);

const Softmax = createOperation(
    (...opers) => opers.length === 0 ? 1 : Math.pow(Math.E, opers[0]) /
        Sumexp.prototype.operation(...opers),
    (variable, ...opers) => {
        return new Divide(new Power(Const.E, opers[0]), expDerivative(variable, ...opers)).diff(variable);
    },
    "softmax"
);

const whiteSpaces = [' ', '\t', '\n', '\r', '\v', '\f', '\uFEFF', '\u00A0'];

const isVar = (str) => vars.includes(str);
const isDigit = (character) => character >= '0' && character <= '9';
const isNumber = (str) => {
    let i = (str[0] === '-' ? 1 : 0);
    for (; i < str.length; i++) {
        if (!isDigit(str[i])) {
            return false;
        }
    }
    return true;
}

const isWhiteSpace = (symbol) => whiteSpaces.includes(symbol);

function Source (expression, index) {
    this.expression = expression;
    this.index = index;
    this.getChar = function() {
        return expression[this.index]
    };
    this.getIndex = function() {
        return this.index
    };
}

function PrefParserSource (expression) {
    Source.call(this, expression, 0);
}

PrefParserSource.prototype.substring = function(begin, end) {
    return this.expression.substring(begin + 1, end + 1)
};

PrefParserSource.prototype.next = function() {
    return this.index++
};

PrefParserSource.prototype.hasNext = function() {
    return this.index < this.expression.length
};

function PostParserSource(expression) {
    Source.call(this, expression, expression.length - 1);
}

PostParserSource.prototype.substring = function(begin, end) {
    return this.expression.substring(begin + 1, end + 1)
};

PostParserSource.prototype.next = function() {
    return this.index--
};

PostParserSource.prototype.hasNext = function() {
    return this.index >= 0
};

function parsePrefix(expression) {
    return AbstractParser('(', ')', new PrefParserSource(expression), "pref");
}

function parsePostfix(expression) {
    return AbstractParser(')', '(', new PostParserSource(expression), "post");
}

function AbstractParser(startChar, endChar, source, arrayOper) {
    let skipWhitespace = () => {
        while (isWhiteSpace(source.getChar())) {
            source.next();
        }
    };

    let checkChar = function(expected) {
        if (expected === source.getChar()) {
            source.next();
            return true;
        }
        return false;
    }

    let next = () => {
        skipWhitespace();
        let startIndex = source.getIndex();
        do {
            source.next()
        } while (source.hasNext() && !isWhiteSpace(source.getChar())
        && source.getChar() !== startChar && source.getChar() !== endChar);

        let result = source.substring(startIndex, source.getIndex());
        skipWhitespace();
        if (result.length === 0) {
            throw new MissingOperationException(source.getIndex()).toString();
        }
        return result;
    }

    let parseArgument = (arg) => {
        if (isNumber(arg)) {
            return new Const(parseFloat(arg));
        }
        if (isVar(arg)) {
            return new Variable(arg);
        }
        throw new UnexpectedOperandException(source.getIndex(), arg).toString();
    }

    let parseOperation = () => {
        skipWhitespace();

        if (!source.hasNext()) {
            throw new MissingOperationException(source.getIndex()).toString();
        }

        if (!checkChar(startChar)) {
            return parseArgument(next());
        }
        let token = next();
        let operation = operations[token];

        if (!operation) {
            throw new UnexpectedOperationException(source.getIndex(), token).toString();
        }

        let exprArgs = [];

        while (source.hasNext()) {
            if (checkChar(endChar)) {
                skipWhitespace();
                if (exprArgs.length === operation.arity || operation.arity === 0) {
                    if (exprArgs.length === 0) {
                        return new operation();
                    }
                    return new operation(...exprArgs);
                }
                throw new InvalidAmountOfOperands(token, source.getIndex()).toString();
            }
            if (arrayOper === "pref") {
                exprArgs.push(parseOperation());
            } else {
                exprArgs.unshift(parseOperation());
            }
        }
        throw new MissingCloseBracketException(source.getIndex()).toString();
    }

    let result = parseOperation();

    skipWhitespace();

    if (source.hasNext()) {
        throw new UnexpectedSymbolException(source.getIndex(), source.next()).toString();
    }

    return result;
}

const splitByWhiteSpaces = (str) => {
    let result = [];
    for (let i = 0; i < str.length; i++) {
        if (!isWhiteSpace(str[i])) {
            let startPosition = i;
            while (i < str.length && !isWhiteSpace(str[i])) {
                i++;
            }
            result.push(str.substring(startPosition, i));
        }
    }
    return result;
};

const parse = expression => {

    let tokens = splitByWhiteSpaces(expression);
    let exprStack = [];

    tokens.map((token) => {
        if (token in operations) {
            let operation = operations[token];
            exprStack.push(new operation(...exprStack.splice(-operation.arity)));
        } else if (vars.includes(token)) {
            exprStack.push(new Variable(token));
        } else {
            exprStack.push(new Const(parseInt(token)));
        }
    });

    return exprStack[0];
};
