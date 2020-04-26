"use strict";

const operations = {};

function AbstractOperation(...operands) {
    this.operands = operands;
}

AbstractOperation.prototype.evaluate = function (...values) {
    let oper = this.operands.map(op => op.evaluate(...values));
    return this.operation(...oper);
};

AbstractOperation.prototype.toString = function () {
    return this.operands.map(op => op.toString()).join(" ") + " " + this.operationSign;
};

AbstractOperation.prototype.diff = function (variable) {
    return this.derivative(variable, ...this.operands);
};

function createOperation(operation, derivative, operationSign) {
    let Operation = function (...operands) {
        AbstractOperation.call(this, ...operands);
    }

    Operation.prototype = Object.create(AbstractOperation.prototype);
    Operation.prototype.operation = operation;
    Operation.prototype.derivative = derivative;
    Operation.prototype.operationSign = operationSign;
    Operation.arity = operation.length;

    operations[operationSign] = Operation;
    return Operation;
}

function Const(value) {
    this.value = value;
}
Const.prototype.diff = () => Const.ZERO;
Const.prototype.evaluate = function() {return this.value;}
Const.prototype.toString = function () {return this.value.toString();}

Const.E = new Const(Math.E);
Const.ZERO = new Const(0);
Const.ONE = new Const(1);

const vars = ["x", "y", "z"];

function Variable(name) {
    this.name = name;
    this.index = vars.indexOf(name);
}

Variable.prototype.evaluate = function (...values) {return values[this.index]};
Variable.prototype.diff = function (variable) {return variable === this.name ? Const.ONE : Const.ZERO};
Variable.prototype.toString = function () {return this.name;};

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

const whiteSpaces = [' ', '\t', '\n', '\r', '\v', '\f', '\uFEFF', '\u00A0'];

const isWhiteSpace = (symbol) => whiteSpaces.includes(symbol);

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
