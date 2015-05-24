 
// Copyright (c) 2015 actorbug
// Released under the MIT license
// http://opensource.org/licenses/mit-license.php
 
var SBParser = {
	parse: function(text, msgs) {
		var buf = new SBParser.Scanner(SBParser.zen2han(text))
		var stmts = new Array();
		while (buf.value !== SBParser.token['EOF']) {
			// Process until error found
			try {
				SBParser.parseStmts(buf, stmts);
				// Handle remaining errors
				if (buf.value !== SBParser.token['EOF']) {
					// High possibility that error previously on same line
					if (stmts.length > 0 && buf.pos.row == stmts[stmts.length-1].pos.row)
						stmts.pop();
					SBParser.errorStmt(buf);
				}
			}
			catch (e) {
				if (e instanceof SBParser.Error)
					msgs.push(e);
				else
					throw e;
			}
			// Skip to what is believed to be the end of the statement
			while (!buf.next().isEnd)
				;
		}
		return stmts;
	},
 
	parseStmts: function(buf, stmts, oneLine, withoutEnd) {
		while (buf.check(SBParser.token[':']) || !oneLine && buf.check(SBParser.token['EOL']))
			;
		// Read instructions for possibility of finding an instruction
		// Used to be determined when reaching the end of a line
		while (buf.value.command && !(withoutEnd && buf.value.name == 'END') || buf.value instanceof SBParser.Word || buf.value instanceof SBParser.Label) {
			stmts.push(SBParser.parseStmt(buf));
			while (buf.check(SBParser.token[':']) || !oneLine && buf.check(SBParser.token['EOL']))
				;
		}
	},
 
	parseStmt: function(buf) {
		if (buf.value instanceof SBParser.Label) {
			var pos = buf.pos;
			var label = SBParser.parseLabel(buf, pos, buf.next());
			return { pos: label.pos, name: '@', exprs: [label] };
		}
		var pos = buf.pos;
		var value = buf.next();
		// Process as assignment if an assignment
		var parser = value.command;
		if (/^[=\[]$/.test(buf.value.name) || value.name == 'VAR' && buf.value.name == '(')
			return SBParser.parseStmtLet(buf, pos, value);
		else if (!parser)
			return SBParser.parseStmtCommand(buf, pos, value.name);
		else
			return parser(buf, pos, value.name);
	},
 
	parseStmtLet: function(buf, pos, value) {
		var v = SBParser.parseArray(buf, pos, value);
		var pos2 = buf.pos;
		// "=" is processed separately because it is likely to be wrong
		if (buf.value !== SBParser.token['=']) {
			// Throw errors found here
			if (buf.value instanceof SBParser.Error)
				throw buf.value;
			SBParser.errorIfEqual(buf);
			SBParser.errorIfParen(buf);
			throw new SBParser.Error(pos, value.name + ' Command does not exist.');
		}
		buf.next();
		return { pos: pos2, name: '=', exprs: [ SBParser.parseExpr(buf) ], vars: [ v ] };
	},
 
	parseStmtCommand: function(buf, pos, name) {
		var exprs = SBParser.parseExprs(buf);
		if (buf.value != SBParser.token['OUT'])
			return { pos: pos, name: name, exprs: exprs };
		buf.next();
		return { pos: pos, name: name, exprs: exprs, vars: SBParser.parseExprs(buf) };
	},
 
	// "Break A=0 is a unique errorduring processing
	parseStmtNoArgCommand: function(buf, pos, name) {
		return { pos: pos, name: name, exprs: [] };
	},
 
	parseStmtVarArgs: function(buf, pos, name) {
		return { pos: pos, name: name, vars: SBParser.parseExprs(buf) };
	},
 
	parseStmtPrint: function(buf, pos, name) {
		var exprs = new Array();
		// Reads expression token "," and ";"
		// Used to be determined at the end of a line
		for (;;) {
			if (typeof(buf.value) == 'number' || typeof(buf.value) == 'string' || buf.value.isExprHead)
				exprs.push(SBParser.parseExpr(buf));
			else if (buf.value === SBParser.token[';'] || buf.value === SBParser.token[',']) {
				exprs.push({ pos: buf.pos, type: buf.value.name });
				buf.next();
			}
			else
				break;
		}
		return { pos: pos, name: 'PRINT', exprs: exprs };
	},
 
	parseStmtInput: function(buf, pos, name) {
		var exprs = new Array();
		// If begins with a string literal, description of the input.
		if (typeof(buf.value) == 'string') {
			exprs.push(SBParser.parseExpr(buf));
			SBParser.checkStmtToken(buf, [';'], 'The command ' + name + ' contains errors in the input.', 'Command ' + name + ' to ";" you do not have.');
			buf.next();
		}
		return { pos: pos, name: name, exprs: exprs, vars: SBParser.parseExprs(buf) };
	},
 
	parseStmtIf: function(buf, pos, name) {
		var exprs = [ SBParser.parseExpr(buf) ];
		SBParser.checkStmtToken(buf, ['THEN','GOTO'], 'There is an error in the IF statement.', 'Corresponding THEN missing for IF statement.');
		var stmts = new Array();
		var oneLine = true;
		if (buf.value == SBParser.token['THEN']) {
			buf.next();
			if (buf.value === SBParser.token['EOL'])
				oneLine = false;
			// Treatment for when there is a GOTO label
			if (buf.value instanceof SBParser.Label)
				stmts.push(SBParser.parseStmtCommand(buf, buf.pos, 'GOTO'));
		}
		SBParser.parseStmts(buf, stmts, oneLine);
		if (buf.value !== SBParser.token['ELSE']) {
			if (buf.value === SBParser.token['ENDIF']) {
				buf.next();
				return { pos: pos, name: name, exprs: exprs, stmts: stmts };
			}
			else if (!oneLine)
				throw new SBParser.Error(buf.pos, 'Corresponding ENDIF missing for IF statement.');
			else
				return { pos: pos, name: name, exprs: exprs, stmts: stmts };
		}
		buf.next();
		var stmts2 = new Array();
		if (buf.value === SBParser.token['EOL'])
			oneLine = false;
		// Treatment for when there is a GOTO label
		if (buf.value instanceof SBParser.Label)
			stmts2.push(SBParser.parseStmtCommand(buf, buf.pos, 'GOTO'));
		SBParser.parseStmts(buf, stmts2, oneLine);
		if (buf.value === SBParser.token['ENDIF']) {
			buf.next();
			return { pos: pos, name: name, exprs: exprs, stmts: stmts, stmts2: stmts2 };
		}
		else if (!oneLine)
			throw new SBParser.Error(buf.pos, 'Corresponding ENDIF missing for IF statement.');
		else
			return { pos: pos, name: name, exprs: exprs, stmts: stmts, stmts2: stmts2 };
	},
 
	parseStmtFor: function(buf, pos, name) {
		var pos2 = buf.pos;
		if (!(buf.value instanceof SBParser.Word))
			throw new SBParser.Error(buf.pos, 'There is an error in the variable in the FOR instruction.');
		var vars = [SBParser.parseArray(buf, pos2, buf.next())];
		var exprs = [];
		SBParser.errorIfEqual(buf);
		SBParser.checkStmtToken(buf, ['='], 'There is an error in the variable in the FOR instruction.', 'The "=" operator is missing in the FOR instruction.');
		buf.next();
		exprs.push(SBParser.parseExpr(buf));
		SBParser.checkStmtToken(buf, ['TO'], 'There is an error in the initial value of the FOR instruction', 'TO is missing in the FOR instruction');
		buf.next();
		exprs.push(SBParser.parseExpr(buf));
		if (buf.value.name == 'STEP') {
			buf.next();
			exprs.push(SBParser.parseExpr(buf));
		}
		var stmts = new Array();
		SBParser.parseStmts( buf, stmts );
		if (buf.value !== SBParser.token['NEXT'])
			throw new SBParser.Error(buf.pos, 'Corresponding NEXT is missing for FOR instruction');
		buf.next();
		if (buf.value instanceof SBParser.Word)
		{
			pos2 = buf.pos;
			vars.push(SBParser.parseArray(buf, pos2, buf.next()));
		}
		return { pos: pos, name: name, exprs: exprs, vars: vars, stmts: stmts };
	},
 
	parseStmtData: function(buf, pos, name) {
		var exprs = SBParser.parseExprs(buf);
		if (exprs.length <= 0)
			throw new SBParser.Error(buf.pos, 'The contents in DATA are incorrect.');
		for (var i = 0; i < exprs.length; ++i) {
			if (!/^(?:number|string)$/.test(exprs[i].type))
				throw new SBParser.Error(exprs[i].pos, 'The contents in DATA are incorrect.');
		}
		return { pos: pos, name: name, exprs: exprs };
	},
 
	parseStmtOn: function(buf, pos, name) {
		var exprs = [ SBParser.parseExpr(buf) ];
		SBParser.checkStmtToken(buf, ['GOTO','GOSUB'], 'There is a problem with the value given in ON', 'There is no GOTO or GOSUB for the ON instruction');
		var value = buf.next();
		// Because it is possible to omit the last label, adjust it to make sense with a cheap trick.
		do {
			if (buf.value instanceof SBParser.Label) {
				var pos2 = buf.pos;
				exprs.push(SBParser.parseLabel(buf, pos2, buf.next()));
			}
			else
				exprs.push({ pos: buf.pos, type: 'null' });
		} while (buf.check(SBParser.token[',']));
		return { pos: pos, name: name + value.name, exprs: exprs };
	},
 
	parseStmtWhile: function(buf, pos, name) {
		var expr = SBParser.parseExpr(buf);
		var stmts = new Array();
		SBParser.parseStmts( buf, stmts );
		if (buf.value !== SBParser.token['WEND'])
			throw new SBParser.Error(buf.pos, 'Corresponding WEND is missing for the WHILE instruction.');
		buf.next();
		return { pos: pos, name: name, exprs: [expr], stmts: stmts };
	},
 
	parseStmtRepeat: function(buf, pos, name) {
		var stmts = new Array();
		SBParser.parseStmts( buf, stmts );
		if (buf.value !== SBParser.token['UNTIL'])
			throw new SBParser.Error(buf.pos, 'Corresponding UNTIL is missing for the REPEAT instruction.');
		buf.next();
		var expr = SBParser.parseExpr(buf);
		return { pos: pos, name: name, exprs: [expr], stmts: stmts };
	},
 
	parseStmtDef: function(buf, pos, name) {
		if (name == 'COMMON') {
			if (buf.value !== SBParser.token['DEF'])
				throw new SBParser.Error(buf.pos, 'Corresponding DEF is missing for the COMMON instruction.');
			buf.next();
			name = 'COMMONDEF';
		}
		if (!(buf.value instanceof SBParser.Word))
			if (buf.value.name)
				throw new SBParser.Error(buf.pos, buf.value.name + ' You can not place a function name this way.');
			else
				throw new SBParser.Error(buf.pos, 'The function definition is incorrect.');
		var name2 = buf.value.name;
		buf.next();
		var isfunc = (buf.value === SBParser.token['(']);
		var exprs;
		var vars = [];
		if (isfunc) {
			buf.next();
			exprs = SBParser.parseExprs(buf);
			SBParser.checkExprToken(buf, [')'], 'The expression is incorrect.', 'Brackets are not closed.');
			buf.next();
		}
		else {
			exprs = SBParser.parseExprs(buf);
			if (buf.value === SBParser.token['OUT']) {
				buf.next();
				vars = SBParser.parseExprs(buf);
			}
		}
		var stmts = new Array();
		SBParser.parseStmts( buf, stmts, false, true );
		if (buf.value !== SBParser.token['END'])
			throw new SBParser.Error(buf.pos, 'Corresponding END is missing for the DEF instruction.');
		buf.next();
		return { pos: pos, name: name, name2: name2, exprs: exprs, vars: vars, stmts: stmts, isfunc: isfunc };
	},
 
	parseStmtDim: function(buf, pos, name) {
		var exprs = [];
		var vars = [];
		do {
			var pos2 = buf.pos;
			vars.push(SBParser.parseArray(buf, pos2, buf.next()));
			if (buf.value === SBParser.token['=']) {
				buf.next();
				exprs.push(SBParser.parseExpr(buf));
			}
			else
				exprs.push({ pos: buf.pos, type: 'null' });
		} while (buf.check(SBParser.token[',']));
		return { pos: pos, name: name, exprs: exprs, vars: vars };
	},
 
	parseExprs: function(buf) {
		var exprs = new Array();
		// Return length 0 if beginning token is ","  (?)
		// Previously determined upon reaching the end of the line.
		if (!(typeof(buf.value) == 'number' || typeof(buf.value) == 'string' || buf.value.isExprHead || buf.value === SBParser.token[',']))
			return exprs;
		// Read, create empty expression if there is no formula.
	    // Because the last comma-separated expression is not omitted, judge by the comma (?) (カンマ区切りの最後の式が省略不可なので、カンマの判定のみで十分)
		do {
			exprs.push( (buf.value === SBParser.token[',']) ? { pos: buf.pos, type: 'null' } : SBParser.parseExpr(buf) );
		} while (buf.check(SBParser.token[',']));
		return exprs;
	},
 
	parseExpr: function(buf, pri) {
		// Recursive priority
		pri = pri || 1;
		if (pri > 6)
			return SBParser.parseUnary(buf);
		var expr = SBParser.parseExpr(buf, pri + 1);
		for (;;) {
			var pos = buf.pos;
			var value = buf.value;
			if (value.binPri != pri) break;
			buf.next();
			expr = { pos: pos, type: 'function', name: value.name, args: [ expr, SBParser.parseExpr(buf, pri + 1) ] };
		}
		// Error if the expression contains "="
		// Use parseVariable() in the case that the assignment can be determined safely
		if (pri == 1 && buf.value === SBParser.token['='])
			throw new SBParser.Error(buf.pos, 'Use "==" instead of "=" to compare values.');
		return expr;
	},
 
	parseUnary: function(buf) {
		var pos = buf.pos;
		if (buf.value === SBParser.token['-']) {
			buf.next();
			var arg = SBParser.parseUnary(buf);
			if (arg.type == 'number' && arg.val >= 0)
				return { pos: pos, type: 'number', val: - arg.val };
			else
				return { pos: pos, type: 'function', name: '-', args: [ arg ] };
		}
		if (buf.value === SBParser.token['!'] || buf.value === SBParser.token['NOT']) {
			var name = buf.next().name;
			return { pos: pos, type: 'function', name: name, args: [ SBParser.parseUnary(buf) ] };
		}
		return SBParser.parseArray(buf, pos, buf.next());
	},
 
	parseArray: function(buf, pos, value) {
		var expr = SBParser.parseFactor(buf, pos, value);
		while (buf.value === SBParser.token['[']) {
			var pos2 = buf.pos;
			buf.next();
			var args = SBParser.parseExprs(buf);
			SBParser.checkExprToken(buf, [']'], 'Array subscript is incorrect.', 'Array brackets are not closed.');
			buf.next();
			args.unshift(expr);
			expr = { pos: pos2, type: 'function', name: '[]', args: args };
		}
		return expr;
	},
 
	parseFactor: function(buf, pos, value) {
		if (typeof(value) == 'number')
			return { pos: pos, type: 'number', val: value };
		if (typeof(value) == 'string')
			return { pos: pos, type: 'string', val: value };
		if (value instanceof SBParser.Word || /^(?:CALL|VAR|TRUE|FALSE)$/.test(value.name))
			return SBParser.parseFuncOrVar(buf, pos, value);
		if (value instanceof SBParser.Label)
			return SBParser.parseLabel(buf, pos, value);
		if (value === SBParser.token['(']) {
			var expr = SBParser.parseExpr(buf);
			SBParser.checkExprToken(buf, [')'], 'Incorrect expression.', 'Brackets are not closed.');
			buf.next();
			return expr;
		}
		SBParser.errorExpr(buf, pos, value);
	},
 
	parseFuncOrVar: function(buf, pos, value) {
		var name = value.name;
		if (!(value instanceof SBParser.Word || /^(?:TRUE|FALSE|VAR|CALL)$/.test(name))) {
			// Might have arrived here using a variable without checking this error
			if (value instanceof SBParser.Keyword)
				throw new SBParser.Error(pos, name + ' Reserved words can not be used as' + ((buf.value !== SBParser.token['('] ) ? 'variable' : 'function') + 'names.');
			else
				SBParser.errorExpr(buf, pos, value);
		}
		if (buf.value !== SBParser.token['(']) {
			if (/^(?:VAR|CALL)$/.test(name))
				throw new SBParser.Error(pos, name + ' Reserved words can not be used as variable names.');
			return { pos: pos, type: 'variable', name: name };
		}
		buf.next();
		var args = SBParser.parseExprs(buf);
		SBParser.checkExprToken(buf, [')'], 'The function ' + name + ' contains an error in the argument.', 'The function ' + name + ' is missing a closing parenthese.');
		buf.next();
		return { pos: pos, type: 'function', name: name, args: args };
	},
 
	parseLabel: function(buf, pos, value) {
		return { pos: pos, type: 'label', name: value.name };
	},
 
	// Errors in the order of instructions
	errorStmt: function(buf) {
		SBParser.errorIfParen(buf);
		var pos = buf.pos;
		var value = buf.next();
		// Throw errors set by parser here
		if (value instanceof SBParser.Error)
			throw value;
		SBParser.errorIfKeywordVariable(buf, pos, value);
		if (value instanceof SBParser.Keyword || value instanceof SBParser.Word)
			throw new SBParser.Error(pos, value.name + ' Not a valid command.');
		else
			throw new SBParser.Error(pos, 'Not a valid command');
	},
 
	// Errors in the order of expressions
	errorExpr: function(buf, pos, value) {
		if (value === SBParser.token['['])
			throw new SBParser.Error(pos, 'Not a valid expression.');
		// Throw errors set by parser here
		if (value instanceof SBParser.Error)
			throw value;
		SBParser.errorIfKeywordVariable(buf, pos, value);
		throw new SBParser.Error(pos, 'An expression is required.');
	},
 
	// Determine instruction (by token?)
	checkStmtToken: function(buf, tokens, err1, err2) {
		if (SBParser.contains(tokens, buf.value.name))
			return;
		// Throw errors set by the scanner here
		if (buf.value instanceof SBParser.Error)
			throw buf.value;
		SBParser.errorIfParen(buf);
		if (!err2)
			throw new SBParser.Error(buf.pos, err1);
		var pos = buf.pos;
		// Load/read errors when finished reading instruction(?)
		while (!buf.value.isEnd) {
			if (SBParser.contains(tokens, buf.value.name))
				throw new SBParser.Error(pos, err1);
			buf.next();
		}
		// If it does not find a token, error that there is no token.
		throw new SBParser.Error(pos, err2);
	},
 
	// Determine expression in token
	checkExprToken: function(buf, tokens, err1, err2) {
		if (SBParser.contains(tokens, buf.value.name))
			return;
		// Throw error set in parser here
		if (buf.value instanceof SBParser.Error)
			throw buf.value;
		if (!err2)
			throw new SBParser.Error(buf.pos, err1);
		var pos = buf.pos;
	    // Load/read errors when finished reading instruction(?)
		while (!buf.value.isEnd && !(buf.value instanceof SBParser.Keyword && !buf.value.isExprHead) &&
			   buf.value != SBParser.token[';'] && buf.value != SBParser.token['=']) {
			if (SBParser.contains(tokens, buf.value.name))
				throw new SBParser.Error(pos, err1);
			buf.next();
		}
	    // If it does not find a token, error that there is no token.
		throw new SBParser.Error(pos, err2);
	},
 
	errorIfParen: function(buf) {
		if (buf.value === SBParser.token[')'] || buf.value === SBParser.token[']'])
			throw new SBParser.Error(buf.pos, 'Too many closing parethesis.');
	},
 
	errorIfEqual: function(buf) {
		if (buf.value === SBParser.token['=='])
			throw new SBParser.Error(buf.pos, 'Use "=" rather than "=="');
	},
 
	errorIfKeywordVariable: function(buf, pos, value) {
		if (value instanceof SBParser.Keyword && buf.value instanceof SBParser.Symbol && buf.value !== SBParser.token[':'] && buf.value !== SBParser.token['!'])
		    throw new SBParser.Error(pos, value.name + ' Reserved words can not be used as variable names.');
	},
 
	contains: function(ary, elem) {
		for (var i = 0; i < ary.length; ++i)
			if (ary[i] == elem)
				return true;
		return false;
	},
 
	Scanner: (function() {
		function Scanner(text) {
			this.buf = new SBParser.Buffer(text);
			this.skipSpace();
			this.next();
		}
		// Since it is read-ahead, next() is not issued except at runtime
		// Instead, set the exception object to the value
		Scanner.prototype.next = function() {
			var value = this.value;
			this.pos = this.buf.pos;
			if (this.buf.isEOF()) {
				this.value = SBParser.token['EOF'];
			}
			else if (this.buf.peek(/^$/)) {
				this.value = SBParser.token['EOL'];
				this.buf.nextLine();
			}
			else {
				var match = this.buf.match(/^(?:((?!\d)\w+[#\$%]?|[-+*\/,:;\(\)\[\]\?]|!=?)|([^-+*\/.,:;\(\)\[\]\?!@"&\w\s]+)|(\d+(?:\.\d*)?|\.\d*)|(&(?:&|H[0-9A-F]+|B[01]+)?)|("[^"]*)"?|@\w*)/i);
				if (match[1]) {
					var name = match[1].toUpperCase();
					this.value = SBParser.token[name] || new SBParser.Word(name);
				}
				else if (match[2]) {
					// "=<" is /special/
					if (!(/^(?:==?|>=?|<=?|<<|>>|\|\|)$/.test(match[2])))
						this.value = new SBParser.Error(this.pos, match[2] + ' You can not use that symbol.');
					else
						this.value = SBParser.token[match[2]];
				}
				else if (match[3]) {
					if (match[3] == '.')
						this.value = new SBParser.Error(this.pos, 'Invalid number');
					else
						this.value = parseFloat(match[3]);
				}
				else if (match[4]) {
					if (match[4] == '&&')
						this.value = SBParser.token[match[4]];
					else if (match[4].length < 2)
						this.value = new SBParser.Error(this.pos, 'Number is incorrect syntax.');
					else if ((/^&H/i).test(match[4]))
						this.value = parseInt(match[4].substr(2), 16);
					else
						this.value = parseInt(match[4].substr(2), 2);
				}
				else if (match[5])
					this.value = match[5].substr(1);
				else {
					if (match[0].length < 2)
						this.value = new SBParser.Error(this.pos, 'There is an error in the label name');
					else
						this.value = new SBParser.Label(match[0].toUpperCase());
				}
			}
			this.skipSpace();
			return value;
		};
		Scanner.prototype.check = function(value) {
			if (this.value !== value)
				return false;
			this.next();
			return true;
		};
		Scanner.prototype.skipSpace = function() {
			this.buf.skip(/^\s*(?:'.*|REM\b.*)?/i);
		};
		return Scanner;
	})(),
 
	Buffer: (function() {
		function Buffer(text) {
			// For speed, split reverse order for each line
			this.lines = text.split('\n');
			this.lines.reverse();
			this.curLen = 0;
			this.pos = new SBParser.Position(0, 0, 0);
			this.nextLine();
		}
		Buffer.prototype.isEOF = function() {
			return this.lines.length <= 0 && this.cur.length <= 0;
		};
		Buffer.prototype.peek = function(regex) {
			return regex.test(this.cur);
		};
		Buffer.prototype.skip = function(regex) {
			var match = regex.exec(this.cur);
			if (match && match[0]) {
				var len = match[0].length;
				this.cur = this.cur.substr(len)
				this.pos = new SBParser.Position(this.pos.cnt, this.pos.row, this.pos.col + len)
			}
		};
		Buffer.prototype.match = function(regex) {
			var match = regex.exec(this.cur);
			if (match) {
				var len = match[0].length;
				this.cur = this.cur.substr(len)
				this.pos = new SBParser.Position(this.pos.cnt, this.pos.row, this.pos.col + len)
			}
			return match;
		};
		Buffer.prototype.nextLine = function() {
			if (this.lines.length <= 0) {
				this.pos = new SBParser.Position(this.pos.cnt, this.pos.row, this.pos.col + this.cur.length);
				this.cur = '';
			}
			else {
				this.pos = new SBParser.Position(this.pos.cnt + this.curLen, this.pos.row + 1, 1);
				this.cur = this.lines.pop();
				this.curLen = this.cur.length + 1;
				this.cur = this.cur.replace(/\r$/, '');
			}
		};
		return Buffer;
	})(),
 
	Error: (function() {
		function Error(pos, msg) {
			this.pos = pos;
			this.msg = msg;
		}
		Error.prototype.toString = function() {
			return this.pos + ' Error: ' + this.msg;
		};
		return Error;
	})(),
 
	Warning: (function() {
		function Warning(pos, msg) {
			this.pos = pos;
			this.msg = msg;
		}
		Warning.prototype.toString = function() {
			return this.pos + ' Warning: ' + this.msg;
		};
		return Warning;
	})(),
 
	Position: (function() {
		function Position(cnt, row, col) {
			this.cnt = cnt;
			this.row = row;
			this.col = col;
		}
		Position.prototype.toString = function() {
			return '(' + this.row + ',' + this.col + ')';
		};
		return Position;
	})(),
 
	compareMessage: function(msg1, msg2) {
		if (msg1 instanceof SBParser.Error && msg2 instanceof SBParser.Warning) return -1;
		if (msg1 instanceof SBParser.Warning && msg2 instanceof SBParser.Error) return 1;
		var cmp = SBParser.comparePosition(msg1.pos, msg2.pos);
		if (cmp != 0) return cmp;
		if (msg1.msg < msg2.msg) return -1;
		if (msg1.msg > msg2.msg) return 1;
		return 0;
	},
 
	comparePosition: function(pos1, pos2) {
		if (pos1.row < pos2.row) return -1;
		if (pos1.row > pos2.row) return 1;
		if (pos1.col < pos2.col) return -1;
		if (pos1.col > pos2.col) return 1;
		return 0;
	},
 
	zen2han: (function() {
		var zenkaku = [
			'０','１','２','３','４','５','６','７','８','９',
			'ａ','ｂ','ｃ','ｄ','ｅ','ｆ','ｇ','ｈ','ｉ','ｊ','ｋ','ｌ','ｍ','ｎ','ｏ','ｐ','ｑ','ｒ','ｓ','ｔ','ｕ','ｖ','ｗ','ｘ','ｙ','ｚ',
			'Ａ','Ｂ','Ｃ','Ｄ','Ｅ','Ｆ','Ｇ','Ｈ','Ｉ','Ｊ','Ｋ','Ｌ','Ｍ','Ｎ','Ｏ','Ｐ','Ｑ','Ｒ','Ｓ','Ｔ','Ｕ','Ｖ','Ｗ','Ｘ','Ｙ','Ｚ',
			'“','”','″','＆','．','＿','＄','（','）','［','］','＋','－','−','＊','／','％','！','＝','＜','＞','＠','：','；','，','？','‘',"’","′",'　','＃','♯','｜'
		];
 
		var hankaku = [
			'0','1','2','3','4','5','6','7','8','9',
			'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
			'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
			'"','"','"','&','.','_','$','(',')','[',']','+','-','-','*','/','%','!','=','<','>','@',':',';',',','?',"'","'","'",' ','#','#','|'
		];
 
		var re_zenkaku = new RegExp('[' + zenkaku.join('') +']', 'g');
		var z2h = new Object();
		for (var i = 0; i < zenkaku.length; i++) {
			z2h[zenkaku[i]] = hankaku[i];
		}
 
		return function(str) {
			return str.replace(re_zenkaku, function(m0) {
				return z2h[m0];
			});
		};
	})(),
 
	Keyword: function(name, command) {
		this.name = name;
		if (command)
			this.command = command;
	},
	Word: (function() {
		function Word(name) {
			this.name = name;
		}
		Word.prototype.isExprHead = true;
		return Word;
	})(),
	Symbol: function(name) {
		this.name = name;
	},
	Label: (function() {
		function Label(name) {
			this.name = name;
		}
		Label.prototype.isExprHead = true;
		return Label;
	})(),
	NoQuoteStr: function(str) {
		this.str = str;
	},
	token: new Object(),
	sysVars: {
		'CSRX':     true,
		'CSRY':     true,
		'CSRZ':     true,
		'FREEMEM':  true,
		'VERSION':  true,
		'TABSTEP':  false,
		'SYSBEEP':  false,
		'ERRNUM':   true,
		'ERRLINE':  true,
		'ERRPRG':   true,
		'PRGLINE':  true,
		'PRGSLOT':  true,
		'RESULT':   true,
		'MAINCNT':  true,
		'MICPOS':   true,
		'MICSIZE':  true,
		'MPCOUNT':  true,
		'MPHOST':   true,
		'MPLOCAL':  true,
		'TRUE':     true,
		'FALSE':    true,
		'TIME$':    true,
		'DATE$':    true
	},
	operators: [ /^(?:&&|\|\|)/, /^(?:AND\b|OR\b|XOR\b)/i, /^(?:==|!=|>=?|<=?)/, /^(?:<<|>>)/, /^[+-]/, /^(?:[*\/]|DIV\b|MOD\b)/i ]
};
 
(function() {
	var commands = [
		'GOTO','GOSUB','RETURN','RESTORE','SWAP','CALL','USE','EXEC'
	];
	for (var i = 0; i < commands.length; ++i)
		SBParser.token[commands[i]] = new SBParser.Keyword(commands[i], SBParser.parseStmtCommand);
	SBParser.token['CALL'].isExprHead = true;
 
	var noArgCommands = [
		'BREAK','CONTINUE','END'
	];
	for (var i = 0; i < noArgCommands.length; ++i)
		SBParser.token[noArgCommands[i]] = new SBParser.Keyword(noArgCommands[i], SBParser.parseStmtNoArgCommand);
 
	SBParser.token['IF']     = new SBParser.Keyword('IF'     , SBParser.parseStmtIf);
	SBParser.token['ON']     = new SBParser.Keyword('ON'     , SBParser.parseStmtOn);
	SBParser.token['FOR']    = new SBParser.Keyword('FOR'    , SBParser.parseStmtFor);
	SBParser.token['WHILE']  = new SBParser.Keyword('WHILE'  , SBParser.parseStmtWhile);
	SBParser.token['REPEAT'] = new SBParser.Keyword('REPEAT' , SBParser.parseStmtRepeat);
	SBParser.token['DEF']    = new SBParser.Keyword('DEF'    , SBParser.parseStmtDef);
	SBParser.token['VAR']    = new SBParser.Keyword('VAR'    , SBParser.parseStmtDim);
	SBParser.token['DIM']    = new SBParser.Keyword('DIM'    , SBParser.parseStmtDim);
	SBParser.token['READ']   = new SBParser.Keyword('READ'   , SBParser.parseStmtVarArgs);
	SBParser.token['DATA']   = new SBParser.Keyword('DATA'   , SBParser.parseStmtData);
	SBParser.token['PRINT']  = new SBParser.Keyword('PRINT'  , SBParser.parseStmtPrint);
	SBParser.token['?']      = new SBParser.Keyword('?'      , SBParser.parseStmtPrint);
	SBParser.token['INPUT']  = new SBParser.Keyword('INPUT'  , SBParser.parseStmtInput);
	SBParser.token['LINPUT'] = new SBParser.Keyword('LINPUT' , SBParser.parseStmtInput);
	SBParser.token['COMMON'] = new SBParser.Keyword('COMMON' , SBParser.parseStmtDef);
	SBParser.token['VAR'].isExprHead = true;
 
	var otherKeywords = [
		'THEN','ELSE','ENDIF','NEXT','WEND','UNTIL',
		'AND','OR','XOR','NOT','TRUE','FALSE','OUT'
	];
	for (var i = 0; i < otherKeywords.length; ++i)
		SBParser.token[otherKeywords[i]] = new SBParser.Keyword(otherKeywords[i]);
	SBParser.token['TRUE'].isExprHead = true;
	SBParser.token['FALSE'].isExprHead = true;
 
	SBParser.token['EOF'] = { name: 'EOF', isEnd: true };
	SBParser.token['EOL'] = { name: 'EOL', isEnd: true };
 
	var symbols = ['-','+','*','/',',',':',';','(',')','[',']','=','==','!','!=','>','>=','<','<=','&&','||','<<','>>'];
	for (var i = 0; i < symbols.length; ++i)
		SBParser.token[symbols[i]] = new SBParser.Symbol(symbols[i]);
	SBParser.token['('].isExprHead = true;
	SBParser.token['-'].isExprHead = true;
	SBParser.token['!'].isExprHead = true;
	SBParser.token['NOT'].isExprHead = true;
	SBParser.token[':'].isEnd = true;
 
	SBParser.token['DIV'] = new SBParser.Word('DIV');
	SBParser.token['MOD'] = new SBParser.Word('MOD');
 
	// only binary operator ranking
	// Unary operators require special treatment
	SBParser.token['&&'].binPri = 1;
	SBParser.token['||'].binPri = 1;
	SBParser.token['AND'].binPri = 2;
	SBParser.token['OR'].binPri = 2;
	SBParser.token['XOR'].binPri = 2;
	SBParser.token['=='].binPri = 3;
	SBParser.token['!='].binPri = 3;
	SBParser.token['>'].binPri = 3;
	SBParser.token['>='].binPri = 3;
	SBParser.token['<'].binPri = 3;
	SBParser.token['<='].binPri = 3;
	SBParser.token['<<'].binPri = 4;
	SBParser.token['>>'].binPri = 4;
	SBParser.token['+'].binPri = 5;
	SBParser.token['-'].binPri = 5;
	SBParser.token['*'].binPri = 6;
	SBParser.token['/'].binPri = 6;
	SBParser.token['DIV'].binPri = 6;
	SBParser.token['MOD'].binPri = 6;
})();
 
 
//----------------CreateForms--------------------------------------------------
 
//----------------Checker------------------------------------------------------
 
// $Date: 2012/07/29 00:30:59 $
 
function checkSource(text) {
	var msgs = new Array();
	//checkLineLength(text, msgs);
	var stmts = SBParser.parse(text, msgs);
	//addLastVarDef(stmts);
	var varInfo = collectVariable(stmts);
	checkLabel(varInfo, msgs);
	checkVariable(varInfo, msgs);
	//checkType(stmts, msgs);
	checkDeadCode(stmts, msgs);
	//checkForNext(stmts, msgs);
	//checkBgmsetdMml(stmts, varInfo, msgs);
	msgs.sort(SBParser.compareMessage);
	return { msgs: msgs, labels: makeLabelList(varInfo), vars: makeVariableList(varInfo) };
}
 
function checkLineLength(text, msgs) {
	var lines = text.split('\n');
	if (lines[lines.length - 1] == '')
		lines.pop();
	var cnt = 0;
	for (var i = 0; i < lines.length; ++i) {
		if (i == 999999) {
			msgs.push(new SBParser.Error(new SBParser.Position(cnt, i+1, 1), 'Program exceeds 999999 lines.'));
			break;
		}
		cnt += lines[i].length+1;
	}
}
 
function checkLabel(info, msgs) {
	var labels = info.labels;
	var regex = createStrLabelRegexAll(info.strLabels);
    for (var name in labels)
		if (labels[name].def) {
			if (labels[name].def.length > 1)
				for (var i = 1; i < labels[name].def.length; ++i)
					msgs.push(new SBParser.Error(labels[name].def[i], 'Label ' + name + ' has already been defined in ' + labels[name].def[0] + '.'));
			// Even if not used, it may not be an error as it may be in string form
			if (!labels[name].ref && !regex.test(name))
				msgs.push(new SBParser.Warning(labels[name].def[0], 'Label ' + name + ' is not used even once.'));
		}
		else
			for (var i = 0; i < labels[name].ref.length; ++i)
				msgs.push(new SBParser.Error(labels[name].ref[i], 'Label ' + name + ' does not exist.'));
	for (var name in info.funcs)
		for (var i = 0; i < info.funcs[name].length; ++i)
			checkLabel(info.funcs[name][i], msgs);
}
 
// generate regular expression for strings used for labels
function createStrLabelRegexAll(exprs) {
	var regexs = new Array();
	for (var i = 0; i < exprs.length; ++i) {
		var regex = createStrLabelRegex(exprs[i]);
		if (regex)
			regexs.push(regex);
	}
	return new RegExp('^(?:' + regexs.join('|') + ')$');
}
function createStrLabelRegex(expr) {
	expr = findAssignedValue(expr);
	if (expr.type == 'string')
		return (/^@?\w*$/.test(expr.val)) ? expr.val.toUpperCase() : false;
	if (expr.type == 'function') {
		if (expr.name == '+') {
			var regex1 = createStrLabelRegex(expr.args[0]);
			var regex2 = createStrLabelRegex(expr.args[1]);
			return (regex1 && regex2) ? regex1 + regex2 : false;
		}
		else if (expr.name == 'STR$')
			return '(?:0|[1-9][0-9]{0,5})';
		else if (expr.name == 'HEX$') {
			if (expr.args.length < 2)
				return '(?:0|[1-9A-F][0-9A-F]{0,4})';
			var expr1 = findAssignedValue(expr.args[1]);
			if (expr1.type == 'number' && expr1.val >= 1 && expr1.val <= 5)
				return '[0-9A-F]{' + expr1.val + '}';
			else
				return '[0-9A-F]{1,5}';
		}
		else if (/^(?:LEFT|RIGHT)\$$/.test(expr.name) && expr.args.length >= 2) {
			var expr1 = findAssignedValue(expr.args[1]);
			if (expr1.type == 'number' && expr1.val >= 0)
				return '[@\\w]{0,' + expr1.val + '}';
		}
	}
	return '[@\\w]*';
}
 
function makeLabelList(info) {
	var list = new Array();
	var labels = info.labels;
	for (var name in labels)
		if (labels[name].def)
			list.push({ name: name, pos: labels[name].def[0] });
	list.sort(function(a, b) { return a.name.localeCompare(b.name); });
	var names = new Array();
	for (var name in info.funcs)
		names.push(name);
	names.sort();
	for (var i=0; i < names.length; ++i) {
		var funcs = info.funcs[names[i]];
		for (var j = 0; j < funcs.length; ++j) {
			list.push({ name: names[i], pos: funcs[j].pos });
			var list2 = makeLabelList(funcs[j]);
			for (var k = 0; k < list2.length; ++k)
				list.push({ name: names[i] + ':' + list2[k].name, pos: list2[k].pos });
		}
	}
	return list;
}
 
function checkVariable(info, msgs) {
	for (var name in info.vars) {
		var vars = info.vars[name];
		if (vars.vardef && vars.vardef[0].cnt < 0)
			continue;
		var varUse = vars.vardef || vars.varref || vars.varset;
		var aryUse = vars.arydef || vars.aryref || vars.aryset;
		var def = (vars.def || []).concat(vars.vardef || []).concat(vars.arydef || []);
		var ref = (vars.ref || []).concat(vars.varref || []).concat(vars.aryref || []);
		var set = (vars.set || []).concat(vars.varset || []).concat(vars.aryset || []);
		if (def.length > 0) {
			if (def.length > 1) {
				def.sort(SBParser.comparePosition);
				for (var i = 1; i < def.length; ++i)
				    msgs.push(new SBParser.Error(def[i], 'Variable ' + name + ' is already defined in ' + def[0] + '.'));
			}
			if (ref.length <= 0)
			    msgs.push(new SBParser.Warning(def[0], 'Variable ' + name + ' is not used even once.'));
			else if (varUse && set.length <= 0)
			    msgs.push(new SBParser.Warning(def[0], 'Variable ' + name + ' has not been assigned a value even once.'));
		}
		else
			if (ref.length <= 0)
				for (var i = 0; i < set.length; ++i)
				    msgs.push(new SBParser.Warning(set[i], 'Variable ' + name + ' is not used even once.'));
			else if (varUse && set.length <= 0)
				for (var i = 0; i < ref.length; ++i)
				    msgs.push(new SBParser.Warning(ref[i], 'Variable ' + name + ' has not been assigned a value even once.'));
			else if (aryUse)
				for (var i = 0; i < set.length; ++i)
					msgs.push(new SBParser.Warning(set[i], 'Variable ' + name + ' has not been declared.'));
	}
	for (var name in info.funcs)
		for (var i = 0; i < info.funcs[name].length; ++i)
			checkVariable(info.funcs[name][i], msgs);
}
 
function collectVariable(stmts, vars) {
	var labels = new Object();
	var strLabels = new Array();
	var funcs = new Object();
	if (!vars) {
		vars = new Object();
		for (var name in SBParser.sysVars)
			vars[name] = { vardef: [new SBParser.Position(-1,-1,-1)] };
	}
	for (var i = 0; i < stmts.length; ++i)
		collectVariableStmt(stmts[i], vars, labels, strLabels, funcs);
	return { vars: vars, labels: labels, strLabels: strLabels, funcs: funcs };
}
 
function collectVariableStmt(stmt, vars, labels, strLabels, funcs) {
	if (/^(?:OPTION|XON|XOFF)$/.test(stmt.name))
		return;
	if (stmt.name == '@') {
		var label = stmt.exprs[0];
		var l = labels[label.name] || (labels[label.name] = new Object());
		(l.def || (l.def = new Array())).push(label.pos);
		return;
	}
	if (/^(?:COMMON)?DEF$/.test(stmt.name)) {
		var vars2 = new Object();
		for (var i = 0; i < stmt.vars.length; ++i) {
			var variable = stmt.vars[i];
			if (variable.type == 'function' && variable.name == '[]' && variable.args.length == 1)
				variable = variable.args[0];
			if (variable.type == 'variable') {
				var v = vars2[variable.name] || (vars2[variable.name] = new Object());
				(v.def || (v.def = new Array())).push(variable.pos);
				(v.ref || (v.ref = new Array())).push(variable.pos);
			}
		}
		for (var i = 0; i < stmt.exprs.length; ++i ) {
			var variable = stmt.exprs[i];
			if (variable.type == 'function' && variable.name == '[]' && variable.args.length == 1)
				variable = variable.args[0];
			if (variable.type == 'variable') {
				var v = vars2[variable.name] || (vars2[variable.name] = new Object());
				(v.def || (v.def = new Array())).push(variable.pos);
				(v.set || (v.set = new Array())).push(variable.pos);
			}
		}
		var varInfo = collectVariable(stmt.stmts, vars2);
		for (var name in varInfo.vars) {
			if (!varInfo.vars[name].def && !varInfo.vars[name].vardef && !varInfo.vars[name].arydef) {
				vars[name] = vars[name] || new Object();
				for (var name2 in varInfo.vars[name])
					vars[name][name2] = (vars[name][name2] || new Array()).concat(varInfo.vars[name][name2]);
				delete varInfo.vars[name];
			}
		}
		varInfo.pos = stmt.pos;
		(funcs[stmt.name2] || (funcs[stmt.name2] = new Array())).push(varInfo);
		return;
	}
	if (stmt.vars) {
		if (/^(?:VAR|DIM)$/.test(stmt.name))
			for (var i = 0; i < stmt.vars.length; ++i) {
				collectVariableVar(stmt.vars[i], vars,
					function(v, variable) {
						(v.vardef || (v.vardef = new Array())).push(variable.pos);
						if (stmt.exprs[i].type != 'null')
							(v.varset || (v.varset = new Array())).push(variable.pos);
					},
					function(v, variable) {
						(v.arydef || (v.arydef = new Array())).push(variable.pos);
					});
			}
		else
			for (var i = 0; i < stmt.vars.length; ++i) {
				collectVariableVar(stmt.vars[i], vars,
					function(v, variable) {
						(v.set || (v.set = new Array())).push(variable.pos);
					},
					function(v, variable) {
						if (/\$$/.test(variable.name))
							(v.set || (v.set = new Array())).push(variable.pos);
						else
							(v.aryset || (v.aryset = new Array())).push(variable.pos);
					});
			}
		// Check also for variable in FOR
		if (stmt.name == 'FOR')
			collectVariableExpr(stmt.vars[0], vars, labels);
		// Check only the array subscript
		else
			for (var i = 0; i < stmt.vars.length; ++i) {
				var variable = stmt.vars[i];
				if (variable.args) {
					for (var j = (variable.type == 'function' && variable.name == '[]') ? 1 : 0; j < variable.args.length; ++j)
						collectVariableExpr(variable.args[j], vars, labels);
				}
			}
	}
	if (stmt.exprs)
		for (var i = 0; i < stmt.exprs.length; ++i)
			collectVariableExpr(stmt.exprs[i], vars, labels);
	if (/^(?:RESTORE|GOTO|GOSUB)$/.test(stmt.name) && stmt.exprs.length >= 1)
		collectVariableLabel(stmt.exprs[0], labels, strLabels);
	else if (stmt.name == 'COPY' && stmt.exprs.length >= 2 && calcType(stmt.exprs[1]) == TYPE_STRING)
		collectVariableLabel(stmt.exprs[1], labels, strLabels);
	else if (stmt.name == 'COPY' && stmt.exprs.length >= 3 && calcType(stmt.exprs[2]) == TYPE_STRING && stmt.exprs[2].type != 'variable')
		collectVariableLabel(stmt.exprs[2], labels, strLabels);
	else if (stmt.name == 'SPDEF' && stmt.exprs.length >= 1 && calcType(stmt.exprs[0]) == TYPE_STRING)
		collectVariableLabel(stmt.exprs[0], labels, strLabels);
	else if (/^(?:SPANIM|BGANIM)$/.test(stmt.name) && stmt.exprs.length >= 3 && calcType(stmt.exprs[2]) == TYPE_STRING)
		collectVariableLabel(stmt.exprs[2], labels, strLabels);
	else if (stmt.name == 'BGMSETD' && stmt.exprs.length >= 2)
		collectVariableLabel(stmt.exprs[1], labels, strLabels);
	if (stmt.stmts)
		for (var i = 0; i < stmt.stmts.length; ++i)
			collectVariableStmt(stmt.stmts[i], vars, labels, strLabels, funcs);
	if (stmt.stmts2)
		for (var i = 0; i < stmt.stmts2.length; ++i)
			collectVariableStmt(stmt.stmts2[i], vars, labels, strLabels, funcs);
}
 
function collectVariableVar(variable, vars, varFunc, aryFunc) {
	if (variable.type == 'variable') {
		var v = vars[variable.name] || (vars[variable.name] = new Object());
		varFunc(v, variable);
	}
	else if (variable.type == 'function' && variable.name == '[]')
		collectVariableVar(variable.args[0], vars, aryFunc, aryFunc);
}
 
function collectVariableExpr(expr, vars, labels) {
	if (expr.type == 'function' && expr.name == '[]') {
		var expr2 = expr.args[0];
		if (expr2.type == 'variable') {
			var v = vars[expr2.name] || (vars[expr2.name] = new Object());
			if (/\$$/.test(expr2.name))
				(v.ref || (v.ref = new Array())).push(expr2.pos);
			else
				(v.aryref || (v.aryref = new Array())).push(expr2.pos);
			for (var i = 1; i < expr.args.length; ++i)
				collectVariableExpr(expr.args[i], vars, labels);
			return;
		}
	}
	else if (expr.type == 'variable') {
		var v = vars[expr.name] || (vars[expr.name] = new Object());
		(v.ref || (v.ref = new Array())).push(expr.pos);
	}
	else if (expr.type == 'label') {
		var l = labels[expr.name] || (labels[expr.name] = new Object());
		(l.ref || (l.ref = new Array())).push(expr.pos);
	}
	if (expr.args)
		for (var i = 0; i < expr.args.length; ++i)
			collectVariableExpr(expr.args[i], vars, labels);
}
 
function collectVariableLabel(expr, labels, strLabels) {
	var expr2 = findAssignedValue(expr);
	// Ignored because labels are processed in normal flow
	if (expr2.type == 'label')
		return;
	// Perform same preocessing as labels in terms of confirming validity if it is a string
	else if (expr2.type == 'string') {
		if (!/^@\w+$/.test(expr2.val))
			return;
		var u = expr2.val.toUpperCase();
		var l = labels[u] || (labels[u] = new Object());
		(l.ref || (l.ref = new Array())).push(expr.pos);
	}
	// Otherwise just keep existence confirmation of label
	else
		strLabels.push(expr);
}
 
function makeVariableList(info) {
	var list = new Array();
	var vars = info.vars;
	for (var name in vars) {
		var vars = info.vars[name];
		if (vars.vardef && vars.vardef[0].cnt < 0)
			continue;
		var def = (vars.def || []).concat(vars.vardef || []).concat(vars.arydef || []);
		var set = (vars.set || []).concat(vars.varset || []).concat(vars.aryset || []);
		if (def.length > 0) {
			def.sort(SBParser.comparePosition);
			list.push({ name: name, pos: def[0] });
		}
		else if (set.length > 0) {
			set.sort(SBParser.comparePosition);
			list.push({ name: name, pos: set[0] });
		}
	}
	list.sort(function(a, b) { return a.name.localeCompare(b.name); });
	var names = new Array();
	for (var name in info.funcs)
		names.push(name);
	names.sort();
	for (var i=0; i < names.length; ++i) {
		var funcs = info.funcs[names[i]];
		for (var j = 0; j < funcs.length; ++j) {
			var list2 = makeVariableList(funcs[j]);
			for (var k = 0; k < list2.length; ++k)
				list.push({ name: names[i] + ':' + list2[k].name, pos: list2[k].pos });
		}
	}
	return list;
}
 
var TYPE_NUMBER = 1;
var TYPE_STRING = 2;
var TYPE_ANY = 3;
var TYPE_LABEL = 4;
var TYPE_NULL = 8;
 
function TypeChecker(expected, pred) {
	this.expected = expected;
	this.pred = pred;
}
TypeChecker.prototype.check = function(expr, type, msgs) {
	if ((type & this.expected) == 0) {
		var pos = (expr.args && expr.args.length == 2 && /^\W/.test(expr.name)) ? expr.args[0].pos : expr.pos;
		if (type == TYPE_NULL)
			msgs.push(new SBParser.Error(pos, 'Argument required.'));
		else if (this.expected & TYPE_LABEL)
			msgs.push(new SBParser.Error(pos, 'Label required.'));
		else if (type == TYPE_NUMBER)
			msgs.push(new SBParser.Error(pos, 'String required.'));
		else
			msgs.push(new SBParser.Error(pos, 'Number required.'));
		return false;
	}
	return !this.pred || this.pred(expr, msgs);
};
 
function createVariableChecker(type) {
	return new TypeChecker(type, function(expr, msgs) {
		if (expr.type != 'variable' && expr.type != 'array' && expr.type != 'null') {
			msgs.push(new SBParser.Error(expr.pos, 'Variable required.'));
			return false;
		}
		if (expr.type == 'variable' && SBParser.sysVars[expr.name]) {
			msgs.push(new SBParser.Error(expr.pos, 'Variable ' + expr.name + ' is not writable.'));
			return false;
		}
		return true;
	});
}
function createMinChecker(min, warn) {
	var err = (warn) ? SBParser.Warning : SBParser.Error;
	return new TypeChecker(TYPE_NUMBER, function(expr, msgs) {
		expr = findAssignedValue(expr);
		if (expr.type == 'number' && expr.val < min) {
			msgs.push(new err(expr.pos, 'Number is too small. (' + min + 'or more)'));
			return false;
		}
		return true;
	});
}
function createRangeChecker(min, max, warn, type) {
	var err = (warn) ? SBParser.Warning : SBParser.Error;
	return new TypeChecker(TYPE_NUMBER | (type || 0), function(expr, msgs) {
		expr = findAssignedValue(expr);
		if (expr.type == 'number') {
			if (expr.val < min) {
				msgs.push(new err(expr.pos, 'Number is too small. (' + min + '-' + max + ')'));
				return false;
			}
			else if (expr.val >= max + 1) {
				msgs.push(new err(expr.pos, 'Number is too large. (' + min + '-' + max + ')'));
				return false;
			}
		}
		return true;
	});
}
function createSelectChecker(vals) {
	return new TypeChecker(TYPE_NUMBER, function(expr, msgs) {
		expr = findAssignedValue(expr);
		if (expr.type == 'number' && !SBParser.contains(vals, Math.floor(expr.val))) {
			msgs.push(new SBParser.Error(expr.pos, 'Incorrect number. (' + vals.join(',') + ')'));
			return false;
		}
		return true;
	});
}
function createRegexChecker(regex, type) {
	return new TypeChecker(TYPE_STRING | (type || 0), function(expr, msgs) {
		expr = findAssignedValue(expr);
		if (expr.type == 'string' && !regex.test(expr.val)) {
			msgs.push(new SBParser.Error(expr.pos, 'Incorrect string.'));
			return false;
		}
		return true;
	});
}

//start of section
var CHK_NUMBER = new TypeChecker(TYPE_NUMBER);
var CHK_STRING = new TypeChecker(TYPE_STRING);
var CHK_ANY = new TypeChecker(TYPE_ANY);
var CHK_LABEL_STR = createRegexChecker(/^@\w+$/, TYPE_LABEL);
var CHK_LABEL_NULL = new TypeChecker(TYPE_LABEL | TYPE_NULL);
var CHK_NUMBER_VAR = createVariableChecker(TYPE_NUMBER);
var CHK_STRING_VAR = createVariableChecker(TYPE_STRING);
var CHK_ANY_VAR = createVariableChecker(TYPE_ANY);
var CHK_NUMBER_NULL_VAR = createVariableChecker(TYPE_NUMBER | TYPE_NULL);
var CHK_NONNEGATIVE = createMinChecker(0);
var CHK_BOOL = createRangeChecker(0,1);
var CHK_RESOURCE = createRegexChecker(/^(?:(?:PRG[0-3]?|GRP[0-5]):)?[A-Z0-9_]{1,14}$/);
var CHK_FILE = createRegexChecker(/^(?:(?:TXT|DAT):)?[A-Z0-9_]{1,14}$/);
/*maybe works*/var CHK_COLOR = createRegexChecker(/^RGB\((?:(?:[0-255])?[0-255],[0-255],[0-255])\)$/);
var CHK_CHR_NUMBER = createRangeChecker(0,4095);
/*investigate*/var CHK_SP_NUMBER = createRangeChecker(0,511);
/*maybe fixed?*/var CHK_CONSOLE_WIDTH = createRangeChecker(0,50,true);
/*will fix*/var CHK_CONSOLE_HEIGHT = createRangeChecker(0,30,true);
var CHK_CONSOLE_Z = createRangeChecker(-256,1024)
/*investigate*/var CHK_CONSOLE_COLOR = createRangeChecker(0,15);
var CHK_GRP_WIDTH = createRangeChecker(0,511,true);
var CHK_GRP_HEIGHT = createRangeChecker(0,511,true);
var CHK_GRP_PAGE = createRangeChecker(0,5);
var CHK_BG_LAYER = createRangeChecker(0,3);
var CHK_BGM_TRACK = createRangeChecker(0,7);
/*investigate*/var CHK_BGM_NUMBER = new TypeChecker(TYPE_NUMBER, function(expr, msgs) {
	expr = findAssignedValue(expr);
	if (expr.type == 'number') {
		if (29 < expr.val && expr.val < 128 || expr.val >= 257)
			msgs.push(new SBParser.Error(expr.pos, 'Incorrect value.'));
		else if (expr.val < 0 || expr.val >= 256)
			msgs.push(new SBParser.Warning(expr.pos, 'Incorrect value'));
	}
	return true;
});
var CHK_SORT_ARY = new TypeChecker(TYPE_ANY, function(expr, msgs) {
	if (expr.type != 'variable')
		msgs.push(new SBParser.Error(expr.pos, 'Array name required.'));
});
//end of section
 
// List of instructions with arguments by number and type (exprs: expression, vars: variable)
// Argument number is any one of the values in the case of an array, in the case of a number, requires more arguments
// If the argument number is a number will appear last type is repeated
// If multiple are true, type differs by the argument number
var expectedCommand = {
	DIM: { vars: { arity: 1, types: [new TypeChecker(TYPE_ANY, function(expr, msgs) {
		if (expr.type != 'array') {
			msgs.push(new SBParser.Error(expr.pos, 'Array required.'));
			return false;
		}
		return true;
	})] } },
	'@': { exprs: { arity: [1], types: [new TypeChecker(TYPE_LABEL)] } },
	KEY: { exprs: { arity: [2], types: [createRangeChecker(1,5), CHK_STRING] } },
	READ: { vars: { arity: 0, types: [CHK_ANY_VAR] } },
	DATA: { exprs: { arity: 1, types: [CHK_STRING] } },
	RESTORE: { exprs: {arity: [1], types: [CHK_LABEL_STR] } },
	GOTO: { exprs: {arity: [1], types: [CHK_LABEL_STR] } },
	GOSUB: { exprs: {arity: [1], types: [CHK_LABEL_STR] } },
	RETURN: { exprs: { arity: [0], types: [] } },
	STOP: { exprs: { arity: [0], types: [] } },
	END: { exprs: { arity: [0], types: [] } },
	FOR: { exprs: { arity: [2,3], types: [CHK_NUMBER, CHK_NUMBER, CHK_NUMBER] }, vars: { arity: [1], types: [CHK_NUMBER_VAR] } },
	NEXT: { vars: { arity: [0,1], types: [new TypeChecker(TYPE_NUMBER, function(expr, msgs) {
		// Can not specify an array to NEXT (although it can be specified to FOR.  Would need to be omitted in corresponding NEXT)
		if (expr.type != 'variable') {
			msgs.push(new SBParser.Error(expr.pos, 'Variable required.'));
			return false;
		}
		if (expr.type == 'variable' && SBParser.sysVars[expr.name]) {
			msgs.push(new SBParser.Error(expr.pos, 'Variable ' + expr.name + ' is not writable.'));
			return false;
		}
		return true;
	})] } },
	IF: { exprs: { arity: [1], types: [CHK_NUMBER] } },
	ONGOTO: { exprs: { arity: 2, types: [CHK_NUMBER, CHK_LABEL_NULL] } },
	ONGOSUB: { exprs: { arity: 2, types: [CHK_NUMBER, CHK_LABEL_NULL] } },
	TMREAD: { exprs: { arity: [1], types: [createRegexChecker(/^\d{2}:\d{2}:\d{2}$/)] }, vars: { arity: [3], types: [CHK_NUMBER_VAR, CHK_NUMBER_VAR, CHK_NUMBER_VAR] } },
	DTREAD: { exprs: { arity: [1], types: [createRegexChecker(/^\d{4}\/\d{2}\/\d{2}$/)] }, vars: { arity: [3], types: [CHK_NUMBER_VAR, CHK_NUMBER_VAR, CHK_NUMBER_VAR] } },
	VSYNC: { exprs: { arity: [1], types: [CHK_NONNEGATIVE] } },
	WAIT: { exprs: { arity: [1], types: [CHK_NONNEGATIVE] } },
	SORT: { exprs: { arity: [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35], types: [CHK_NONNEGATIVE, CHK_NUMBER, CHK_SORT_ARY] } },
	RSORT: { exprs: { arity: [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35], types: [CHK_NONNEGATIVE, CHK_NUMBER, CHK_SORT_ARY] } },
	CLS: { exprs: { arity: [0], types: [] } },
	COLOR: { exprs: { arity: [1,2], types: [CHK_CONSOLE_COLOR, CHK_CONSOLE_COLOR] } },
	LOCATE: { exprs: { arity: [2], types: [CHK_CONSOLE_WIDTH, CHK_CONSOLE_HEIGHT] } },
	PRINT: { exprs: { arity: 0, types: [CHK_ANY] } },
	INPUT: { exprs: { arity: [0,1], types: [CHK_STRING] }, vars: { arity: 1, types: [CHK_ANY_VAR] } },
	LINPUT: { exprs: { arity: [0,1], types: [CHK_STRING] }, vars: { arity: [1], types: [CHK_STRING_VAR] } },
	BREPEAT: { exprs: { arity: [1,3], types: [createRangeChecker(0,12), CHK_NONNEGATIVE, CHK_NONNEGATIVE] } },
	LOAD: { exprs: { arity: [1,2], types: [CHK_RESOURCE, CHK_NUMBER] } },
	SAVE: { exprs: { arity: [1,2], types: [CHK_RESOURCE, createRegexChecker(/^[0-9A-F]{0,12}$/)] } },
	DELETE: { exprs: { arity: [1], types: [CHK_FILE] } },
	EXEC: { exprs: { arity: [1], types: [createRegexChecker(/^(?:PRG:)?[A-Z0-9_]{1,8}$/)] } },
	RENAME: { exprs: { arity: [2], types: [CHK_FILE, createRegexChecker(/^[A-Z0-9_]{1,8}$/)] } },
	VISIBLE: { exprs: { arity: [6], types: [createRangeChecker(0,1,false,TYPE_NULL),
											createRangeChecker(0,1,false,TYPE_NULL),
											createRangeChecker(0,1,false,TYPE_NULL),
											createRangeChecker(0,1,false,TYPE_NULL),
											createRangeChecker(0,1,false,TYPE_NULL),
											createRangeChecker(0,1)] } },
	COLINIT: { exprs: { arity: [0,1,2], types: [CHK_COL_BANK, CHK_COLOR] } },
	COLSET: { exprs: { arity: [3], types: [CHK_COL_BANK, CHK_COLOR, createRegexChecker(/^[0-9A-F]{6}$/i)] } },
	COLREAD: { exprs: { arity: [2], types: [CHK_COL_BANK, CHK_COLOR] }, vars: { arity: [3], types: [CHK_NUMBER_VAR, CHK_NUMBER_VAR, CHK_NUMBER_VAR] } },
	ACLS: { exprs: { arity: [0], types: [] } },
	CHRINIT: { exprs: { arity: [1], types: [CHK_CHR_NAME] } },
	CHRSET: { exprs: { arity: [3], types: [CHK_CHR_NAME, CHK_CHR_NUMBER, createRegexChecker(/^[0-9A-F]{64}$/i)] } },
	CHRREAD: { exprs: { arity: [2], types: [CHK_CHR_NAME, CHK_CHR_NUMBER] }, vars: { arity: [1], types: [CHK_STRING_VAR] } },
	SPPAGE: { exprs: { arity: [1], types: [createRangeChecker(0,1)] } },
	SPSET: { exprs: { arity: [6,8], types: [CHK_SP_NUMBER, createRangeChecker(0,511), CHK_PALETTE, CHK_BOOL, CHK_BOOL, createRangeChecker(0,3), createSelectChecker([8,16,32,64]), createSelectChecker([8,16,32,64])] } },
	SPCLR: { exprs: { arity: [0,1], types: [CHK_SP_NUMBER] } },
	/*What to do about binary attributes?*/SPCHR: { exprs: { arity: [2,6], types: [CHK_SP_NUMBER, createRangeChecker(0,511), createRangeChecker(0,511), createRangeChecker(0,511), createRangeChecker(0-511), ] } },
	SPANIM: { exprs: { arity: [3,4], types: [CHK_SP_NUMBER, createMinChecker(1), CHK_NONNEGATIVE, CHK_NONNEGATIVE] } },
	SPOFS: { exprs: { arity: [3,4], types: [CHK_SP_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NONNEGATIVE] } },
	SPANGLE: { exprs: { arity: [2,3,4], types: [createRangeChecker(0,31), CHK_NUMBER, CHK_NONNEGATIVE, createSelectChecker([-1,1])] } },
	SPSCALE: { exprs: { arity: [2,3], types: [createRangeChecker(0,31), new TypeChecker(TYPE_NUMBER, function(expr, msgs) {
		expr = findAssignedValue(expr);
		if (expr.type == 'number') {
			if (expr.val < 0) {
				msgs.push(new err(expr.pos, 'Number is too small. (0-2.0)'));
				return false;
			}
			else if (expr.val > 2) {
				msgs.push(new err(expr.pos, 'Number is too large. (0-2.0)'));
				return false;
			}
		}
		return true;
	}), CHK_NONNEGATIVE] } },
	SPREAD: { exprs: { arity: [1], types: [CHK_SP_NUMBER] }, vars: { arity: [1,2,3,4,5], types: [CHK_NUMBER_NULL_VAR, CHK_NUMBER_NULL_VAR, CHK_NUMBER_NULL_VAR, CHK_NUMBER_NULL_VAR, CHK_NUMBER_VAR] } },
	SPSETV: { exprs: { arity: [3], types: [CHK_SP_NUMBER, createRangeChecker(0,7), CHK_NUMBER] } },
	SPHOME: { exprs: { arity: [3], types: [CHK_SP_NUMBER, CHK_NUMBER, CHK_NUMBER] } },
	SPCOL: { exprs: { arity: [6,7], types: [CHK_SP_NUMBER, createRangeChecker(-128,127), createRangeChecker(-128,127), createRangeChecker(0,255), createRangeChecker(0,255), CHK_BOOL, createRangeChecker(0,255)] } },
	SPCOLVEC: { exprs: { arity: [1,3], types: [CHK_SP_NUMBER, CHK_NUMBER, CHK_NUMBER] } },
	BGPAGE: { exprs: { arity: [1], types: [createRangeChecker(0,1)] } },
	BGCLIP: { exprs: { arity: [4], types: [createRangeChecker(0,31), createRangeChecker(0,23), createRangeChecker(0,31), createRangeChecker(0,23)] } },
	BGOFS: { exprs: { arity: [3,4], types: [CHK_BG_LAYER, CHK_NUMBER, CHK_NUMBER, CHK_NONNEGATIVE] } },
	BGCLR: { exprs: { arity: [0,1], types: [CHK_BG_LAYER] } },
	BGCOPY: { exprs: { arity: [7], types: [CHK_BG_LAYER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER] } },
	BGPUT: { exprs: { multiple: true, arity: [4,7], types: [[CHK_BG_LAYER, CHK_NUMBER, CHK_NUMBER, createRegexChecker(/^[0-9A-F]{4}$/, TYPE_NUMBER)], [CHK_BG_LAYER, CHK_NUMBER, CHK_NUMBER, createRangeChecker(0,1023), CHK_PALETTE, CHK_BOOL, CHK_BOOL]] } },
	BGFILL: { exprs: { multiple: true, arity: [6,9], types: [[CHK_BG_LAYER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, createRegexChecker(/^(?:[0-9A-F]{4})*$/, TYPE_NUMBER)], [CHK_BG_LAYER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, createRangeChecker(0,1023), CHK_PALETTE, CHK_BOOL, CHK_BOOL]] } },
	BGREAD: { exprs: { arity: [3], types: [CHK_BG_LAYER, CHK_NUMBER, CHK_NUMBER] }, vars: { multiple: true, arity: [1,4], types: [[CHK_ANY_VAR], [CHK_NUMBER_VAR, CHK_NUMBER_VAR, CHK_NUMBER_VAR, CHK_NUMBER_VAR]] } },
	GPAGE: { exprs: { arity: [1,3], types: [createRangeChecker(0,1), CHK_GRP_PAGE, CHK_GRP_PAGE] } },
	GCLS: { exprs: { arity: [0,1], types: [CHK_COLOR] } },
	GCOLOR: { exprs: { arity: [1], types: [CHK_COLOR] } },
	GPSET: { exprs: { arity: [2,3], types: [CHK_GRP_WIDTH, CHK_GRP_HEIGHT, CHK_COLOR] } },
	GPAINT: { exprs: { arity: [2,3,4], types: [CHK_GRP_WIDTH, CHK_GRP_HEIGHT, CHK_COLOR, CHK_COLOR] } },
	GLINE: { exprs: { arity: [4,5], types: [CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_COLOR] } },
	GBOX: { exprs: { arity: [4,5], types: [CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_COLOR] } },
	GFILL: { exprs: { arity: [4,5], types: [CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_COLOR] } },
	GCIRCLE: { exprs: { arity: [3,4,6], types: [CHK_NUMBER, CHK_NUMBER, createMinChecker(1,true), createRangeChecker(0,255,false,TYPE_NULL), CHK_NUMBER, CHK_NUMBER] } },
	GPUTCHR: { exprs: { arity: [6], types: [CHK_NUMBER, CHK_NUMBER, CHK_CHR_NAME, CHK_CHR_NUMBER, CHK_PALETTE, createSelectChecker([1,2,4,8])] } },
	GDRAWMD: { exprs: { arity: [1], types: [CHK_BOOL] } },
	GPRIO: { exprs: { arity: [1], types: [CHK_GRP_PAGE] } },
	GCOPY: { exprs: { arity: [7,8], types: [CHK_GRP_PAGE, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NUMBER] } },
	BEEP: { exprs: { arity: [0,1,2,3,4], types: [createRangeChecker(0,263,false,TYPE_NULL), createRangeChecker(-8192,8192,false,TYPE_NULL), createRangeChecker(0,127,true,TYPE_NULL), createRangeChecker(0,127)] } },
	BGMSTOP: { exprs: { arity: [0,1,2], types: [CHK_BGM_TRACK, CHK_NUMBER] } },
	BGMVOL: { exprs: { multiple: true, arity: [1,2], types: [[createRangeChecker(0,127)], [CHK_BGM_TRACK, createRangeChecker(0,127)]] } },
	BGMSET: { exprs: { arity: [2,3,4,5,6,7,8,9,10], types: [createRangeChecker(128,255), CHK_STRING] } },
	BGMSETD: { exprs: { arity: [2], types: [createRangeChecker(128,255), CHK_LABEL_STR] } },
	BGMCLEAR: { exprs: { arity: [0,1], types: [createRangeChecker(128,255)] } },
	BGMSETV: { exprs: { arity: [3], types: [CHK_BGM_TRACK, createRangeChecker(0,7), createRangeChecker(0,255)] } },
	BGMPRG: { exprs: { arity: [6], types: [createRangeChecker(224,255), createRangeChecker(0,127), createRangeChecker(0,127), createRangeChecker(0,127), createRangeChecker(0,127), createRegexChecker(/^(?:[0-9A-F]{128}){1,2}$/)] } },
	TALK: { exprs: { arity: [1,2,3,4,5,6,7,8,9], types: [CHK_STRING] } },
	TALKSTOP: { exprs: { arity: [0], types: [] } },
	PNLTYPE: { exprs: { arity: [1], types: [createRegexChecker(/^(?:OFF|PNL|KYA|KYM|KYK)$/)] } },
	PNLSTR: { exprs: { arity: [3,4], types: [CHK_CONSOLE_WIDTH, CHK_CONSOLE_HEIGHT, CHK_STRING, CHK_CONSOLE_COLOR] } },
	ICONSET: { exprs: { arity: [2], types: [createRangeChecker(0,3), createRangeChecker(0,63)] } },
	ICONCLR: { exprs: { arity: [0,1], types: [createRangeChecker(0,3)] } }
};
 
// List of number and type of arguments
var expectedFunction = {
	INKEY$: { arity: [0], types: [] },
	BUTTON: { arity: [0,1], types: [createRangeChecker(0,3)] },
	CHKCHR: { arity: [2], types: [CHK_CONSOLE_WIDTH, CHK_CONSOLE_HEIGHT] },
	BTRIG: { arity: [0], types: [] },
	SPCHK: { arity: [1], types: [CHK_SP_NUMBER] },
	SPGETV: { arity: [2], types: [CHK_SP_NUMBER, createRangeChecker(0,7)] },
	SPHITSP: { arity: [2], types: [CHK_SP_NUMBER, CHK_SP_NUMBER] },
	SPHIT: { arity: [1,2], types: [CHK_SP_NUMBER, CHK_SP_NUMBER] },
	SPHITRC: { arity: [5,7], types: [CHK_SP_NUMBER, CHK_NUMBER, CHK_NUMBER, CHK_NONNEGATIVE, CHK_NONNEGATIVE, CHK_NUMBER, CHK_NUMBER] },
	BGCHK: { arity: [1], types: [CHK_BG_LAYER] },
	GSPOIT: { multiple: true, arity: [2,3], types: [[CHK_GRP_WIDTH, CHK_GRP_HEIGHT], [CHK_GRP_PAGE, CHK_GRP_WIDTH, CHK_GRP_HEIGHT]] },
	BGMCHK: { arity: [0,1], types: [CHK_BGM_TRACK] },
	BGMGETV: { arity: [2], types: [CHK_BGM_TRACK, createRangeChecker(0,7)] },
	TALKCHK: { arity: [0], types: [] },
	ICONCHK: { arity: [0], types: [] },
	ASC: { arity: [1], types: [CHK_STRING] },
	CHR$: { arity: [1], types: [CHK_CHR_NUMBER] },
	VAL: { arity: [1], types: [CHK_STRING] },
	STR$: { arity: [1], types: [CHK_NUMBER] },
	HEX$: { arity: [1,2], types: [CHK_NUMBER, createRangeChecker(1,5)] },
	MID$: { arity: [3], types: [CHK_STRING, CHK_NONNEGATIVE, CHK_NONNEGATIVE] },
	LEN: { arity: [1], types: [CHK_STRING] },
	LEFT$: { arity: [2], types: [CHK_STRING, CHK_NONNEGATIVE] },
	RIGHT$: { arity: [2], types: [CHK_STRING, CHK_NONNEGATIVE] },
	INSTR: { multiple: true, arity: [2,3], types: [[CHK_STRING, CHK_STRING], [CHK_NUMBER, CHK_STRING, CHK_STRING]] },
	SUBST$: { multiple: true, arity: [3,4], types: [[CHK_STRING, CHK_NONNEGATIVE, CHK_STRING], [CHK_STRING, CHK_NONNEGATIVE, CHK_NUMBER, CHK_STRING]] },
	RND: { arity: [1], types: [CHK_NUMBER] },
	ABS: { arity: [1], types: [CHK_NUMBER] },
	SGN: { arity: [1], types: [CHK_NUMBER] },
	PI: { arity: [0], types: [] },
	RAD: { arity: [1], types: [createRangeChecker(0,360)] },
	DEG: { arity: [1], types: [CHK_NUMBER] },
	SIN: { arity: [1], types: [CHK_NUMBER] },
	COS: { arity: [1], types: [CHK_NUMBER] },
	TAN: { arity: [1], types: [CHK_NUMBER] },
	ATAN: { arity: [1,2], types: [CHK_NUMBER, CHK_NUMBER] },
	SQR: { arity: [1], types: [CHK_NONNEGATIVE] },
	EXP: { arity: [1], types: [CHK_NUMBER] },
	LOG: { arity: [1], types: [new TypeChecker(TYPE_NUMBER, function(expr, msgs) {
		expr = findAssignedValue(expr);
		if (expr.type == 'number' && expr.val <= 0) {
			msgs.push(new SBParser.Error(expr.pos, 'Positive number required.'));
			return false;
		}
		return true;
	})] },
	FLOOR: { arity: [1], types: [CHK_NUMBER] },
	POW: { arity: [2], types: [CHK_NUMBER, CHK_NUMBER] },
	NOT: { arity: [1], types: [CHK_NUMBER] },
	AND: { arity: [2], types: [CHK_NUMBER, CHK_NUMBER] },
	OR: { arity: [2], types: [CHK_NUMBER, CHK_NUMBER] },
	XOR: { arity: [2], types: [CHK_NUMBER, CHK_NUMBER] },
	'-': { arity: [1,2], types: [CHK_NUMBER, CHK_NUMBER] },
	'/': { arity: [2], types: [CHK_NUMBER, CHK_NUMBER] },
	'%': { arity: [2], types: [CHK_NUMBER, CHK_NUMBER] },
	'!': { arity: [1], types: [CHK_NUMBER] }
};
 
function checkType(stmts, msgs) {
	forEachStmt(stmts, function(stmt) {
		checkTypeStmt(stmt, msgs);
	});
}
 
function checkTypeStmt(stmt, msgs) {
	// Assignment types on both sides must be the same
	if (stmt.name == '=') {
		checkTypeExpr(stmt.vars[0], CHK_ANY_VAR, msgs);
		checkTypeExpr(stmt.exprs[0], new TypeChecker(calcType(stmt.vars[0])), msgs);
	}
	// SWAP arguments must be the same type
	else if (stmt.name == 'SWAP') {
		var checker = createVariableChecker((stmt.exprs.length > 0) ? calcType(stmt.exprs[0]) : TYPE_ANY);
		checkTypeArgs('Command SWAP', stmt.exprs, { arity: [2], types: [checker, checker] }, stmt.pos, msgs);
	}
	// BGMPLAY (is) complex
	else if (stmt.name == 'BGMPLAY') {
		var type1 = (stmt.exprs.length > 0) ? calcType(stmt.exprs[0]) : TYPE_ANY;
		var type2 = (stmt.exprs.length > 1) ? calcType(stmt.exprs[1]) : TYPE_ANY;
		if (type1 == TYPE_STRING) {
			checkTypeArgs('Command BGMPLAY', stmt.exprs, { arity: [1,2,3,4,5,6,7,8,9], types: [CHK_STRING] }, stmt.pos, msgs);
			checkMml(stmt.exprs, msgs);
		}
		else if (type2 == TYPE_STRING) {
			checkTypeArgs('Command BGMPLAY', stmt.exprs, { arity: [2,3,4,5,6,7,8,9], types: [createRangeChecker(0,0), CHK_STRING] }, stmt.pos, msgs);
			checkMml(stmt.exprs.slice(1), msgs);
		}
		else if (stmt.exprs.length < 2)
			checkTypeArgs('Command BGMPLAY', stmt.exprs, { arity: [1], types: [CHK_BGM_NUMBER] }, stmt.pos, msgs);
		else
			checkTypeArgs('Command BGMPLAY', stmt.exprs, { arity: [2,3], types: [CHK_BGM_TRACK, CHK_BGM_NUMBER, createRangeChecker(0,127)] }, stmt.pos, msgs);
	}
	// Otherwise normal processing
	else {
		var expected = expectedCommand[stmt.name];
		if (stmt.name == 'SPSET' && stmt.exprs.length >= 8) {
			var expr1 = findAssignedValue(stmt.exprs[6]), expr2 = findAssignedValue(stmt.exprs[7]);
			if (expr1.type == 'number' && expr2.type == 'number') {
				var val1 = Math.floor(expr1.val), val2 = Math.floor(expr2.val);
				if (val1 == 64 && (val2 == 8 || val2 == 16) || val2 == 64 && (val1 == 8 || val1 == 16))
					msgs.push(new SBParser.Error(stmt.pos, '' + val1 + 'x' + val2 + 'of the sprite can not be created.'));
			}
		}
		else if (stmt.name == 'BGMSET')
			checkMml(stmt.exprs.slice(1), msgs);
		else if (stmt.name == 'TALK')
			checkTalk(stmt.exprs, msgs);
		if (expected.exprs)
			checkTypeArgs('Command ' + stmt.name, stmt.exprs, expected.exprs, stmt.pos, msgs);
		if (expected.vars)
			checkTypeArgs('Command ' + stmt.name, stmt.vars, expected.vars, stmt.pos, msgs);
	}
}
 
function checkTypeArgs(name, args, expected, pos, msgs) {
	checkArity(name, args.length, expected.arity, pos, msgs);
	var types = expected.types;
	if (expected.multiple) {
		var i = 0;
		for (; i < expected.arity.length - 1; ++i)
			if (args.length <= expected.arity[i])
				break;
		types = types[i];
	}
	var rest = (typeof expected.arity == 'number') ? expected.types[expected.types.length-1] : new TypeChecker(TYPE_ANY | TYPE_LABEL | TYPE_NULL);
	for (var i = 0; i < args.length; ++i)
		checkTypeExpr(args[i], types[i] || rest, msgs)
}
 
function checkArity(name, arity, expected, pos, msgs) {
	if (typeof expected == 'number') {
		if (arity >= expected)
			return;
		msgs.push(new SBParser.Error(pos, name + ' Not enough arguments (' + expected + 'or more).'));
	}
	else {
		for (var i = 0; i < expected.length; ++i)
			if (arity == expected[i])
				return;
		var range;
		if (expected.length == 1)
			range = '(' + expected[0] + ')';
		else {
			var cont = true;
			for (var i = 1; i < expected.length; ++i)
				if (expected[i-1] + 1 != expected[i]) {
					cont = false;
					break;
				}
			range = '(' + ((cont) ? expected[0] + '-' + expected[expected.length-1] : expected.join(',')) + ')';
		}
		if (arity < expected[0])
			msgs.push(new SBParser.Error(pos, name + ' There are not enough arguments.' + range));
		else if (arity > expected[expected.length-1])
			msgs.push(new SBParser.Error(pos, name + ' There are too many arguments.' + range));
		else
			msgs.push(new SBParser.Error(pos, name + ' The number of arguments is incorrect.' + range));
	}
}
 
function checkTypeExpr(expr, checker, msgs) {
	// If type checking goes normally, only perform checks on arguments to prevent detecting multiple errors.
	var type = calcType(expr);
	if (!checker.check(expr, type, msgs))
		return;
	if (!expr.args)
		return;
	if (expr.type == 'array') {
		checkTypeArgs('Array ' + expr.name, expr.args, { arity: [1,2], types: [CHK_NONNEGATIVE, CHK_NONNEGATIVE] }, expr.pos, msgs);
		return;
	}
	var expected = expectedFunction[expr.name];
	// Comparison and addition of argument type is the same
	if (/[=<>+]/.test(expr.name)) {
		var checker = new TypeChecker(calcType(expr.args[0]));
		expected = { arity: [2], types: [checker, checker] };
	}
	else if (expr.name == '*')
		expected = { arity: [2], types: (type == TYPE_NUMBER) ? [CHK_NUMBER, CHK_NUMBER] : [CHK_STRING, createMinChecker(0,true)] };
	else if (expr.name == 'POW' && expr.args.length >= 2) {
		var expr1 = findAssignedValue(expr.args[0]), expr2 = findAssignedValue(expr.args[1]);
		if (expr1.type == 'number' && expr2.type == 'number') {
			var val1 = expr1.val, val2 = expr2.val;
			if (val1 < 0 && val2 != Math.floor(val2))
				msgs.push(new SBParser.Error(expr.pos, 'When base is negative exponent must be an integer.'));
			else if (val2 < 0 && val1 == 0)
				msgs.push(new SBParser.Error(expr.pos, 'You can not have negative exponents for a 0 base. '));
		}
	}
	checkTypeArgs('Function ' + expr.name, expr.args, expected, expr.pos, msgs);
}
 
// Finds data type
function calcType(expr) {
	var type;
	switch (expr.type) {
	case 'null':
		return TYPE_NULL;
	case 'number':
		return TYPE_NUMBER;
	case 'string':
	case ';':
	case ',':
		return TYPE_STRING;
	case 'label':
		return TYPE_LABEL;
	default:
		if (/^[+*]$/.test(expr.name))
			return calcType(expr.args[0]);
		else
		// If it contains "$" or alphanumerics otherwise.
			return (/\$/.test(expr.name)) ? TYPE_STRING : TYPE_NUMBER;
	}
}
 
function checkDeadCode(stmts, msgs) {
	for (var i = 1; i < stmts.length; ++i) {
		// Is not performed immediately after GOTO,RETURN,END,BREAK,CONTINUE
		if (isEndStmt(stmts[i-1])) {
			// No need to run DATA if it is a GOTO destination or label/function declaration
			if (!/^(?:@|DATA|DEF|COMMONDEF)/.test(stmts[i].name)) {
				msgs.push(new SBParser.Warning(stmts[i].pos, 'Command ' + stmts[i].name + ' will never be executed.'));
			}
		}
	}
	for (var i = 0; i < stmts.length; ++i) {
		if (stmts[i].stmts)
			checkDeadCode(stmts[i].stmts, msgs);
		if (stmts[i].stmts2)
			checkDeadCode(stmts[i].stmts2, msgs);
	}
}
 
function checkForNext(stmts, msgs) {
	var stack = new Array();
	var checkEnd = function() {
		for (var j = 0; j < stack.length; ++j)
			msgs.push(new SBParser.Warning(stack[j].pos, 'Corresponding NEXT missing for FOR'));
		stack = new Array();
	};
	for (var i = 0; i < stmts.length; ++i) {
		if (stmts[i].name == 'FOR')
			stack.push(stmts[i]);
		else if (stmts[i].name == 'NEXT') {
			// Does not check FOR because there is a possibility that GOTO is used for a block IF
			if (stack.length > 0) {
				var stmt = stack.pop();
				if (stmts[i].vars.length > 0 && stmts[i].vars[0].name != stmt.vars[0].name)
					msgs.push(new SBParser.Warning(stmts[i].pos, 'FOR and NEXT are using different variable names. (' + stmt.vars[0].name + ',' + stmts[i].vars[0].name + ')'));
			}
		}
		// Does not check NEXT because it is possible that GOTO is used to hold a block IF
		else if (stmts[i].name == 'GOTO')
			stack = new Array();
		else if (isEndStmt(stmts[i]))
			checkEnd();
	}
	checkEnd();
}
 
function isEndStmt(stmt) {
	if (/^(?:GOTO|RETURN|END|BREAK|CONTINUE)$/.test(stmt.name))
		return true;
	// Regards the end of a command when there is a termination to both branches(?)
	// Check only the last instruction since there is unreachable code
	else if (stmt.name == 'IF' && stmt.stmts2 && stmt.stmts.length > 0 && stmt.stmts2.length > 0 &&
			 isEndStmt(stmt.stmts[stmt.stmts.length-1]) && isEndStmt(stmt.stmts2[stmt.stmts2.length-1]))
		return true;
	else
		return false;
}
 
function forEachStmt(stmts, func) {
	for (var i = 0; i < stmts.length; ++i) {
		func(stmts[i]);
		if (stmts[i].stmts)
			forEachStmt(stmts[i].stmts, func);
		if (stmts[i].stmts2)
			forEachStmt(stmts[i].stmts2, func);
	}
}
 
function setTextAreaCursorPos(textArea, pos) {
	var cnt = pos.cnt + pos.col - 1;
	if (textArea.setSelectionRange)
	{
		textArea.setSelectionRange(cnt, cnt);
		textArea.focus();
	}
	else if (textArea.createTextRange) {
		cnt -= pos.row - 1;
		var range = textArea.createTextRange();
		range.collapse(true);
		range.moveEnd('character', cnt);
		range.moveStart('character', cnt);
		range.select();
	}
}
 
function mmlChecker(min, max, canVar) {
	if (canVar)
		return function(cmd, arg, msgs) {
			if (arg.type == 'number') {
				if (arg.val < min)
					msgs.push(new SBParser.Error(arg.pos, cmd + ' Number is too small. (' + min + '-' + max + ')'));
				else if (max < arg.val)
					msgs.push(new SBParser.Error(arg.pos, cmd + ' Number is too large. (' + min + '-' + max + ')'));
			}
			else
				if (arg.val > 7)
					msgs.push(new SBParser.Error(arg.pos, cmd + ' Variable is too large. (0-7)'));
		};
	else
		return function(cmd, arg, msgs) {
			if (arg.type == 'number') {
				if (arg.val < min)
					msgs.push(new SBParser.Error(arg.pos, cmd + ' Number too small. (' + min + '-' + max + ')'));
				else if (max < arg.val)
					msgs.push(new SBParser.Error(arg.pos, cmd + ' Number too large. (' + min + '-' + max + ')'));
			}
			else
				msgs.push(new SBParser.Error(arg.pos, cmd + ' Number required.'));
		};
}
function mmlLabelChecker(cmd, arg, msgs) {
	if (arg.val.length > 8)
		msgs.push(new SBParser.Error(arg.pos, cmd + ' The name of the label ' + arg.val + ' is too long.'));
}
 
var expectedMml = {
	':': { arity: [1], args: [mmlChecker(0,15)] },
	T: { arity: [1], args: [mmlChecker(1,512)] },
	'=': { arity: [2], args: [mmlChecker(0,0,true), mmlChecker(-128,255,true)] },
	L: { arity: [1], args: [mmlChecker(1,192)] },
	Q: { arity: [1], args: [mmlChecker(0,8)] },
	'&': { arity: [0], args: [] },
	R: { arity: [0,1], args: [mmlChecker(1,192,true)], dotted: true },
	N: { arity: [1], args: [mmlChecker(0,127,true)] },
	O: { arity: [1], args: [mmlChecker(0,8)] },
	'<': { arity: [0], args: [] },
	'>': { arity: [0], args: [] },
	'@D': { arity: [1], args: [mmlChecker(-128,127,true)] },
	'_': { arity: [0], args: [] },
	V: { arity: [1], args: [mmlChecker(0,127,true)] },
	'(': { arity: [0,1], args: [mmlChecker(0,127)] },
	')': { arity: [0,1], args: [mmlChecker(0,127)] },
	P: { arity: [1], args: [mmlChecker(0,127,true)] },
	'@V': { arity: [1], args: [mmlChecker(0,127,true)] },
	'@': { arity: [1], args: [mmlChecker(0,511)] },
	'@E': { arity: [4], args: [mmlChecker(0,127),mmlChecker(0,127),mmlChecker(0,127),mmlChecker(0,127)] },
	'@ER': { arity: [0], args: [] },
	'[]': { arity: [0,1], args: [mmlChecker(0,255,true)] },
	'{=}': { arity: [1], args: [mmlLabelChecker] },
	'{}': { arity: [1], args: [mmlLabelChecker] },
	'@MON': { arity: [0], args: [] },
	'@MOF': { arity: [0], args: [] },
	'@MA': { arity: [4], args: [mmlChecker(0,127),mmlChecker(0,127),mmlChecker(0,127),mmlChecker(0,127)] },
	'@MP': { arity: [4], args: [mmlChecker(0,127),mmlChecker(0,127),mmlChecker(0,127),mmlChecker(0,127)] }
};
(function() {
	var chars = 'CDEFGAB';
	var checker = { arity: [0,1], args: [mmlChecker(1,192,true)], dotted: true };
	for (var i = 0; i < chars.length; ++i) {
		var c = chars.charAt(i);
		expectedMml[c] = expectedMml[c+'#'] = expectedMml[c+'+'] = expectedMml[c+'-'] = checker;
	}
})();
function checkMml(strs, msgs) {
	var strs2 = new Array();
	for (var i = 0; i < strs.length; ++i ) {
		var str = findAssignedValue(strs[i]);
		if (str.type != 'string')
			return;
		strs2.push(str);
	}
	checkMmlCmds(parseMml(strs2, msgs), msgs);
}
function checkMmlCmds(cmds, msgs) {
	for (var i = 0; i < cmds.length; ++i) {
		var expected = expectedMml[cmds[i].name];
		checkArity('MML ' + cmds[i].name, cmds[i].args.length, expected.arity, cmds[i].pos, msgs);
		for (var j = 0; j < Math.min(cmds[i].args.length, expected.args.length); ++j)
			expected.args[j]('MML', cmds[i].args[j], msgs);
		if (cmds[i].dots && !expected.dotted)
			msgs.push(new SBParser.Error(cmds[i].dots.pos, 'MML ' + cmds[i].name + ' breakpoint can not be specified here'));
		if (cmds[i].cmds)
			checkMmlCmds(cmds[i].cmds, msgs);
	}
}
 
function parseMml(strs, msgs) {
	var buf = new MmlBuffer(strs);
	buf.str = buf.str.toUpperCase();
	var cmds;
	try {
		cmds = parseMmlCmds(buf);
		if (!buf.isEOF())
			msgs.push(new SBParser.Error(buf.pos, 'MML contains an error.'));
	}
	catch (e) {
		if (e instanceof SBParser.Error)
			msgs.push(e);
		else
			throw e;
		cmds = new Array();
	}
	return cmds;
}
function parseMmlCmds(buf) {
	var cmds = new Array();
	for (;;) {
		buf.skip(/^\s*/);
		var pos = buf.pos;
		var match = buf.match(/^(?:[\[:TLQ&RNO<>_V()P]|[CDEFGAB][#+-]?|\$([0-9]+)=|@(?:D|V|ER?|MON|MOF|MA|MP)?|{([A-Z_][A-Z0-9_]*)=|{([A-Z_][A-Z0-9_]*)})/);
		if (!match)
			break;
		var cmd = { name: match[0], pos: pos, args: new Array() };
		if (match[0] == '[') {
			cmd.name = '[]';
			cmd.cmds = parseMmlInner(buf, /^\]/);
		}
		else if (match[1]) {
			cmd.name = '=';
			cmd.pos = new SBParser.Position(pos.cnt, pos.row, pos.col + match[0].length - 1);
			cmd.args.push({ type: 'variable', val: parseInt(match[1], 10), pos: pos });
		}
		else if (match[2]) {
			cmd.name = '{=}';
			cmd.args.push({ type: 'label', val: match[2], pos: new SBParser.Position(pos.cnt, pos.row, pos.col + 1) });
			cmd.cmds = parseMmlInner(buf, /^}/);
		}
		else if (match[3]) {
			cmd.name = '{}';
			cmd.args.push({ type: 'label', val: match[3], pos: new SBParser.Position(pos.cnt, pos.row, pos.col + 1) });
		}
		var num = parseMmlNumOrVar(buf);
		if (num) {
			cmd.args.push(num);
			while (buf.match(/^,/)) {
				num = parseMmlNumOrVar(buf);
				if (!num)
					throw new SBParser.Error(buf.pos, 'MML argument is required.');
				cmd.args.push(num);
			}
		}
		pos = buf.pos;
		match = buf.match(/^\.+/);
		if (match)
			cmd.dots = { pos: pos, val: match[0].length };
		cmds.push(cmd);
	}
	return cmds;
}
function parseMmlInner(buf, end) {
	var inner = parseMmlCmds(buf);
	if (buf.isEOF())
		throw new SBParser.Error(buf.pos, 'MML parentheses are open.');
	if (!buf.match(end))
		throw new SBParser.Error(buf.pos, 'MML is incorrect.');
	return inner;
}
function parseMmlNumOrVar(buf) {
	var pos = buf.pos;
	var match = buf.match(/^(-?[0-9]+)|^\$([0-9]+)(?!=)/);
	if (!match)
		return false;
	else if (match[1] !== undefined)
		return { type: 'number', val: parseInt(match[1], 10), pos: pos };
	else
		return { type: 'variable', val: parseInt(match[2], 10), pos: pos };
}
 
var expectedTalk = {
	'@H': { arity: [1], args: [mmlChecker(0,3)] },
	'@N': { arity: [1], args: [mmlChecker(0,3500)] },
	'@T': { arity: [1], args: [mmlChecker(0,1000)] },
	'@V': { arity: [1], args: [mmlChecker(0,80)] },
	'@S': { arity: [1], args: [mmlChecker(0,11)] },
	'@E': { arity: [1], args: [mmlChecker(0,16)] },
};
(function() {
	// Because it would change the position and degrade the sound in kana and voice、I intentionally leave the sound
	var chars = "ヲァィゥェォャュョッアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜ガギグゲゴザジズゼゾダヂヅデドバビブベボパピプペポ'/|_.?!% -";
	var checker = { arity: [0], args: [] };
	for (var i = 0; i < chars.length; ++i) {
		var c = chars.charAt(i);
		expectedTalk[c] = checker;
	}
})();
function checkTalk(strs, msgs) {
	for (var i = 0; i < strs.length; ++i )
		if (strs[i].type != 'string')
			return;
	var cmds = parseTalk(strs, msgs);
	for (var i = 0; i < cmds.length; ++i) {
		if (cmds[i].name == 'ー') {
			msgs.push(new SBParser.Error(cmds[i].pos, 'TALK macron "ー" can not be used。Please use "-" (minus)。'));
			continue;
		}
		var expected = expectedTalk[cmds[i].name];
		checkArity('TALK ' + cmds[i].name, cmds[i].args.length, expected.arity, cmds[i].pos, msgs);
		for (var j = 0; j < Math.min(cmds[i].args.length, expected.args.length); ++j)
			expected.args[j]('TALK', cmds[i].args[j], msgs);
	}
}
 
function parseTalk(strs, msgs) {
	var buf = new MmlBuffer(strs);
	buf.str = changeKana(buf.str);
	var cmds = new Array();
	for (;;) {
		// Not skipped, blank space has meaning
		var pos = buf.pos;
		var match = buf.match(/^(?:[ヲァィゥェォャュョッアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜ガギグゲゴザジズゼゾダヂヅデドバビブベボパピプペポー'\/|_.?!% -]|@[HNTVSE])/);
		if (!match)
			break;
		var cmd = { name: match[0], pos: pos, args: new Array() };
		pos = buf.pos;
		match = buf.match(/^[0-9]+/);
		if (match)
			cmd.args.push({ type: 'number', val: parseInt(match[0], 10), pos: pos });
		cmds.push(cmd);
	}
	if (!buf.isEOF())
		msgs.push(new SBParser.Error(buf.pos, 'TALK is incorrect.'));
	return cmds;
}
 
function MmlBuffer(strs) {
	this.str = "";
	for (var i = 0; i < strs.length; ++i) {
		this.str += strs[i].val;
	}
	this.strs = strs.slice(0);
	this.strs.reverse();
	this.rest = 0;
	this.next();
}
MmlBuffer.prototype.isEOF = function() {
	return this.str.length <= 0;
};
MmlBuffer.prototype.skip = function(regex) {
	var match = regex.exec(this.str);
	if (match && match[0])
		this.shift(match[0].length);
};
MmlBuffer.prototype.match = function(regex) {
	var match = regex.exec(this.str);
	if (match)
		this.shift(match[0].length);
	return match;
};
MmlBuffer.prototype.shift = function(len) {
	this.str = this.str.substr(len)
	this.rest -= len;
	this.pos = new SBParser.Position(this.pos.cnt, this.pos.row, this.pos.col + len);
	while (this.rest <= 0 && !this.isEOF())
		this.next();
};
MmlBuffer.prototype.next = function() {
	if (this.isEOF())
		return;
	this.cur = this.strs.pop();
	this.pos = new SBParser.Position(this.cur.pos.cnt, this.cur.pos.row, this.cur.pos.col - this.rest + ((this.cur.noQuote) ? 0 : 1));
	this.rest += this.cur.val.length;
};
 
var changeKana = (function() {
	var orgChar = [
		'ｦを', 'ｧぁ', 'ｨぃ', 'ｩぅ', 'ｪぇ', 'ｫぉ', 'ｬゃ', 'ｭゅ', 'ｮょ', 'ｯっ', 'ｰ', 'ｱあ', 'ｲい', 'ｳう', 'ｴえ', 'ｵお',
		'ｶか', 'ｷき', 'ｸく', 'ｹけ', 'ｺこ', 'ｻさ', 'ｼし', 'ｽす', 'ｾせ', 'ｿそ', 'ﾀた', 'ﾁち', 'ﾂつ', 'ﾃて', 'ﾄと',
		'ﾅな', 'ﾆに', 'ﾇぬ', 'ﾈね', 'ﾉの', 'ﾊは', 'ﾋひ', 'ﾌふ', 'ﾍへ', 'ﾎほ', 'ﾏま', 'ﾐみ', 'ﾑむ', 'ﾒめ', 'ﾓも',
		'ﾔや', 'ﾕゆ', 'ﾖよ', 'ﾗら', 'ﾘり', 'ﾙる', 'ﾚれ', 'ﾛろ', 'ﾜわ', 'ﾝん', 'ﾞ', 'ﾟ',
		'が', 'ぎ', 'ぐ', 'げ', 'ご', 'ざ', 'じ', 'ず', 'ぜ', 'ぞ', 'だ', 'ぢ', 'づ', 'で', 'ど',
		'ば', 'び', 'ぶ', 'べ', 'ぼ', 'ぱ', 'ぴ', 'ぷ', 'ぺ', 'ぽ'
	];
 
	var newChar = [
		'ヲ', 'ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ャ', 'ュ', 'ョ', 'ッ', 'ー', 'ア', 'イ', 'ウ', 'エ', 'オ',
		'カ', 'キ', 'ク', 'ケ', 'コ', 'サ', 'シ', 'ス', 'セ', 'ソ', 'タ', 'チ', 'ツ', 'テ', 'ト',
		'ナ', 'ニ', 'ヌ', 'ネ', 'ノ', 'ハ', 'ヒ', 'フ', 'ヘ', 'ホ', 'マ', 'ミ', 'ム', 'メ', 'モ',
		'ヤ', 'ユ', 'ヨ', 'ラ', 'リ', 'ル', 'レ', 'ロ', 'ワ', 'ン', '゛', '゜',
		'ガ', 'ギ', 'グ', 'ゲ', 'ゴ', 'ザ', 'ジ', 'ズ', 'ゼ', 'ゾ', 'ダ', 'ヂ', 'ヅ', 'デ', 'ド',
		'バ', 'ビ', 'ブ', 'ベ', 'ボ', 'パ', 'ピ', 'プ', 'ペ', 'ポ'
	];
 
	var re_org = new RegExp('[' + orgChar.join('') +']', 'g');
	var org2new = new Object();
	for (var i = 0; i < orgChar.length; i++) {
		for (var j = 0; j < orgChar[i].length; ++j) {
			org2new[orgChar[i].charAt(j)] = newChar[i];
		}
	}
	function changeKana(str) {
		return str.replace(re_org, function(m0) {
			return org2new[m0];
		});
	}
	return changeKana;
})();
 
function checkBgmsetdMml(stmts, varInfo, msgs) {
	forEachStmt(stmts, function(stmt) {
		if (!(stmt.name == 'BGMSETD' && stmt.exprs.length >= 2))
			return;
		var expr = findAssignedValue(stmt.exprs[1]);
		var name;
		if (expr.type == 'label')
			name = expr.name;
		else if (expr.type == 'string')
			name = expr.val;
		else
			return;
		var l = varInfo.labels[name];
		if (!(l && l.def && l.def.length == 1))
			return;
		var idx = findPos(stmts, l.def[0]);
		if (!(idx < stmts.length && stmts[idx].name == '@'))
			return;
		var strs = new Array();
		for (var i = idx; i < stmts.length; ++i) {
			if (stmts[i].name != 'DATA')
				continue;
			for (var j = 0; j < stmts[i].exprs.length; ++j) {
				if (stmts[i].exprs[j].val == '0' && stmts[i].exprs[j].noQuote) {
					checkMml(strs, msgs);
					return;
				}
				strs.push(stmts[i].exprs[j]);
			}
		}
		msgs.push(new SBParser.Warning(stmt.pos, 'MML command BGMSETD does not end'));
		checkMml(strs, msgs);
	});
}
 
function findPos(stmts, pos) {
	var l = 0, r = stmts.length - 1;
	while( l <= r ) {
		var m = Math.floor((l + r) / 2);
		var c = SBParser.comparePosition(stmts[m].pos, pos);
		if (c < 0)
			l = m + 1;
		else if (c > 0)
			r = m - 1;
		else
			return m;
	}
	return r + 1;
}
 
function addLastVarDef(stmts, env) {
	env = env || new Object();
	for (var i = 0; i < stmts.length; ++i) {
		var stmt = stmts[i];
		if (stmt.exprs) {
			var eLen = stmt.exprs.length;
			if (stmt.name == 'FOR')
				eLen = Math.min(1, eLen);
			else if (/R?SORT$/.test(stmt.name))
				eLen = 0;
			for (var j = 0; j < eLen; ++j)
				addLastVarDefExpr(stmt.exprs[j], env);
		}
		if (stmt.vars)
			for (var j = 0; j < stmt.vars.length; ++j)
				if (stmt.vars[j].args)
					for (var k = 0; k < stmt.vars[j].args.length; ++k)
						addLastVarDefExpr(stmt.vars[j].args[k], env);
		if (/^(?:@|ONGOSUB|GOSUB|GOTO|BREAK|CONTINUE|END|STOP|CALL|RETURN)$/.test(stmt.name))
			clearObject(env);
		else if (!/^(?:=|VAR|DIM)$/.test(stmt.name)) {
			for (var j = 0; j < stmt.vars.length; ++j) {
				var v = stmt.vars[j];
				if (v.type == 'variable')
					delete env[v.name];
			}
		}
		else if (stmt.name == '=') {
			if (stmt.vars[0].type == 'variable')
				env[stmt.vars[0].name] = stmt.exprs[0];
		}
		else if (stmt.name == 'SWAP') {
			if (stmt.exprs.length >= 2 && stmt.exprs[0].type == 'variable' && stmt.exprs[1].type == 'variable') {
				env[stmt.exprs[1].name] = stmt.exprs[0].val;
				env[stmt.exprs[0].name] = stmt.exprs[1].val;
			}
		}
		else if (stmt.name == 'IF') {
			var env2 = copyObject(env);
			if (stmt.stmts.length > 0 && isEndStmt(stmt.stmts[stmt.stmts.length-1])) {
				addLastVarDef(stmt.stmts, env2);
				addLastVarDef(stmt.stmts2 || [], env);
			}
			else if (stmt.stmts2 && stmt.stmts2.length > 0 && isEndStmt(stmt.stmts2[stmt.stmts2.length-1])) {
				addLastVarDef(stmt.stmts, env);
				addLastVarDef(stmt.stmts2, env2);
			}
			else {
				addLastVarDef(stmt.stmts, env);
				addLastVarDef(stmt.stmts2 || [], env2);
				if (!isSameObject(env, env2))
					clearObject(env);
			}
		}
		else if (/^(?:DEF|COMMONDEF)$/.test(stmt.name))
			addLastVarDef(stmt.stmts, copyObject(env));
		else if (/^(?:FOR|WHILE|REPEAT)$/.test(stmt.name)) {
			if (stmt.name == 'FOR' && stmt.vars[0].type == 'variable')
				delete env[stmt.vars[0].name];
			var env2 = copyObject(env);
			addLastVarDef(stmt.stmts, env);
			if (!isSameObject(env, env2))
				clearObject(env);
		}
	}
}
function addLastVarDefExpr(expr, env) {
	if (expr.type == 'variable' && env[expr.name] !== undefined)
		expr.val = env[expr.name];
	if (expr.args)
		for (var i = 0; i < expr.args.length; ++i)
			addLastVarDefExpr(expr.args[i], env);
}
 
function clearObject(obj) {
	var props = new Array();
	for (var prop in obj)
		props.push(prop);
	for (var i = 0; i < props.length; ++i)
		delete obj[props[i]];
}
function copyObject(obj) {
	var copy = new Object();
	for (var prop in obj)
		copy[prop] = obj[prop];
	return copy;
}
function isSameObject(obj1, obj2) {
	for (var prop in obj1)
		if (obj1[prop] !== obj2[prop])
			return false;
	for (var prop in obj2)
		if (obj2[prop] !== obj1[prop])
			return false;
	return true;
}
function findAssignedValue(expr) {
	while (expr.type == 'variable' && expr.val)
		expr = expr.val;
	return expr;
}
var msgs, labels, vars;
	function select() {
		var source = document.checker.source;
		source.focus();
		source.select();
	}
	function check() {
		try {
			document.checker.source.value = document.checker.source.value.replace(/\u200b/g, "");
			msgs = null;
			var ret = checkSource(document.checker.source.value);
			msgs = ret.msgs;
			labels = ret.labels;
			vars = ret.vars;
			var options = document.checker.msgs.options;
			if (msgs.length <= 0) {
				options.length = 1;
				options[0] = new Option("No errors found.", "0");
			}
			else {
				options.length = msgs.length;
				for (var i = 0; i < msgs.length; ++i)
					options[i] = new Option(msgs[i], i);
			}
			options = document.checker.labels.options;
			options.length = labels.length;
			for (var i = 0; i < labels.length; ++i)
				options[i] = new Option(labels[i].name, i);
			options = document.checker.vars.options;
			options.length = vars.length;
			for (var i = 0; i < vars.length; ++i)
				options[i] = new Option(vars[i].name, i);
			alert("Check complete.");
		}
		catch (e) {
			alert("Errors: " + e);
		}
	}
	function movePos() {
		var source = document.checker.source;
		var idx = document.checker.msgs.selectedIndex;
		if (!msgs || !msgs[idx] || !msgs[idx].pos || !source.setSelectionRange && !source.createTextRange) return;
		setTextAreaCursorPos(source, msgs[idx].pos);
	}
	function movePosLabels() {
		var source = document.checker.source;
		var idx = document.checker.labels.selectedIndex;
		if (!labels || !labels[idx] || !labels[idx].pos || !source.setSelectionRange && !source.createTextRange) return;
		setTextAreaCursorPos(source, labels[idx].pos);
	}
	function movePosVars() {
		var source = document.checker.source;
		var idx = document.checker.vars.selectedIndex;
		if (!vars || !vars[idx] || !vars[idx].pos || !source.setSelectionRange && !source.createTextRange) return;
		setTextAreaCursorPos(source, vars[idx].pos);
	}
 
 //slacker' pretty much wrote this whole script
 //but lumage won't give him admin
 //and he took all the credit
 var parentDiv = document.getElementById("sbchecker"),
    child = document.createElement("TEXTAREA"),
    childG = document.createElement("FORM"),
    style = document.createElement("STYLE");
 
 style.innerHTML = ".a{min-width: 200px;}.b{min-width: 100px;}";
 parentDiv.appendChild(style);
 
 childG.name = "checker";
 parentDiv.appendChild(childG);
 
 child.innerHTML="'SmileBASIC code here.";
 child.rows = 24;
 child.cols = 60;
 child.className = "lined";
 child.id = "source";
 child.name= "source"
 child.addEventListener("focus", function() { 
   //remove default text, if present 
   if (child.innerHTML == "'SmileBASIC code here.") { 
   child.innerHTML  = ""; 
 }});
 childG.appendChild(child);
 
 var child2A = document.createElement("BR");
 childG.appendChild(child2A);
 
 var child3 = document.createElement("INPUT");
 child3.type = "button";
 child3.value = "Check";
 childG.appendChild(child3);
 
 var child2B = document.createElement("BR");
 childG.appendChild(child2B);
 
 var child4 = document.createElement("SELECT");
 child4.name = "msgs";
 child4.size = 10;
 child4.setAttribute("class", "a");
 child4.innerHTML = '<option value="0">No problems found.</option>';
 childG.appendChild(child4);
 
 var child5 = document.createElement("SELECT");
 child5.name = "labels";
 child5.size = 10;
 child5.setAttribute("class", "b");
 
 childG.appendChild(child5);
 
 var child6 = document.createElement("SELECT");
 child6.name = "vars";
 child6.size = 10;
 child6.setAttribute("class", "b");
 
 childG.appendChild(child6);
 
  parentDiv.addEventListener("load", function(){select()}, false);
 child3.addEventListener("click", function(){check()}, false);
 child4.addEventListener("change", function(){movePos()}, false);
 child5.addEventListener("change", function(){movePosLabels()}, false);
 child6.addEventListener("change", function(){movePosVars()}, false);
 
 
 
 
/**
 * jQuery Lined Textarea Plugin 
 *   http://alan.blog-city.com/jquerylinedtextarea.htm
 *
 * Copyright (c) 2010 Alan Williamson
 * 
 * Version: 
 *    $Id: jquery-linedtextarea.js 464 2010-01-08 10:36:33Z alan $
 *
 * Released under the MIT License:
 *    http://www.opensource.org/licenses/mit-license.php
 * 
 * Usage:
 *   Displays a line number count column to the left of the textarea
 *   
 *   Class up your textarea with a given class, or target it directly
 *   with JQuery Selectors
 *   
 *   $(".lined").linedtextarea({
 *   	selectedLine: 10,
 *    selectedClass: 'lineselect'
 *   });
 *
 * History:
 *   - 2010.01.08: Fixed a Google Chrome layout problem
 *   - 2010.01.07: Refactored code for speed/readability; Fixed horizontal sizing
 *   - 2010.01.06: Initial Release
 *
 */
(function($) {
 
	$.fn.linedtextarea = function(options) {
 
		// Get the Options
		var opts = $.extend({}, $.fn.linedtextarea.defaults, options);
 
 
		/*
		 * Helper function to make sure the line numbers are always
		 * kept up to the current system
		 */
		var fillOutLines = function(codeLines, h, lineNo){
			while ( (codeLines.height() - h ) <= 0 ){
				if ( lineNo == opts.selectedLine )
					codeLines.append("<div class='lineno lineselect'>" + lineNo + "</div>");
				else
					codeLines.append("<div class='lineno'>" + lineNo + "</div>");
 
				lineNo++;
			}
			return lineNo;
		};
 
 
		/*
		 * Iterate through each of the elements are to be applied to
		 */
		return this.each(function() {
			var lineNo = 1;
			var textarea = $(this);
 
			/* Turn off the wrapping of as we don't want to screw up the line numbers */
			textarea.attr("wrap", "off");
			textarea.css({resize:'none'});
			var originalTextAreaWidth	= textarea.outerWidth();
 
			/* Wrap the text area in the elements we need */
			textarea.wrap("<div class='linedtextarea'></div>");
			var linedTextAreaDiv	= textarea.parent().wrap("<div class='linedwrap' style='width:" + originalTextAreaWidth + "px'></div>");
			var linedWrapDiv 			= linedTextAreaDiv.parent();
 
			linedWrapDiv.prepend("<div class='lines' style='width:50px'></div>");
 
			var linesDiv	= linedWrapDiv.find(".lines");
			linesDiv.height( textarea.height() + 6 );
 
 
			/* Draw the number bar; filling it out where necessary */
			linesDiv.append( "<div class='codelines'></div>" );
			var codeLinesDiv	= linesDiv.find(".codelines");
			lineNo = fillOutLines( codeLinesDiv, linesDiv.height(), 1 );
 
			/* Move the textarea to the selected line */ 
			if ( opts.selectedLine != -1 && !isNaN(opts.selectedLine) ){
				var fontSize = parseInt( textarea.height() / (lineNo-2) );
				var position = parseInt( fontSize * opts.selectedLine ) - (textarea.height()/2);
				textarea[0].scrollTop = position;
			}
 
 
			/* Set the width */
			var sidebarWidth					= linesDiv.outerWidth();
			var paddingHorizontal 		= parseInt( linedWrapDiv.css("border-left-width") ) + parseInt( linedWrapDiv.css("border-right-width") ) + parseInt( linedWrapDiv.css("padding-left") ) + parseInt( linedWrapDiv.css("padding-right") );
			var linedWrapDivNewWidth 	= originalTextAreaWidth - paddingHorizontal;
			var textareaNewWidth			= originalTextAreaWidth - sidebarWidth - paddingHorizontal - 20;
 
			textarea.width( textareaNewWidth );
			linedWrapDiv.width( linedWrapDivNewWidth );
 
 
 
			/* React to the scroll event */
			textarea.scroll( function(tn){
				var domTextArea		= $(this)[0];
				var scrollTop 		= domTextArea.scrollTop;
				var clientHeight 	= domTextArea.clientHeight;
				codeLinesDiv.css( {'margin-top': (-1*scrollTop) + "px"} );
				lineNo = fillOutLines( codeLinesDiv, scrollTop + clientHeight, lineNo );
			});
 
 
			/* Should the textarea get resized outside of our control */
			textarea.resize( function(tn){
				var domTextArea	= $(this)[0];
				linesDiv.height( domTextArea.clientHeight + 6 );
			});
 
		});
	};
 
  // default options
  $.fn.linedtextarea.defaults = {
  	selectedLine: -1,
  	selectedClass: 'lineselect'
  };
})(jQuery);
 
$(function() {
	$(".lined").linedtextarea(
		{selectedLine: 1}
	);
});
