module pegged.grammartester;

import std.stdio;
import std.array;

import pegged.testerparser;
import pegged.grammar;

struct GrammarTester(grammar, string startSymbol)
{
	bool soft = false;
	bool verbose = false;
	
	bool assertSimilar( string textToParse, string desiredTreeRepresentation )
	{
		auto treeGot = grammar.decimateTree(mixin("grammar."~startSymbol~"(textToParse)"));
		auto treeChecker = TesterGrammar.decimateTree(TesterGrammar.Root(desiredTreeRepresentation));
		
		writefln("%s",treeGot);
		writefln("");
		writefln("%s",treeChecker);
		writefln("");
		
		auto ctx = getDifferencer(treeGot, treeChecker);
		ctx.diff();
		ctx.doPrinting = true;
		ctx.diff();
		
		if ( !soft )
			assert(true);

		return true;
	}
	
	bool assertDifferent( string textToParse, string desiredTreeRepresentation )
	{
		assert(0,"Not implemented yet!");
	}
	
	void dumpAnyErrors()
	{
	}
	
	static auto getDifferencer(T,P)( auto ref T treeRoot, auto ref P patternRoot )
	{
		Differencer!(T,P) t;
		t.treeRoot = &treeRoot;
		t.patternRoot = &patternRoot;
		return t;
	}
	
	private struct Differencer(T,P)
	{
		int level = 0;
		int differences = 0;
		const(T)* treeRoot;
		const(P)* patternRoot;
		int max1stColumnWidth = 0;
		Appender!(string[]) leftColumn;
		Appender!(char[])   centerLine;
		Appender!(string[]) rightColumn;
		bool doPrinting = false;
		
		alias T TParseTree;
		alias P PParseTree;
	
		private void diff()
		{
			level = 0;
			differences = 0;
			
			leftColumn  = appender!(string[])();
			centerLine  = appender!(char[])();
			rightColumn = appender!(string[])();
			
			if ( doPrinting )
			{
				writefln("");
				printColumns("  Nodes Found",'|',"  Nodes Expected");
				printColumns("",             '|',"");
			}
			else
				max1stColumnWidth = "  Nodes Found".length;
			
			diffNode(treeRoot,patternRoot);
			
			if ( doPrinting )
				writefln("");
		}

		private void diffNode( const(T*) node, const(P*) pattern )
		{
			assert(pattern);
			switch ( pattern.name )
			{
				case "TesterGrammar.Root":
					diffNode(node, &pattern.children[0]);
					break;

				case "TesterGrammar.Node":

					lineDiff(node,pattern);

					// These will track which node children we have visited.
					int cursor = 0;
					bool[] visited = [];
					if ( node ) visited = new bool[node.children.length];
					foreach ( ref flag; visited ) flag = false;

					// This will handle nodes that we expect to find as
					//   children of this one.  If these nodes don't exist,
					//   then we throw out angry diff lines about them.
					string uniformBranchType = null;
					foreach ( branch; pattern.children )
					{
						// Enforce uniform branch types.
						if ( uniformBranchType == null )
							uniformBranchType = branch.name;
						else
						{
							auto branchType = branch.name;
							if ( branchType != uniformBranchType )
								throw new Exception(
									"It is forbidden to have both unordered and "~
									"ordered branches from the same node.");
						}
						
						// Note that we don't dereference the node's children:
						//   the pattern child will be a Branch statement and
						//   it will do the dereference for us.
						diffBranch(node, &branch, visited, cursor);
					}

					// This will handle nodes that the pattern doesn't expect
					//   to find, but that we find anyways.  Anything here
					//   deserves a nice angry diff line about the extra node.
					level++;
					if ( node ) foreach( i, childNode; node.children )
					{
						// TODO: rescanning the visit array this is probably slow.
						if ( visited[i] )
							continue;
						
						traverseUnexpected(&node.children[i]);
						visited[i] = true;
					}
					level--;

					break;

				default: assert(0, "Unexpected element in pattern: "~pattern.name);
			}
		}
		
		private void traverseUnexpected( const(T*) node )
		{
			assert(node);
			lineDiff(node, null);
			level++;
			foreach( child; node.children )
				traverseUnexpected(&child);
			level--;
		}
		
		private void diffBranch( const(T*) node, const(P*) pattern, bool[] visited, ref int cursor )
		{
			assert(pattern);
			switch ( pattern.name )
			{
				
				case "TesterGrammar.OrderedBranch":
				
					level++;
					if ( node ) foreach ( patternChild; pattern.children )
					{
						if ( cursor >= node.children.length )
						{
							diffNode(null, &patternChild);
						}
						else
						{
							diffNode(&node.children[cursor], &patternChild);
							visited[cursor] = true;
							cursor++;
						}
					}
					level--;
					
					break;

				case "TesterGrammar.UnorderedBranch":

					level++;
					if ( node ) foreach ( i, patternChild; pattern.children )
					{
						int foundIndex = -1;
						
						// Cheap hack for now: 0(n^2) scan-in-scan.
						//
						// Ideally the parser will generate hints for locating
						//   submatches:
						//     GrammarName.childLocationHint(
						//       GrammarName.ruleId!(RuleName),
						//       GrammarName.ruleId!(ChildRuleName));
						// Think about it... it should be possible to compile 
						//   this into integer table lookups, thus allowing us
						//   to do what a compiler does with an AST: vtable
						//   lookups followed by offset indexing to typed
						//   fields which are sometimes (typed) arrays.
						// At that point things stay nice and dynamic while
						//   being just as performant as oldschool methods.
						//
						foreach ( j, nodeChild; node.children )
						{
							// TODO: rescanning the visit array this is probably slow.
							if ( visited[j] )
								continue;

							if ( checkIdentifierMatch( &nodeChild, &patternChild ) )
							{
								foundIndex = j;
								break;
							}
						}
					
						if ( foundIndex < 0 )
							diffNode(null, &patternChild);
						else
						{
							diffNode(&node.children[foundIndex], &patternChild);
							visited[foundIndex] = true;
						}
					}
					level--;
					
					break;
				
				default: assert(0, "Unexpected element in pattern: "~pattern.name);
			}
		}
		
		private bool checkIdentifierMatch(const(T*) node, const(P*) pattern)
		{
			if ( !node || !pattern )
				return false;

			auto splitNode    = std.range.retro(std.algorithm.splitter(node.name,'.'));
			auto splitPattern = std.range.retro(std.algorithm.splitter(pattern.matches[0],'.'));
			
			while ( true )
			{
				auto nodePathElem = splitNode.front;
				auto patternPathElem = splitPattern.front;
				splitNode.popFront();
				splitPattern.popFront();
				
				if ( nodePathElem != patternPathElem )
					return false;

				if ( splitNode.empty || splitPattern.empty )
					break;
			}
			
			return true;
		}
		
		private void lineDiff(const(T*) node, const(P*) expected)
		{
			if ( !checkIdentifierMatch( node, expected ) )
			{
				differences++;
				doNodeAction(node, expected, true);
			}
			else
				doNodeAction(node, expected, false);
		}
		
		private void doNodeAction(const(T*) node, const(P*) expected, bool different)
		{
			// Part of me wants it to be more reusable and less stateful
			//   than this... but this is a simple way to do it for now. --Chad
			if ( doPrinting )
				printNodes(node, expected, different);
			else
				accumulateColumnWidth(node, expected, different);
		}
		
		private string getNodeTxt(const(T*) node)
		{
			string nodeTxt = "";
			if ( node )
				nodeTxt = node.name;
			return std.array.replicate("  ",level) ~ nodeTxt;
		}
		
		private string getPatternTxt(const(P*) pattern)
		{

			string patternTxt = "";
			if ( pattern )
				patternTxt = pattern.matches[0];
			return std.array.replicate("  ",level) ~ patternTxt;
		}

		private void printNodes(const(T*) node, const(P*) pattern, bool different)
		{
			auto nodeTxt    = getNodeTxt(node);
			auto patternTxt = getPatternTxt(node);

			char diffLine = '=';
			if ( !node && pattern )
				diffLine = '>';
			else if ( node && !pattern )
				diffLine = '<';
			else if ( different )
				diffLine = '|';
			
			printColumns( nodeTxt, diffLine, patternTxt );
		}

		private void printColumns( string ltext, char center, string rtext )
		{
			auto columnOneWidth = max1stColumnWidth;
			if ( columnOneWidth == 0 )
				columnOneWidth = 38;
		
			writefln("%-*s  %c  %s", columnOneWidth, ltext, center, rtext);
		}
		
		private void accumulateColumnWidth(const(T*) node, const(P*) pattern, bool different)
		{
			auto nodeTxt = getNodeTxt(node);
			max1stColumnWidth = std.algorithm.max(max1stColumnWidth, nodeTxt.length);
		}
	}
}

unittest
{
	GrammarTester!(TesterGrammar, "Root") tester;
	tester.soft = true;
	tester.verbose = true;
	tester.assertSimilar(`
		FOO
		{
			Bar->baz
		}
		`,`
		/* FOO */   Root->Node
		/* Bar */       ->UnorderedBranch->Node
		/* baz */           ->OrderedBranch->Node
		`);
		
	tester.assertSimilar(`
		FOO
		{
			x->/y->/z
			u
			v
			Bar->baz
		}
		`,`
		/* FOO */   Root->Node
		/*     */       ->UnorderedBranch
		/*     */       {
		/* x   */           Node
		/*     */           {
		/*  y  */               OrderedBranch->Node
		/*   z */               OrderedBranch->Node
		/*     */           }
		/* u   */           Node
		/* v   */           Node
		/* Bar */           Node->OrderedBranch
		/* baz */               ->Node
		/*     */       }
		`);
	
	tester.assertSimilar(`
		FOO
		{
			x->/y->/z
			u
			v
			Bar->baz
		}
		`,`
		/* FOO */   Root->Node
		/*     */       ->UnorderedBranch
		/*     */       {
		/* x   */           Node->OrderedBranch
		/*     */           { // There should actually be two ordered branches.
		/*   y */               Node
		/*bug:z*/               Node
		/*bug:z*/               Node
		/*     */           }
		/*     */       }
		`);
	
	tester.dumpAnyErrors();
}