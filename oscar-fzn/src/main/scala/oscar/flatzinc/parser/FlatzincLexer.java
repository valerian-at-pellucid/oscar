// Generated from /home/janho/workspace/oscar/oscar-fzn/src/main/scala/oscar/flatzinc/parser/Flatzinc.g by ANTLR 4.4

package oscar.flatzinc.parser;
import oscar.flatzinc.parser.intermediatemodel.*;
import oscar.flatzinc.model.Annotation;
import oscar.flatzinc.model.Domain;
import oscar.flatzinc.model.DomainSet;
import oscar.flatzinc.model.DomainRange;
import oscar.flatzinc.ParsingException;
import java.util.Set;
import java.util.HashSet;


import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class FlatzincLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.4", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__24=1, T__23=2, T__22=3, T__21=4, T__20=5, T__19=6, T__18=7, T__17=8, 
		T__16=9, T__15=10, T__14=11, T__13=12, T__12=13, T__11=14, T__10=15, T__9=16, 
		T__8=17, T__7=18, T__6=19, T__5=20, T__4=21, T__3=22, T__2=23, T__1=24, 
		T__0=25, Boolconst=26, PREDANNID=27, VARPARID=28, Floatconst=29, INT=30, 
		STRING=31, WS=32;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"'\\u0000'", "'\\u0001'", "'\\u0002'", "'\\u0003'", "'\\u0004'", "'\\u0005'", 
		"'\\u0006'", "'\\u0007'", "'\b'", "'\t'", "'\n'", "'\\u000B'", "'\f'", 
		"'\r'", "'\\u000E'", "'\\u000F'", "'\\u0010'", "'\\u0011'", "'\\u0012'", 
		"'\\u0013'", "'\\u0014'", "'\\u0015'", "'\\u0016'", "'\\u0017'", "'\\u0018'", 
		"'\\u0019'", "'\\u001A'", "'\\u001B'", "'\\u001C'", "'\\u001D'", "'\\u001E'", 
		"'\\u001F'", "' '"
	};
	public static final String[] ruleNames = {
		"T__24", "T__23", "T__22", "T__21", "T__20", "T__19", "T__18", "T__17", 
		"T__16", "T__15", "T__14", "T__13", "T__12", "T__11", "T__10", "T__9", 
		"T__8", "T__7", "T__6", "T__5", "T__4", "T__3", "T__2", "T__1", "T__0", 
		"Boolconst", "PREDANNID", "VARPARID", "Floatconst", "INT", "NUM", "STRING", 
		"WS"
	};


	public FlatzincLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Flatzinc.g"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 32: WS_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WS_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip(); break;
		}
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\"\u00f3\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3"+
		"\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n"+
		"\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\17"+
		"\3\17\3\17\3\17\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\21"+
		"\3\21\3\21\3\21\3\21\3\22\3\22\3\23\3\23\3\23\3\24\3\24\3\24\3\24\3\24"+
		"\3\24\3\24\3\24\3\24\3\24\3\24\3\25\3\25\3\26\3\26\3\27\3\27\3\27\3\27"+
		"\3\27\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3\30\3\30\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\32\3\32\3\32\3\33\3\33\3\33\3\33\3\33\3\33"+
		"\3\33\3\33\3\33\5\33\u00be\n\33\3\34\3\34\7\34\u00c2\n\34\f\34\16\34\u00c5"+
		"\13\34\3\35\6\35\u00c8\n\35\r\35\16\35\u00c9\3\35\3\35\3\36\3\36\3\36"+
		"\3\36\3\36\5\36\u00d3\n\36\3\36\3\36\3\36\3\36\5\36\u00d9\n\36\3\37\5"+
		"\37\u00dc\n\37\3\37\3\37\3 \6 \u00e1\n \r \16 \u00e2\3!\3!\6!\u00e7\n"+
		"!\r!\16!\u00e8\3!\3!\3\"\3\"\3\"\5\"\u00f0\n\"\3\"\3\"\2\2#\3\3\5\4\7"+
		"\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22"+
		"#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65\34\67\359\36;\37= ?\2A!C"+
		"\"\3\2\b\4\2C\\c|\6\2\62;C\\aac|\4\2GGgg\4\2--//\3\2$$\4\2\13\f\"\"\u00fa"+
		"\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2"+
		"\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2"+
		"\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2"+
		"\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2"+
		"\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3"+
		"\2\2\2\2=\3\2\2\2\2A\3\2\2\2\2C\3\2\2\2\3E\3\2\2\2\5G\3\2\2\2\7P\3\2\2"+
		"\2\tS\3\2\2\2\13U\3\2\2\2\rW\3\2\2\2\17Y\3\2\2\2\21[\3\2\2\2\23_\3\2\2"+
		"\2\25e\3\2\2\2\27i\3\2\2\2\31m\3\2\2\2\33o\3\2\2\2\35q\3\2\2\2\37w\3\2"+
		"\2\2!\u0081\3\2\2\2#\u0086\3\2\2\2%\u0088\3\2\2\2\'\u008b\3\2\2\2)\u0096"+
		"\3\2\2\2+\u0098\3\2\2\2-\u009a\3\2\2\2/\u00a3\3\2\2\2\61\u00a9\3\2\2\2"+
		"\63\u00b1\3\2\2\2\65\u00bd\3\2\2\2\67\u00bf\3\2\2\29\u00c7\3\2\2\2;\u00d8"+
		"\3\2\2\2=\u00db\3\2\2\2?\u00e0\3\2\2\2A\u00e4\3\2\2\2C\u00ef\3\2\2\2E"+
		"F\7_\2\2F\4\3\2\2\2GH\7o\2\2HI\7k\2\2IJ\7p\2\2JK\7k\2\2KL\7o\2\2LM\7k"+
		"\2\2MN\7|\2\2NO\7g\2\2O\6\3\2\2\2PQ\7q\2\2QR\7h\2\2R\b\3\2\2\2ST\7.\2"+
		"\2T\n\3\2\2\2UV\7]\2\2V\f\3\2\2\2WX\7*\2\2X\16\3\2\2\2YZ\7<\2\2Z\20\3"+
		"\2\2\2[\\\7k\2\2\\]\7p\2\2]^\7v\2\2^\22\3\2\2\2_`\7c\2\2`a\7t\2\2ab\7"+
		"t\2\2bc\7c\2\2cd\7{\2\2d\24\3\2\2\2ef\7x\2\2fg\7c\2\2gh\7t\2\2h\26\3\2"+
		"\2\2ij\7u\2\2jk\7g\2\2kl\7v\2\2l\30\3\2\2\2mn\7}\2\2n\32\3\2\2\2op\7\177"+
		"\2\2p\34\3\2\2\2qr\7h\2\2rs\7n\2\2st\7q\2\2tu\7c\2\2uv\7v\2\2v\36\3\2"+
		"\2\2wx\7r\2\2xy\7t\2\2yz\7g\2\2z{\7f\2\2{|\7k\2\2|}\7e\2\2}~\7c\2\2~\177"+
		"\7v\2\2\177\u0080\7g\2\2\u0080 \3\2\2\2\u0081\u0082\7d\2\2\u0082\u0083"+
		"\7q\2\2\u0083\u0084\7q\2\2\u0084\u0085\7n\2\2\u0085\"\3\2\2\2\u0086\u0087"+
		"\7+\2\2\u0087$\3\2\2\2\u0088\u0089\7<\2\2\u0089\u008a\7<\2\2\u008a&\3"+
		"\2\2\2\u008b\u008c\7e\2\2\u008c\u008d\7q\2\2\u008d\u008e\7p\2\2\u008e"+
		"\u008f\7u\2\2\u008f\u0090\7v\2\2\u0090\u0091\7t\2\2\u0091\u0092\7c\2\2"+
		"\u0092\u0093\7k\2\2\u0093\u0094\7p\2\2\u0094\u0095\7v\2\2\u0095(\3\2\2"+
		"\2\u0096\u0097\7?\2\2\u0097*\3\2\2\2\u0098\u0099\7=\2\2\u0099,\3\2\2\2"+
		"\u009a\u009b\7o\2\2\u009b\u009c\7c\2\2\u009c\u009d\7z\2\2\u009d\u009e"+
		"\7k\2\2\u009e\u009f\7o\2\2\u009f\u00a0\7k\2\2\u00a0\u00a1\7|\2\2\u00a1"+
		"\u00a2\7g\2\2\u00a2.\3\2\2\2\u00a3\u00a4\7u\2\2\u00a4\u00a5\7q\2\2\u00a5"+
		"\u00a6\7n\2\2\u00a6\u00a7\7x\2\2\u00a7\u00a8\7g\2\2\u00a8\60\3\2\2\2\u00a9"+
		"\u00aa\7u\2\2\u00aa\u00ab\7c\2\2\u00ab\u00ac\7v\2\2\u00ac\u00ad\7k\2\2"+
		"\u00ad\u00ae\7u\2\2\u00ae\u00af\7h\2\2\u00af\u00b0\7{\2\2\u00b0\62\3\2"+
		"\2\2\u00b1\u00b2\7\60\2\2\u00b2\u00b3\7\60\2\2\u00b3\64\3\2\2\2\u00b4"+
		"\u00b5\7v\2\2\u00b5\u00b6\7t\2\2\u00b6\u00b7\7w\2\2\u00b7\u00be\7g\2\2"+
		"\u00b8\u00b9\7h\2\2\u00b9\u00ba\7c\2\2\u00ba\u00bb\7n\2\2\u00bb\u00bc"+
		"\7u\2\2\u00bc\u00be\7g\2\2\u00bd\u00b4\3\2\2\2\u00bd\u00b8\3\2\2\2\u00be"+
		"\66\3\2\2\2\u00bf\u00c3\t\2\2\2\u00c0\u00c2\t\3\2\2\u00c1\u00c0\3\2\2"+
		"\2\u00c2\u00c5\3\2\2\2\u00c3\u00c1\3\2\2\2\u00c3\u00c4\3\2\2\2\u00c48"+
		"\3\2\2\2\u00c5\u00c3\3\2\2\2\u00c6\u00c8\7a\2\2\u00c7\u00c6\3\2\2\2\u00c8"+
		"\u00c9\3\2\2\2\u00c9\u00c7\3\2\2\2\u00c9\u00ca\3\2\2\2\u00ca\u00cb\3\2"+
		"\2\2\u00cb\u00cc\5\67\34\2\u00cc:\3\2\2\2\u00cd\u00ce\5=\37\2\u00ce\u00cf"+
		"\7\60\2\2\u00cf\u00d2\5? \2\u00d0\u00d1\t\4\2\2\u00d1\u00d3\5=\37\2\u00d2"+
		"\u00d0\3\2\2\2\u00d2\u00d3\3\2\2\2\u00d3\u00d9\3\2\2\2\u00d4\u00d5\5="+
		"\37\2\u00d5\u00d6\t\4\2\2\u00d6\u00d7\5=\37\2\u00d7\u00d9\3\2\2\2\u00d8"+
		"\u00cd\3\2\2\2\u00d8\u00d4\3\2\2\2\u00d9<\3\2\2\2\u00da\u00dc\t\5\2\2"+
		"\u00db\u00da\3\2\2\2\u00db\u00dc\3\2\2\2\u00dc\u00dd\3\2\2\2\u00dd\u00de"+
		"\5? \2\u00de>\3\2\2\2\u00df\u00e1\4\62;\2\u00e0\u00df\3\2\2\2\u00e1\u00e2"+
		"\3\2\2\2\u00e2\u00e0\3\2\2\2\u00e2\u00e3\3\2\2\2\u00e3@\3\2\2\2\u00e4"+
		"\u00e6\7$\2\2\u00e5\u00e7\n\6\2\2\u00e6\u00e5\3\2\2\2\u00e7\u00e8\3\2"+
		"\2\2\u00e8\u00e6\3\2\2\2\u00e8\u00e9\3\2\2\2\u00e9\u00ea\3\2\2\2\u00ea"+
		"\u00eb\7$\2\2\u00ebB\3\2\2\2\u00ec\u00f0\t\7\2\2\u00ed\u00ee\7\17\2\2"+
		"\u00ee\u00f0\7\f\2\2\u00ef\u00ec\3\2\2\2\u00ef\u00ed\3\2\2\2\u00f0\u00f1"+
		"\3\2\2\2\u00f1\u00f2\b\"\2\2\u00f2D\3\2\2\2\f\2\u00bd\u00c3\u00c9\u00d2"+
		"\u00d8\u00db\u00e2\u00e8\u00ef\3\3\"\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}