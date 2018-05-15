BeginTestSection["Testing Of functionArgumentFailure"]

BeginTestSection["init"]

EndTestSection[]

BeginTestSection["Tests"]

BeginTestSection["resetFailurelist"]

VerificationTest[(* 1 *)
	resetFailurelist[]
	,
	Null	
]

VerificationTest[(* 2 *)
	CompoundExpression[resetFailurelist[], failurelist]
	,
	List[]	
]

EndTestSection[]

BeginTestSection["functionArgumentFailure"]

VerificationTest[(* 3 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], test["a"]]
	,
	1	
]

VerificationTest[(* 4 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], FailureQ[test[1]]]
	,
	True
	,
	{functionArgumentFailure::wrongargs}
]

VerificationTest[(* 5 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], FailureQ[test["a", "b"]]]
	,
	True
	,
	{functionArgumentFailure::toomanyargs}
]

VerificationTest[(* 6 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], FailureQ[test[]]]
	,
	True
	,
	{functionArgumentFailure::toofewargs}
]

VerificationTest[(* 7 *)
	CompoundExpression[resetFailurelist[], ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], test[], Length[failurelist]]
	,
	1
	,
	{functionArgumentFailure::toofewargs}
]

VerificationTest[(* 8 *)
	CompoundExpression[resetFailurelist[], ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], test[], test["a", "b"], Length[failurelist]]
	,
	2
	,
	{functionArgumentFailure::toofewargs, functionArgumentFailure::toomanyargs}
]

VerificationTest[(* 9 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[type, Blank[]], Function[MemberQ[List["m1", "m2", "m3", "m4"], Slot[1]]]]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], test["m1"]]
	,
	1	
]

VerificationTest[(* 10 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[type, Blank[]], Function[MemberQ[List["m1", "m2", "m3", "m4"], Slot[1]]]], PatternTest[Pattern[t, Blank[]], Function[Equal[Head[Slot[1]], Dataset]]], PatternTest[Pattern[xx, Blank[]], Function[Equal[Head[Slot[1]], Association]]], Pattern[yy, Blank[String]]], 1], SetDelayed[test[Pattern[args, BlankNullSequence[]]], functionArgumentFailure[test, args]], test["m1", Dataset["t"], Association[""], ""]]
	,
	1	
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`wrongArgumentTypes"]

VerificationTest[(* 11 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[Pattern[a, Blank[String]]], a], Head[functionArgumentFailure`Private`wrongArgumentTypes[List[1], functionArgumentFailure`Private`findPatternTests[test], test]]]
	,
	Association
	,
	{functionArgumentFailure::wrongargs}
]

VerificationTest[(* 12 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[Pattern[a, Blank[String]]], a], First[functionArgumentFailure`Private`wrongArgumentTypes[List[1], functionArgumentFailure`Private`findPatternTests[test], test]]]
	,
	"argument(s) which failed the test: a"
	,
	{functionArgumentFailure::wrongargs}
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`tooManyArguments"]

VerificationTest[(* 13 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[Pattern[a, Blank[String]]], a], Head[functionArgumentFailure`Private`tooManyArguments[List[1, 1, 1], functionArgumentFailure`Private`findPatternTests[test], test]]]
	,
	Association
	,
	{functionArgumentFailure::toomanyargs}
]

VerificationTest[(* 14 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[Pattern[a, Blank[String]]], a], Normal[functionArgumentFailure`Private`tooManyArguments[List["a", "b", "c"], functionArgumentFailure`Private`findPatternTests[test], test]]]
	,
	List[Rule["reason of failure", "too many arguments"], Rule["Position of failed argument(s)", List[2, 3]], Rule["List of failed argument(s)", List["b", "c"]]]
	,
	{functionArgumentFailure::toomanyargs}
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`tooFewArguments"]

VerificationTest[(* 15 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[Pattern[a, Blank[String]], Pattern[b, Blank[String]]], a], Head[functionArgumentFailure`Private`tooFewArguments[List["a"], functionArgumentFailure`Private`findPatternTests[test], test]]]
	,
	Association
	,
	{functionArgumentFailure::toofewargs}
]

VerificationTest[(* 16 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[Pattern[a, Blank[String]], Pattern[b, Blank[String]]], a], Normal[functionArgumentFailure`Private`tooFewArguments[List["a"], functionArgumentFailure`Private`findPatternTests[test], test]]]
	,
	List[Rule["reason of failure", "not enough arguments"], Rule["Position of failed argument(s)", Missing[]], Rule["List of failed argument(s)", Missing[]]]
	,
	{functionArgumentFailure::toofewargs}
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`genFailureInfo"]

VerificationTest[(* 17 *)
	Head[functionArgumentFailure`Private`genFailureInfo[List["a"], functionArgumentFailure`Private`findPatternTests[test], test]]
	,
	Association	
]

VerificationTest[(* 18 *)
	Normal[functionArgumentFailure`Private`genFailureInfo[List["a"], functionArgumentFailure`Private`findPatternTests[test], test]]
	,
	List[Rule["reason of failure", List["a"]], Rule["Position of failed argument(s)", List["a_String", " b_String"]], Rule["List of failed argument(s)", test]]	
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`getArgumentnamelist"]

VerificationTest[(* 19 *)
	functionArgumentFailure`Private`getArgumentnamelist[List["", " test", "()"]]
	,
	List[]	
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`getFailedPositionOfPatterntest"]

VerificationTest[(* 20 *)
	functionArgumentFailure`Private`getFailedPositionOfPatterntest[List[1], List["(a_)?StringQ"]]
	,
	List[List[1]]	
]

VerificationTest[(* 21 *)
	functionArgumentFailure`Private`getFailedPositionOfPatterntest[List["xx"], List["(a_)?StringQ"]]
	,
	List[]	
]

EndTestSection[]

BeginTestSection["setFailure"]

VerificationTest[(* 22 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], FailureQ[setFailure[test, Association[Rule["a", 1]]]]]
	,
	True	
]

VerificationTest[(* 23 *)
	CompoundExpression[resetFailurelist[], ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], setFailure[test, Association[Rule["a", 1]]], Length[failurelist]]
	,
	1	
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`findPatternTests"]

VerificationTest[(* 24 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ]], 1], functionArgumentFailure`Private`findPatternTests[test]]
	,
	List["(a_)?StringQ"]	
]

VerificationTest[(* 25 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ], Pattern[b, Blank[Integer]]], 1], functionArgumentFailure`Private`findPatternTests[test]]
	,
	List["(a_)?StringQ", " b_Integer"]	
]

VerificationTest[(* 26 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[PatternTest[Pattern[a, Blank[]], StringQ], Condition[Pattern[b, Blank[Integer]], Greater[b, 1]]], 1], Length[functionArgumentFailure`Private`findPatternTests[test]]]
	,
	2	
]

VerificationTest[(* 27 *)
	CompoundExpression[ClearAll[test], SetDelayed[test[Condition[PatternTest[Pattern[b, Blank[]], AssociationQ], Greater[Length[b], 2]]], 1], Length[functionArgumentFailure`Private`findPatternTests[test]]]
	,
	1	
]

VerificationTest[(* 28 *)
	CompoundExpression[ClearAll[test], SetDelayed[check1[Pattern[b, Blank[]]], Greater[Length[b], 2]], SetDelayed[test[Condition[PatternTest[Pattern[b, Blank[]], AssociationQ], check1[b]]], 1], Length[functionArgumentFailure`Private`findPatternTests[test]]]
	,
	1	
]

VerificationTest[(* 29 *)
	CompoundExpression[ClearAll[test], SetDelayed[check1[Pattern[b, Blank[]]], Greater[Length[b], 2]], SetDelayed[test[Condition[PatternTest[Pattern[b, Blank[]], AssociationQ], check1[b]]], 1], functionArgumentFailure`Private`findPatternTests[test]]
	,
	List["(b_)?AssociationQ /; check1[b]"]	
]

VerificationTest[(* 30 *)
	CompoundExpression[ClearAll[test], Set[Options[test], List[Rule["test1", 1]]], SetDelayed[test[Pattern[b, Blank[Integer]], Pattern[opts, OptionsPattern[]]], 1], Length[functionArgumentFailure`Private`findPatternTests[test]]]
	,
	1	
]

EndTestSection[]

BeginTestSection["functionArgumentFailure`Private`checkFunctionArgumentpattern"]

VerificationTest[(* 31 *)
	functionArgumentFailure`Private`checkFunctionArgumentpattern[List[1, "a_Integer"]]
	,
	True	
]

VerificationTest[(* 32 *)
	functionArgumentFailure`Private`checkFunctionArgumentpattern[List["a", "a_Integer"]]
	,
	False	
]

EndTestSection[]

EndTestSection[]

EndTestSection[]
