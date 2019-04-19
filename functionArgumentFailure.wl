(* ::Package:: *)

(* ::Chapter:: *)
(*Begin*)


BeginPackage["functionArgumentFailure`"];

  (* usages for public functions *)
  functionArgumentFailure::usage = "When argument patterns are not matched an Failure object is produced";
  resetFailurelist::usage = "reset the global failurelist";
  setFailure::usage = "generates a Failure object and appends it to the failurelist";


(* ::Chapter:: *)
(*Private*)


  Begin["`Private`"];


(* ::Subsection:: *)
(*functionArgumentFailure*)


  functionArgumentFailure[function_Symbol, args___] :=
      Module[{givennumberofarguments, expectednumberofarguments, argumentslist,  patterntests,
          failure, failureinfo, failurebasicinfo},

        argumentslist = {args};
        patterntests = findPatternTests[function];

        givennumberofarguments = Length[argumentslist];
        expectednumberofarguments = Length[patterntests];

        If[expectednumberofarguments == givennumberofarguments,
          failureinfo = wrongArgumentTypes[argumentslist, patterntests, function];
          ,

          If[expectednumberofarguments < givennumberofarguments,
            failureinfo = tooManyArguments[argumentslist, patterntests, function];
            ,
            failureinfo = tooFewArguments[argumentslist, patterntests, function];
          ]
        ];

        failurebasicinfo = <|
          "number of given arguments" -> givennumberofarguments,
          "expected number of arguments" -> expectednumberofarguments,
          "Heads of given arguments" -> Head /@ argumentslist,
          "Perfomed tests" -> patterntests|>;

        failure = setFailure[function, Prepend[failureinfo, failurebasicinfo ] ];
        Return[failure]

      ];
  functionArgumentFailure::wrongargs = "in `1` :wrong arguments";
  functionArgumentFailure::toomanyargs = "in `1` :too many arguments";
  functionArgumentFailure::toofewargs = "in `1` :not enough  arguments";


(* ::Subsection:: *)
(*wrongArgumentTypes*)

  wrongArgumentTypes[argumentslist_List, patterntests_List, function_Symbol] :=
      Module[{failedpos, likelyreason, failedargument, messageid, argumentnamelist},

        argumentnamelist = getArgumentnamelist[patterntests];

        failedpos = getFailedPositionOfPatterntest[argumentslist, patterntests];
        likelyreason = "argument(s) which failed the test: " <> Riffle[Extract[argumentnamelist, failedpos], ", "] ;
        failedargument = Short[#, 3] & /@ Extract[argumentslist, failedpos];
        Message[functionArgumentFailure::wrongargs, function];

        Return[genFailureInfo[likelyreason, failedpos, failedargument]]

      ]


(* ::Subsection:: *)
(*tooManyArguments*)

  tooManyArguments[argumentslist_List, patterntests_List, function_Symbol] := Module[{failedpos, likelyreason,
    failedargument, givennumberofarguments, expectednumberofarguments},

    givennumberofarguments = Length[argumentslist];
    expectednumberofarguments = Length[patterntests];

    failedpos = Range[expectednumberofarguments + 1, givennumberofarguments];
    likelyreason = "too many arguments";
    failedargument = argumentslist[[expectednumberofarguments + 1 ;; -1]];
    Message[functionArgumentFailure::toomanyargs, function];

    Return[genFailureInfo[likelyreason, failedpos, failedargument]]
  ]


(* ::Subsection:: *)
(*tooFewArguments*)

  tooFewArguments[argumentslist_List, patterntests_List, function_Symbol] := Module[{failedpos, likelyreason,
    failedargument},

    failedpos = Missing[];
    likelyreason = "not enough arguments";
    failedargument = Missing[];
    Message[functionArgumentFailure::toofewargs, function];

    Return[genFailureInfo[likelyreason, failedpos, failedargument]]
  ];


(* ::Subsection:: *)
(*genFailureInfo*)

  genFailureInfo[likelyreason_, failedpos_, failedargument_ ] := <|"reason of failure"-> likelyreason,
    "Position of failed argument(s)" -> failedpos, "List of failed argument(s)" -> failedargument|>


(* ::Subsection:: *)
(*getArgumentnamelist*)

  getArgumentnamelist[patterntests_List] := Flatten[StringTrim[#, (WhitespaceCharacter | "_")] & /@
      StringCases[StringReplace[patterntests, {"(" -> "", ")" -> ""}], __ ~~ "_"]]


(* ::Subsection:: *)
(*getFailedPositionOfPatterntest*)

  getFailedPositionOfPatterntest[argumentslist_List, patterntests_List]:= Position[checkFunctionArgumentpattern /@
      Thread[{argumentslist, patterntests}], False];

(* ::Subsection:: *)
(*checkFunctionArgumentpattern*)

  (* Applying and check the pattern tests found with findPatternTests. *)
  checkFunctionArgumentpattern[{argument_, test_String}] := MatchQ[argument, ToExpression[test]]


(* ::Subsection:: *)
(*findPatternTests*)


  (* Returns the Pattern tests of the "function" as string used.*)
  findPatternTests[function_Symbol] := Module[{patternsstringlist, functiondefinition, functiondefinitionlist,
    patternsstringlistwithoutoptions, s},

    functiondefinition = ToString[Definition[function]];
    functiondefinitionlist = First@StringSplit[First[StringSplit[functiondefinition, "\n"]], ":="];

    patternsstringlist =
        StringSplit[StringDrop[StringDrop[First[StringCases[functiondefinitionlist, "[" ~~ Longest[__] ~~ "]"]],
          {1}], {-1}], ","];

    patternsstringlistwithoutoptions = DeleteCases[patternsstringlist,
      s_String /; StringMatchQ[s, ___ ~~ "OptionsPattern[]" ~~ ___]];

    Return[patternsstringlistwithoutoptions]
  ];


(* ::Subsection:: *)
(*setFailure*)


  (* Define the Failure Object and add it to the failurelist*)
  setFailure[function_Symbol, failures_?AssociationQ] := Module[{failure},

    failure = Failure[function, failures];

    If[ListQ@ Global`failurelist == False, resetFailurelist[]];
    AppendTo[Global`failurelist, failure];

    Return[failure]
  ];
  setFailure[args___]:= exception[setFailure, args];


(* ::Subsection:: *)
(*resetFailurlist*)

  resetFailurelist[] := Module[{}, Global`failurelist = {};];



(* ::Chapter:: *)
(*End of package*)


End[];

EndPackage[ ];
