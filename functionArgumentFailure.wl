(* ::Package:: *)

(* ::Chapter:: *)
(*Begin*)


BeginPackage["functionArgumentFailure`"];

  (* usages for public functions *)
  resetFailurelist::usage = "reset the global failurelist";
  functionArgumentFailure::usage = "When argument patterns are not matched an Failure object is produced";
  setFailure::usage = "generates a Failure object and appends it to the failurelist";



(* ::Chapter:: *)
(*Private*)


  Begin["`Private`"];


(* ::Subsection:: *)
(*functionArgumentFailure*)


  functionArgumentFailure[function_Symbol, args___] :=
      Module[{length, expectednumberofnormalarguments, likelyreason = "unknown", argumentslist,  patterntests,
        failedpos = Missing[], failedargument = Missing[], argumentnamelist, failure, messageid},

        argumentslist = {args};

        patterntests = findPatternTests[function];
        argumentnamelist = Flatten[StringTrim[#, (WhitespaceCharacter | "_")] & /@
            StringCases[StringReplace[patterntests, {"(" -> "", ")" -> ""}], __ ~~ "_"]];

        length = Length@argumentslist;

        expectednumberofnormalarguments = Length@patterntests;

        If[expectednumberofnormalarguments == length,

          failedpos = Position[checkFunctionArgumentpattern /@ Thread[{argumentslist, patterntests}], False];

          likelyreason = "argument(s) which failed the test: " <> Riffle[Extract[argumentnamelist, failedpos], ", "] ;
          failedargument = Short[#, 3] & /@ Extract[argumentslist, failedpos];
          messageid = 1;

          ,

          If[expectednumberofnormalarguments < length,
            likelyreason = "too many arguments";

            failedargument = argumentslist[[expectednumberofnormalarguments + 1 ;; -1]];
            failedpos = Range[expectednumberofnormalarguments + 1, length];
            messageid = 2;
            ,
            likelyreason = "not enough arguments";
            messageid = 3;

          ]

        ];

        failure = setFailure[function, <|
          "reason of failure" -> likelyreason,
          "number of given arguments" -> length,
          "expected number of arguments" -> expectednumberofnormalarguments,
          "Heads of given arguments" -> Head /@ argumentslist,
          "Perfomed tests" -> patterntests,
          "Position of failed argument(s)" -> failedpos,
          "List of failed argument" -> failedargument|> ];

        Which[messageid == 1,
          Message[functionArgumentFailure::wrongargs, function],
          messageid == 2,
          Message[functionArgumentFailure::tomanyargs, function],
          messageid == 3,
          Message[functionArgumentFailure::tolittleargs, function];
        ];

        Return[failure]

      ];

  functionArgumentFailure::wrongargs = "in `1` :wrong arguments";
  functionArgumentFailure::tomanyargs = "in `1` :too many arguments";
  functionArgumentFailure::tolittleargs = "in `1` :not enough  arguments";


(* ::Subsection:: *)
(*setFailure*)


  (* Define the Failuure Object and add it to the failurelist*)
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


(* ::Subsection:: *)
(*findPatternTests*)


(* Returns the Pattern tests of the "function" as string used.*)
  findPatternTests[function_Symbol] := Module[{patternsstringlist, test, test2},

        test = ToString[Definition[function]];
        test2 = First@StringSplit[First[StringSplit[test, "\n"]], ":="];
        patternsstringlist =
            StringSplit[StringDrop[StringDrop[First[StringCases[test2, "[" ~~ Longest[__] ~~ "]"]], {1}], {-1}], ","];
        patternsstringlistwithoutoptions = DeleteCases[patternsstringlist,
                s_String /; StringMatchQ[s, ___ ~~ "OptionsPattern[]" ~~ ___]];
        Return[patternsstringlistwithoutoptions]
      ];


(* ::Subsection:: *)
(*checkFunctionArgumentpattern*)


  (* Applying and check the pattern tests found with findPatternTests. *)
  checkFunctionArgumentpattern[{argument_, test_String}] := MatchQ[argument, ToExpression[test]]



(* ::Chapter:: *)
(*End of package\:201a*)


End[];

EndPackage[ ];
