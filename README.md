# functionArgumentFailure

A Mathematica package to handle errors when calling a function with wrong arguments.

## Authors

Thomas Gölles, [https://thomasgoelles.com](https://thomasgoelles.com)

## Intro


The Wolfram Language is a bit special in the way it handles functions which have been called with wrong arguments. Most built-in functions will give a message and return unevaluated when the arguments are not correct.

I was looking for a better solution which gives me more feedback and makes debugging easier. Therefore, I created the functionArgumentFailure package. Whenever a function is called with wrong arguments a FailureObject is returned and added to the global "failurelist". Each FailureObject informs where the error occurred and the reason for it.

In version 10 the FailureObject has been introduced, and the Function FailureQ has been added in version 10.2.
The FailureObject is not often used in the internal function but I think it is ideal for error handling.

## Installation

In Mathematica go to File > Install ... and install the package.

## Basic Usage



	Needs["functionArgumentFailure`"]

	ClearAll[test];
	
	test[a_?StringQ] := 1;
	test[args___] := functionArgumentFailure[test, args];
	

So whenever the pattern of test["some string"] is matched the functionArgumentFailure is called with 2 arguments. First the name of the failed function and second arguments arguments. 
Make sure to use 3 _ for the pattern test, as this allows zero or more arguments.

Here an example then the function test has been called with not enough arguments

![Failure Example](/Users/Thomas/Documents/Wolfram Mathematica/functionArgumentFailure/failure.png)

For further information see Examples.nb and the unit tests in tests/Test.nb
 
##More Info:

[FailureQ documentation](http://reference.wolfram.com/language/ref/FailureQ.html)

[FailureObject documentation](http://reference.wolfram.com/language/ref/Failure.html)

[An interresting discussion on stackexchange](https://mathematica.stackexchange.com/questions/29321/what-are-the-best-practices-most-common-idiomatic-ways-to-report-errors-in-m#6563886)