# verybignumbers

This project is for practice in Erlang/OTP.
Target: precision calculating.

Task File Structure:

R=Precision:non_neg_integer() A=(List of output variables with a comma)
e. g. R=45 A=(Z, X, _ERT)
Each next string is either condition, or expression
No blank strings
Minimal expression is Var1= Var2/Number operator Var3/Number2
Typical expression isVar1= Expr1/Var2/Number1 operator Expr2/Var3/Number2 with brackets
Bracket can consist minimal expression, no less
You can use unary minus like: -A+-16/(-ZZ*FR_Q)+22.4
Variable names only is in camel case and underscore
Conditions is either like WHILE or DO...WHILE
Cond operators is: == >= =< <>
You can nest it
WHILE is
?Var1 cond Var2
Expressions/conditions
?
DO...WHILE is
??
Expressions/conditions
??Var1 cond Var2

Result is file w/ext filepathfilename.aout or, if you working over inet, connect again for asking abt task state.

